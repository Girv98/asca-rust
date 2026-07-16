#![allow(clippy::too_many_arguments)]

// NOTE(girv): lots of duplication here atm (and starting to look like spaghetti), focusing on getting things done before optimising

use std ::{
    cell::RefCell, 
    collections::HashMap,
    num::NonZeroU8
};


mod context;
mod deletion;
mod input;
mod insertion;
mod metathesis;
mod substitution;

use crate  :: {
    error  :: RuleRuntimeError, 
    rule   :: { Alpha, AlphaMod, BinMod, EnvItem, ModKind, Modifiers, ParseElement, ParseItem, PlaceMod, Position, RuleType, SpecMod, SupraSegs, UnderlineStruct }, 
    word   :: { FeatKind, NodeKind, Phrase, SegPos, Segment, StressKind, Syllable, Tone, Word },
};

type WrdPos = usize;    // the index of the word in the phrase, for WordBound, the word before the boundary
type SylPos = usize;    // the index of the syllable in the word.syllables array
type BndPos = usize;    // the index of the syllable in the word.syllables array that has this boundary as it's start
type SetInd = usize;    // the index of the matched set choice
type ErrPos = Position; // the matching token position, used for errors

#[derive(Debug, Clone)]
pub(crate) enum MatchElement {
    #[allow(unused)]
    Set(Vec<MatchElement>, SetInd, ErrPos), 
    Segment(SegPos, ErrPos),
    LongSegment(SegPos, ErrPos),
    Syllable(WrdPos, SylPos, ErrPos),
    SyllBound(WrdPos, BndPos, ErrPos),
    WordBound(WrdPos, ErrPos),
}


#[derive(Debug)]
enum Payload {
    Segment(Segment, Option<SupraSegs>),
    Syllable(Syllable),
}

type OldLen = NonZeroU8;
type NewLen = NonZeroU8;
type SegLen = NonZeroU8;


#[derive(Debug)]
#[allow(unused)]
enum ActionKind {
    DeleteSyllable,
    DeleteSegment(SegLen),
    InsertSegment(SegLen, Segment, Option<SupraSegs>, ErrPos),
    InsertSyllable(Syllable),
    InsertWord(Word), // NOTE: This is will never happen, see self.apply() for more
    ReplaceSegment((OldLen, NewLen), Payload, ErrPos),
    ReplaceSyllable(Syllable),
    ModifySyllable(SupraSegs, ErrPos),
    PassBoundary,
    InsertBoundary,
    DeleteBoundary,
    InsertWordBound,
    DeleteWordBound,
}


#[derive(Debug)]
struct Action {
    kind: ActionKind,
    pos: SegPos,
}


#[derive(Debug, Clone)]
pub(crate) enum RefKind {
    Segment(Segment),
    Syllable(Syllable)
}

#[derive(Debug)]
pub(crate) struct SubRule {
    pub(crate) input      : Vec<ParseItem>,
    pub(crate) output     : Vec<ParseItem>,
    pub(crate) context    : Option<EnvItem>,
    pub(crate) except     : Option<EnvItem>,
    pub(crate) rule_type  : RuleType,
    pub(crate) references : RefCell<HashMap<usize, RefKind>>,
    pub(crate) alphas     : RefCell<HashMap<char, Alpha>>,
    pub(crate) is_reversed: bool,
    pub(crate) inp_x_bound: bool,
    pub(crate) env_x_bound: bool,
}

impl SubRule {

    fn has_cross_bound(&self) -> bool { self.inp_x_bound || self.env_x_bound }

    pub(crate) fn apply(&self, phrase: Phrase) -> Result<Phrase, RuleRuntimeError> {
        if phrase.is_empty() || (phrase.len() == 1 && phrase[0].syllables.is_empty()) { return Ok(phrase) }

        // '##' will not match if there's only one word, 
        if self.has_cross_bound() && phrase.len() < 2 { return Ok(phrase) }

        let res = self.apply_phrase(if self.is_reversed { phrase.reversed() } else { phrase })?;

        if self.is_reversed { Ok(res.reversed()) } else { Ok(res) }
    }

    fn apply_phrase(&self, phrase: Phrase) -> Result<Phrase, RuleRuntimeError> {
        if self.rule_type == RuleType::Insertion {
            self.transform(&phrase, &[], &mut None)
        } else {
            self.apply_other(&phrase)
        }
    }

    fn transform(&self, phrase: &Phrase, input: &[MatchElement], next_pos: &mut Option<SegPos>) -> Result<Phrase, RuleRuntimeError> {
        match self.rule_type {
            RuleType::Substitution => self.substitution(phrase, input, next_pos),
            RuleType::Metathesis   |
            RuleType::MetaOrdered  => self.metathesis(phrase, input, next_pos),
            RuleType::Deletion     => self.deletion(phrase, input, next_pos),
            RuleType::Insertion    => self.insertion(phrase),
        }
    }

    fn env_is_hangable(&self) -> bool {
        // for insertion, contexts currenly can only be be <=1
        let conts = self.get_contexts();
        // for (env_bef, env_cen, env_aft) in self.get_contexts() {}

        if conts.is_empty() { return true }

        let (env_bef, env_cen, env_aft) = conts[0];

        if env_cen.is_some() { return false }

        let mut left_hangable  = true;
        for b in env_bef {
            if !b.is_opt_and_nullable() {
                left_hangable = false;
                break;
            }
        }

        let mut right_hangable  = true;
        for a in env_aft {
            if !a.is_opt_and_nullable() {
                right_hangable = false;
                break;
            }
        }

        left_hangable && right_hangable
    }

    fn set_start(&self, res: &[MatchElement], phrase: &Phrase) -> (SegPos, bool) {
        match res {
            [MatchElement::Set(els, ..), ..] => self.set_start(els, phrase),
            [MatchElement::Segment(sp, _) | MatchElement::LongSegment(sp, _), ..] => (*sp, true),
            [MatchElement::Syllable(wp, s, _) | MatchElement::SyllBound(wp, s, _), ..] => {
                (SegPos::new(*wp,* s, 0), true)
            }
            [MatchElement::WordBound(wp, _), ..] => {
                let mut pos = SegPos::new(wp+1, 0, 0);
                pos.word_decrement(phrase);
                (pos, false)
            }
            [] => unreachable!("res is not empty")
        }
    }

    fn set_end(&self, res: &[MatchElement], phrase: &Phrase) -> (SegPos, bool) {
        match res {
            [.., MatchElement::Set(els, ..)] => self.set_end(els, phrase),
           &[.., MatchElement::Segment(mut sp, _) | MatchElement::LongSegment(mut sp, _)] => {
                // So that long vowels work
                let mut seg_len = phrase.seg_length_at(sp);
                // sp.seg_index += seg_len - 1;
                while seg_len > 1 {
                    sp.increment(phrase);
                    seg_len -= 1;
                }
                (sp, true)
            },
            [.., MatchElement::Syllable(wp, s, _)] => (SegPos::new(*wp, *s, phrase[*wp].syllables[*s].segments.len()-1), true),
            [.., MatchElement::SyllBound(wp, s, _)] => (SegPos::new(*wp, *s, 0), false),
            [.., MatchElement::WordBound(wp, _)] => (SegPos::new(wp+1, 0, 0), false),
            [] => unreachable!("res is not empty")
        }
    }

    fn apply_other(&self, phrase: &Phrase) -> Result<Phrase, RuleRuntimeError> {
        let mut res_phrase = phrase.clone();
        let mut cur_index = SegPos::new(0, 0, 0);
        // TODO(girv): `$ > *` or any broad deletion rule without context/exception should give a warning to the user
        
        // For infinite loop checking
        let phrase_len_max = Self::phrase_len_max(&res_phrase);

        loop {
            if res_phrase.seg_count() > phrase_len_max {
                return Err(RuleRuntimeError::InfiniteLoop(self.input.first().unwrap().position, res_phrase.clone(), res_phrase.clone()))
            }

            self.alphas.borrow_mut().clear();
            self.references.borrow_mut().clear();
            let (res, mut next_index) = self.input_match_at(&res_phrase, cur_index, 0)?;
            if res.is_empty() {
                // No match
                if cur_index.word_index < res_phrase.len() - 1 {
                    cur_index.word_increment(&res_phrase);
                    continue
                } else {
                    break
                }
            }

            let (start, inc_start) = self.set_start(&res,  &res_phrase);
            let (end, inc_end) = self.set_end(&res, &res_phrase);

            if !self.match_contexts_and_exceptions(&res_phrase, &res, start, end, inc_start, inc_end)? {
                if next_index.is_some() { 
                    let last = cur_index;
                    cur_index.increment(&res_phrase);
                    if last == cur_index && cur_index.word_index < res_phrase.len() - 1 {
                        cur_index.word_increment(&res_phrase);
                    }
                    continue;
                }
                // end of word
                if cur_index.word_index < res_phrase.len() - 1 {
                    cur_index.word_increment(&res_phrase);
                    continue;
                } else {
                    break;
                }
            }

            res_phrase = self.transform(&res_phrase,  &res, &mut next_index)?;
            
            if let Some(ci) = next_index { 
                cur_index = ci;
            } else {
                // End of Word
                if cur_index.word_index < res_phrase.len() - 1 {
                    cur_index.word_increment(&res_phrase);
                    continue
                } else {
                    break
                }
            }
        }
        Ok(res_phrase)
    }

    fn phrase_len_max(phrase: &Phrase) -> usize {
        // Arbitrary limits as a sign of potential infinite loop
        let phrase_len = phrase.seg_count();
        if phrase_len > 20 {
            phrase_len * 4 // purely so the error output is not too big
        } else {
            phrase_len * 10
        }
    }

    fn gen_syll_from_struct(&self, items: &[ParseItem], stress: &Option<SpecMod>, tone: &Option<Tone>, refr: &Option<usize>, err_pos: Position, is_inserting: bool) -> Result<Syllable, RuleRuntimeError> {
        let mut syll = Syllable::new();
        syll.apply_syll_mods(&self.alphas, &SupraSegs { stress: *stress, length: None, tone: *tone }, err_pos)?;

        for item in items {
            match &item.kind {
                ParseElement::Ellipsis | ParseElement::OptEllipsis => return Err(if is_inserting {RuleRuntimeError::InsertionEllipsis(item.position)} else {RuleRuntimeError::SubstitutionEllipsis(item.position)}),
                ParseElement::Matrix(..)   => return Err(if is_inserting {RuleRuntimeError::InsertionMatrix(item.position)} else {RuleRuntimeError::SubstitutionMatrix(item.position)}),
                ParseElement::Optional(..) => return Err(if is_inserting {RuleRuntimeError::InsertionOpt(item.position)} else {RuleRuntimeError::SubstitutionOpt(item.position)}),
                ParseElement::Set(_)       => return Err(if is_inserting {RuleRuntimeError::InsertionSet(item.position)} else {RuleRuntimeError::SubstitutionSet(item.position)}),
                ParseElement::Negation(_)  => return Err(RuleRuntimeError::BadNegationOutput(item.position)),
                &ParseElement::Ipa(mut segment, ref modifiers) => {
                    let mut len = 1;
                    if let Some(mods) = modifiers {
                        segment.apply_seg_mods(&self.alphas, mods.nodes, mods.feats, item.position, false)?;
                        len = match mods.suprs.length {
                            Some(lm) => match lm {
                                SpecMod::Second(v) => if v.as_bool(&self.alphas, item.position)? { 3 } else { 1 },
                                SpecMod::First(l) => if l.as_bool(&self.alphas, item.position)? { 2 } else { 1 },
                                SpecMod::Both(l, v) => match (l.as_bool(&self.alphas, item.position)?, v.as_bool(&self.alphas, item.position)?) {
                                    (true, true)   => 3,
                                    (true, false)  => 2,
                                    (false, false) => 1,
                                    (false, true)  => Err(RuleRuntimeError::OverlongPosLongNeg(item.position))?,
                                },
                                SpecMod::Joined(j) => match j {
                                    ModKind::Binary(_) => return Err(RuleRuntimeError::GroupSuprIsBinary("length", err_pos)),
                                    ModKind::Alpha(am) => match am {
                                        AlphaMod::InvAlpha(_) => return Err(RuleRuntimeError::GroupSuprIsInverted("length", err_pos)),
                                        AlphaMod::Alpha(ch) => {
                                            if let Some(alph) = self.alphas.borrow().get(&ch) {
                                                let Some(group) = alph.as_grouped() else { return Err(RuleRuntimeError::AlphaIsNotSuprGroup(err_pos)) };
                                                match group {
                                                    (true, true)   => 3,
                                                    (true, false)  => 2,
                                                    (false, false) => 1,
                                                    (false, true)  => Err(RuleRuntimeError::OverlongPosLongNeg(item.position))?,
                                                }
                                            } else {
                                                return Err(RuleRuntimeError::AlphaUnknown(err_pos))
                                            }

                                        },
                                    },
                                },
                            }
                            None => 1,
                        };
                        // TODO: Ignore syll suprs?
                    }
                    for _ in 0..len {
                        syll.segments.push_back(segment);
                    }
                
                }
                ParseElement::Reference(num, modifiers) => {
                    if let Some(refr) = self.references.borrow().get(&num.value) {
                        match refr {
                            RefKind::Syllable(_) => return Err(RuleRuntimeError::SyllRefInsideStruct(item.position)),
                            &RefKind::Segment(mut segment) => {
                                let mut len = 1;
                                if let Some(mods) = modifiers {
                                    segment.apply_seg_mods(&self.alphas, mods.nodes, mods.feats, item.position, false)?;
                                    len = match mods.suprs.length {
                                        Some(lm) => match lm {
                                            SpecMod::Second(v) => if v.as_bool(&self.alphas, item.position)? { 3 } else { 1 },
                                            SpecMod::First(l) => if l.as_bool(&self.alphas, item.position)? { 2 } else { 1 },
                                            SpecMod::Both(l, v) => match (l.as_bool(&self.alphas, item.position)?, v.as_bool(&self.alphas, item.position)?) {
                                                (true, true)   => 3,
                                                (true, false)  => 2,
                                                (false, false) => 1,
                                                (false, true)  => Err(RuleRuntimeError::OverlongPosLongNeg(item.position))?,
                                            },
                                            SpecMod::Joined(j) => match j {
                                                ModKind::Binary(_) => return Err(RuleRuntimeError::GroupSuprIsBinary("length", err_pos)),
                                                ModKind::Alpha(am) => match am {
                                                    AlphaMod::InvAlpha(_) => return Err(RuleRuntimeError::GroupSuprIsInverted("length", err_pos)),
                                                    AlphaMod::Alpha(ch) => {
                                                        if let Some(alph) = self.alphas.borrow().get(&ch) {
                                                            let Some(group) = alph.as_grouped() else { return Err(RuleRuntimeError::AlphaIsNotSuprGroup(err_pos)) };
                                                            match group {
                                                                (true, true)   => 3,
                                                                (true, false)  => 2,
                                                                (false, false) => 1,
                                                                (false, true)  => Err(RuleRuntimeError::OverlongPosLongNeg(item.position))?,
                                                            }
                                                        } else {
                                                            return Err(RuleRuntimeError::AlphaUnknown(err_pos))
                                                        }

                                                    },
                                                },
                                            },
                                        }
                                        None => 1,
                                    };
                                    // TODO: Ignore syll suprs?
                                }
                                for _ in 0..len {
                                    syll.segments.push_back(segment);
                                }
                            },
                        }
                    }
                }
                
                ParseElement::SyllBound | ParseElement::WordBound | ParseElement::ExtlBound => continue, // TODO: maybe error if not at periphery

                ParseElement::MetaOrdered  | ParseElement::Metathesis | ParseElement::EmptySet | 
                ParseElement::Syllable(..) | ParseElement::Structure(..) => unreachable!()
            }
        }

        if let Some(r) = refr {
            self.references.borrow_mut().insert(*r, RefKind::Syllable(syll.clone()));
        }

        Ok(syll)
    }

    fn concat_tone(prev: Tone, aft: Tone) -> Tone {
        if prev == aft { return prev }
        if prev == 0 || aft == 0 { return prev | aft }

        let new_tone = prev as u64 * 10u64.pow(aft.ilog10()+1) + aft as u64;

        // FIXME: may as well not be doing the above if we're gonna convert to a collection anyway
        let mut nums = new_tone.to_string()
            .as_str().bytes()
            .map(|b| b - b'0')
            .collect::<Vec<_>>();
        // i.e. 5225 > 525
        nums.dedup();

        if nums.len() > 4 {
            // Somehow meld the two tones together
            // Guaranteed to have no zeros and no adjacent duplicates
            // Between 5 and 8 digits

            let mut arr = vec![0,0,0];
            arr[0] = nums[0];
            arr[2] = nums[nums.len()-1];

            arr[1] = if nums.len() == 5 && nums[1] == nums[3] { // 1 2541 2
                nums[2]
            } else {
                // Find the mean of the middle values
                let mut acc = 0;
                for i in nums.iter().take(nums.len()-1).skip(1) {
                    acc+=i;
                }
                acc / (nums.len()-2) as u8
            };

            nums = arr;
        }

        nums.iter().fold(0, |acc, elem| acc * 10 + *elem as Tone)
    }

    fn match_prim_stress(&self, syll: &Syllable, val: &ModKind) -> Result<bool, RuleRuntimeError> {
        match val {
            ModKind::Binary(bm) => match bm {
                BinMod::Negative => if syll.stress != StressKind::Unstressed { return Ok(false) },
                BinMod::Positive => if syll.stress == StressKind::Unstressed { return Ok(false) },
            },
            ModKind::Alpha(am) => match am {
                AlphaMod::Alpha(ch) => {
                    if let Some(alph) = self.alphas.borrow().get(ch) {
                        let pos = alph.as_binary();
                        match syll.stress {
                            StressKind::Unstressed => if  pos { return Ok(false) },
                            _                      => if !pos { return Ok(false) },
                        }
                    } else {
                        let stress = syll.stress != StressKind::Unstressed;
                        self.alphas.borrow_mut().insert(*ch, Alpha::Supra(stress));
                    }
                },
                AlphaMod::InvAlpha(ch) => {
                    if let Some(alph) = self.alphas.borrow().get(ch) {
                        let pos = alph.as_binary();
                        match syll.stress {
                            StressKind::Unstressed => if !pos { return Ok(false) },
                            _                      => if  pos { return Ok(false) },
                        }
                    } else {
                        let stress = syll.stress == StressKind::Unstressed;
                        self.alphas.borrow_mut().insert(*ch, Alpha::Supra(stress));
                    }
                },
            },
        }

        Ok(true)
    }

    fn match_sec_stress(&self, syll: &Syllable, val: &ModKind) -> Result<bool, RuleRuntimeError> {
        match val {
            ModKind::Binary(bm) => match bm {
                BinMod::Negative => if syll.stress == StressKind::Secondary { return Ok(false) },
                BinMod::Positive => if syll.stress != StressKind::Secondary { return Ok(false) },
            },
            ModKind::Alpha(am) => match am {
                AlphaMod::Alpha(ch) => {
                    if let Some(alph) = self.alphas.borrow().get(ch) {
                        let pos = alph.as_binary();
                        match syll.stress {
                            StressKind::Secondary  => if !pos { return Ok(false) },
                            _                      => if  pos { return Ok(false) },
                        }
                    } else {
                        let stress = syll.stress == StressKind::Secondary;
                        self.alphas.borrow_mut().insert(*ch, Alpha::Supra(stress));
                    }
                },
                AlphaMod::InvAlpha(ch) => {
                    if let Some(alph) = self.alphas.borrow().get(ch) {
                        let pos = alph.as_binary();
                        match syll.stress {
                            StressKind::Secondary  => if  pos { return Ok(false) },
                            _                      => if !pos { return Ok(false) },
                        }
                    } else {
                        let stress = syll.stress != StressKind::Secondary;
                        self.alphas.borrow_mut().insert(*ch, Alpha::Supra(stress));
                    }
                },
            },
        }

        Ok(true)
    }

    fn match_joined_stress(&self, syll: &Syllable, val: &ModKind, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        match val {
            ModKind::Binary(_) => return Err(RuleRuntimeError::GroupSuprIsBinary("stress", err_pos)),
            ModKind::Alpha(am) => match am {
                AlphaMod::InvAlpha(_) => return Err(RuleRuntimeError::GroupSuprIsInverted("stress", err_pos)),
                AlphaMod::Alpha(ch) => {
                    if let Some(alph) = self.alphas.borrow().get(ch) {
                        let Some(group) = alph.as_grouped() else { return Err(RuleRuntimeError::AlphaIsNotSuprGroup(err_pos)) };
                        match group {
                            // Long, Overlong
                            (true, true)   => if syll.stress != StressKind::Secondary  { return Ok(false) },
                            (true, false)  => if syll.stress == StressKind::Unstressed { return Ok(false) },
                            (false, false) => if syll.stress != StressKind::Unstressed { return Ok(false) },
                            (false, true)  => return Err(RuleRuntimeError::SecStrPosStrNeg(err_pos)),
                        }
                    } else {
                        self.alphas.borrow_mut().insert(*ch, Alpha::Grouped(syll.stress == StressKind::Primary, syll.stress == StressKind::Secondary));
                    }
                },
            }
        }

        Ok(true)

    }

    fn match_stress(&self, stress: &Option<SpecMod>, syll: &Syllable, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        let Some(str_mods) = stress else { return Ok(true) };

        match str_mods {
            SpecMod::First(val) => Ok(self.match_prim_stress(syll, val)?),
            SpecMod::Second(val) => Ok(self.match_sec_stress(syll, val)?),
            SpecMod::Both(prim, sec) => Ok(self.match_prim_stress(syll, prim)? && self.match_sec_stress(syll, sec)?),
            SpecMod::Joined(val) => Ok(self.match_joined_stress(syll, val, err_pos)?),
        }
    }

    fn match_tone(&self, tone: &Tone, syll: &Syllable) -> bool { *tone == syll.tone }

    fn match_ipa_with_modifiers(&self, seg: &Segment, mods: &Modifiers, phrase: &Phrase, pos: &SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> {   
        let mut joined_mods = seg.as_modifiers();
        
        for (i, n) in mods.nodes.iter().enumerate() {
            if n.is_some() { joined_mods.nodes[i] = *n }
        }
        for (i, f) in mods.feats.iter().enumerate() {
            if f.is_some() { joined_mods.feats[i] = *f }
        }
        joined_mods.suprs = mods.suprs;

        self.match_modifiers(&joined_mods, phrase, pos, err_pos)
    }

    fn match_modifiers(&self, mods: &Modifiers, phrase: &Phrase, pos: &SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        let seg = phrase.get_seg_at(*pos).expect("Segment Position should be within bounds");
        
        for (i, m) in mods.feats.iter().enumerate() {
            if !self.match_feat_mod(m, i, seg)? {
                return Ok(false);
            }
        }
        for (i, m) in mods.nodes.iter().enumerate() {
            if !self.match_node_mod(m, i, seg, err_pos)? {
                return Ok(false);
            }
        }
        self.match_supr_mod_seg(phrase, &mods.suprs, pos, err_pos)
    }

    fn match_supr_mod_seg(&self, phrase: &Phrase, mods: &SupraSegs, pos: &SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> {

        let syll = &phrase[pos.word_index].syllables[pos.syll_index];

        if !self.match_stress(&mods.stress, syll, err_pos)? { return Ok(false) }
        if !self.match_seg_length(phrase, &mods.length, pos, err_pos)? { return Ok(false) }

        if let Some(t) = mods.tone.as_ref() {
            return Ok(self.match_tone(t, syll))
        }

        Ok(true)
    }

    fn match_long(&self, phrase: &Phrase, val: &ModKind, pos: &SegPos) -> bool {
        let seg_length = phrase.seg_length_at(*pos);
        
        match val {
            ModKind::Binary(bm) => match bm {
                BinMod::Positive => if seg_length < 2 { return false },
                BinMod::Negative => if seg_length > 1 { return false },
            },
            ModKind::Alpha(am) => match am {
                AlphaMod::Alpha(ch) => {
                    if let Some(alph) = self.alphas.borrow().get(ch) {
                        match alph.as_binary() {
                            true  => if seg_length < 2 { return false },
                            false => if seg_length > 1 { return false },
                        }
                    } else {
                        self.alphas.borrow_mut().insert(*ch, Alpha::Supra(seg_length > 1));
                    }
                },
                AlphaMod::InvAlpha(ch) => {
                    if let Some(alph) = self.alphas.borrow().get(ch) {
                        match !alph.as_binary() {
                            true  => if seg_length < 2 { return false },
                            false => if seg_length > 1 { return false }
                        }
                    } else {
                        self.alphas.borrow_mut().insert(*ch, Alpha::Supra(seg_length <= 1));
                    }
                },
            },
        }

        true
    }

    fn match_overlong(&self, phrase: &Phrase, val: &ModKind, pos: &SegPos) -> bool {
        let seg_length = phrase.seg_length_at(*pos);
        match val {
            ModKind::Binary(bm) => match bm {
                BinMod::Positive => if seg_length < 3 { return false },
                BinMod::Negative => if seg_length > 2 { return false },
            },
            ModKind::Alpha(am) => match am {
                AlphaMod::Alpha(ch) => {
                    if let Some(alph) = self.alphas.borrow().get(ch) {
                        match alph.as_binary() {
                            true  => if seg_length < 3 { return false },
                            false => if seg_length > 2 { return false },
                        }
                    } else {
                        self.alphas.borrow_mut().insert(*ch, Alpha::Supra(seg_length > 2));
                    }
                },
                AlphaMod::InvAlpha(ch) => {
                    if let Some(alph) = self.alphas.borrow().get(ch) {
                        match !alph.as_binary() {
                            true  => if seg_length < 3 { return false },
                            false => if seg_length > 2 { return false },
                        }
                    } else {
                        self.alphas.borrow_mut().insert(*ch, Alpha::Supra(seg_length <= 2));
                    }
                },
            },
        }
        true
    }

    fn match_joined_length(&self, phrase: &Phrase, val: &ModKind, pos: &SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        let seg_length = phrase.seg_length_at(*pos);

        match val {
            ModKind::Binary(_) => return Err(RuleRuntimeError::GroupSuprIsBinary("length", err_pos)),
            ModKind::Alpha(am) => match am {
                AlphaMod::InvAlpha(_) => return Err(RuleRuntimeError::GroupSuprIsInverted("length", err_pos)),
                AlphaMod::Alpha(ch) => {
                    if let Some(alph) = self.alphas.borrow().get(ch) {
                        let Some(group) = alph.as_grouped() else { return Err(RuleRuntimeError::AlphaIsNotSuprGroup(err_pos)) };
                        match group {
                            // Long, Overlong
                            (true, true)   => if seg_length < 3 { return Ok(false) },
                            (true, false)  => if seg_length < 2 { return Ok(false) },
                            (false, false) => if seg_length > 1 { return Ok(false) },
                            (false, true)  => return Err(RuleRuntimeError::OverlongPosLongNeg(err_pos)),
                        }
                    } else {
                        self.alphas.borrow_mut().insert(*ch, Alpha::Grouped(seg_length > 1, seg_length > 2));
                    }
                },
            },
        }

        Ok(true)
    }

    fn match_seg_length(&self, phrase: &Phrase, length: &Option<SpecMod>, pos: &SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        let Some(len_mods) = length else { return Ok(true) };

        match len_mods {
            SpecMod::First(val) => Ok(self.match_long(phrase, val, pos)),
            SpecMod::Second(val) => Ok(self.match_overlong(phrase, val, pos)),
            SpecMod::Both(lng, vlg) => Ok(self.match_long(phrase, lng, pos) && self.match_overlong(phrase, vlg, pos)),
            SpecMod::Joined(val) => Ok(self.match_joined_length(phrase, val, pos, err_pos)?),
        }
    }

    fn match_node_mod(&self, md:&Option<ModKind>, node_index: usize, seg: Segment, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        if let Some(kind) = md {
            let node = NodeKind::from_usize(node_index);
            return self.match_node(seg, node, kind, err_pos)
        }
        Ok(true)
    }

    fn match_feat_mod(&self, md: &Option<ModKind>, feat_index: usize, seg: Segment) -> Result<bool, RuleRuntimeError> {
        if let Some(kind) = md { 
            let (node, mask) = FeatKind::from_usize(feat_index).as_node_mask();
            return self.match_seg_kind(kind, seg, node, mask)
        }
        Ok(true)
    }

    fn match_node(&self, seg: Segment, node: NodeKind, val: &ModKind, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        match val {
            ModKind::Binary(bt) => if node == NodeKind::Place {
                let x = seg.is_place_some();
                match bt {
                    BinMod::Positive => Ok(x),
                    BinMod::Negative => Ok(!x),
                }
            } else {
                match bt {
                    BinMod::Positive => Ok(seg.get_node(node).is_some()),
                    BinMod::Negative => Ok(seg.get_node(node).is_none()),
                }
            },
            ModKind::Alpha(am) => match am {
                AlphaMod::Alpha(ch) => {
                    if let Some(alph) = self.alphas.borrow().get(ch) {
                        if let Some((n, m)) = alph.as_node() {
                            if n == node {
                                return Ok(seg.node_match(n, m))
                            } else {
                                return Err(RuleRuntimeError::AlphaIsNotSameNode(err_pos))
                            }
                        } else if let Some(place) = alph.as_place() {
                            return Ok(
                                seg.node_match(NodeKind::Labial, place.lab)  &&
                                seg.node_match(NodeKind::Coronal, place.cor) &&
                                seg.node_match(NodeKind::Dorsal, place.dor)  &&
                                seg.node_match(NodeKind::Pharyngeal, place.phr)
                            )
                        } else {
                            return Err(RuleRuntimeError::AlphaIsNotNode(err_pos))
                        }
                    }
                    if node == NodeKind::Place {
                        let place = PlaceMod::new(
                            seg.get_node(NodeKind::Labial),
                            seg.get_node(NodeKind::Coronal),
                            seg.get_node(NodeKind::Dorsal),
                            seg.get_node(NodeKind::Pharyngeal),
                        );
                        self.alphas.borrow_mut().insert(*ch, Alpha::Place(place)); 
                    } else {
                        self.alphas.borrow_mut().insert(*ch, Alpha::Node(node, seg.get_node(node))); 
                    }
                    Ok(true)
                },
                AlphaMod::InvAlpha(inv_ch) => {
                    if let Some(alph) = self.alphas.borrow().get(inv_ch) {
                        if let Some((n, m)) = alph.as_node() {
                            if n == node {
                                Ok(!seg.node_match(n, m))
                            } else {
                                Err(RuleRuntimeError::AlphaIsNotNode(err_pos))
                            }
                        } else if let Some(place) = alph.as_place() {
                            Ok(
                                !seg.node_match(NodeKind::Labial, place.lab)  ||
                                !seg.node_match(NodeKind::Coronal, place.cor) ||
                                !seg.node_match(NodeKind::Dorsal, place.dor)  ||
                                !seg.node_match(NodeKind::Pharyngeal, place.phr)
                            )
                        } else {
                            Err(RuleRuntimeError::AlphaIsNotNode(err_pos))
                        }
                    } else {
                        Err(RuleRuntimeError::AlphaUnknownInv(err_pos))
                    }
                },
            },
        }
    }

    fn match_seg_kind(&self, kind: &ModKind, seg: Segment, node: NodeKind, mask: u8) -> Result<bool, RuleRuntimeError> {
        match kind {
            ModKind::Binary(bt) => match bt {
                BinMod::Negative => Ok(seg.feat_match(node, mask, false)),
                BinMod::Positive => Ok(seg.feat_match(node, mask, true)),
            },
            ModKind::Alpha(am) => match am {
                AlphaMod::Alpha(ch) => {
                    if let Some(alph) = self.alphas.borrow().get(ch) {
                        Ok(seg.feat_match(node, mask, alph.as_binary()))
                    } else if let Some(f) = seg.get_feat(node, mask) {
                        self.alphas.borrow_mut().insert(*ch, Alpha::Feature(f != 0)); 
                        Ok(true)
                    } else {
                        // Maybe err?
                        Ok(false)
                    }
                },
                AlphaMod::InvAlpha(inv_ch) => {
                    if let Some(alph) = self.alphas.borrow().get(inv_ch) {
                        Ok(seg.feat_match(node, mask, !alph.as_binary()))
                    } else if let Some(f) = seg.get_feat(node, mask) {
                        self.alphas.borrow_mut().insert(*inv_ch, Alpha::Feature(f == 0));
                        Ok(true)
                    } else {
                        // Maybe err?
                        Ok(false)
                    } 
                },
            },
        }
    }
 
    fn syll_inc(phrase: &Phrase, pos: &mut SegPos) {
        pos.syll_index += 1; pos.seg_index = 0;
        if !phrase.in_bounds(*pos) {
            pos.word_increment(phrase);
        }
    }

    fn matrix_increment(&self, phrase: &Phrase, pos: &mut SegPos) {
        // the way we implement `long` vowels means we need to do this
        if !phrase.in_bounds(*pos) { return }
        let seg_length = phrase.seg_length_at(*pos);
        for _ in 1..seg_length {
            pos.increment(phrase);
        }
    }

    fn apply_actions(&self, phrase: &Phrase, actions: &[Action]) -> Result<(Phrase, isize), RuleRuntimeError> {
        let mut res_phrase = phrase.clone();
        if actions.is_empty() { return Ok((res_phrase, 0)) }

        let mut phrase_len_change: isize = 0;
        let mut word_len_change = vec![0; phrase.len()];

        // NOTE: because we are going reverse, the first of multiple syllable tone or stress changes on the same syllable will be final
        // This may not be a problem, but is the opposite of what happened previously
        for (i, action) in actions.iter().enumerate().rev() {
            match &action.kind {
                ActionKind::ReplaceSegment((old_length, new_length), payload, err_pos) => match payload {
                    Payload::Segment(segment, mods) => {
                        let syll = res_phrase[action.pos.word_index].syllables.get_mut(action.pos.syll_index).unwrap();
                        for _ in 0..old_length.get()-1 {
                            syll.segments.remove(action.pos.seg_index+1);
                        }

                        if syll.segments.is_empty() {
                            for _ in 0..new_length.get() {
                                syll.segments.insert(action.pos.seg_index, *segment);
                            }
                        } else {
                            syll.segments[action.pos.seg_index] = *segment;
                            for _ in 0..new_length.get()-1 {
                                syll.segments.insert(action.pos.seg_index, *segment);
                            }
                        }

                        if let Some(m) = mods {
                            syll.apply_syll_mods(&self.alphas, m, *err_pos)?;
                        }
                    },
                    Payload::Syllable(insert_syll) => {
                        let old_syll = res_phrase[action.pos.word_index].syllables.get_mut(action.pos.syll_index).unwrap();

                        if old_syll.segments.len() == old_length.get() as usize {
                            *old_syll = insert_syll.clone();
                            continue;
                        }

                        // Else, split old_syll in two at segment pos and insert payload syll between them

                        for _ in 0..old_length.get() {
                            old_syll.segments.remove(action.pos.seg_index);
                        }

                        let mut new_syll = Syllable::new();
                        new_syll.stress = old_syll.stress;
                        new_syll.tone = old_syll.tone;

                        while old_syll.segments.len() > action.pos.seg_index {
                            new_syll.segments.push_front(old_syll.segments.pop_back().unwrap());
                        }

                        let mut adjustment = 0;
                        if old_syll.segments.is_empty() {
                            *old_syll = insert_syll.clone();
                        } else {
                            res_phrase[action.pos.word_index].syllables.insert(action.pos.syll_index+1, insert_syll.clone());
                            adjustment = 1;
                            word_len_change[action.pos.word_index] += 1
                        }
                        if !new_syll.segments.is_empty() {
                            res_phrase[action.pos.word_index].syllables.insert(action.pos.syll_index+1+adjustment, new_syll);
                        }

                    },
                },
                ActionKind::ReplaceSyllable(syllable) => {
                    res_phrase[action.pos.word_index].syllables[action.pos.syll_index] = syllable.clone();
                },
                ActionKind::ModifySyllable(mods, err_pos) => {
                    res_phrase[action.pos.word_index].syllables[action.pos.syll_index].apply_syll_mods(&self.alphas, mods, *err_pos)?;
                },
                ActionKind::PassBoundary => {},
                ActionKind::InsertSegment(seg_len, segment, mods, err_pos) => {
                    if let Some(syll) = res_phrase[action.pos.word_index].syllables.get_mut(action.pos.syll_index) {
                        for _ in 0..seg_len.get() {
                            syll.segments.insert(action.pos.seg_index, *segment);
                        }
                        if let Some(m) = mods {
                            syll.apply_syll_mods(&self.alphas, m, *err_pos)?;
                        }
                    } else {
                        let syll = res_phrase[action.pos.word_index].syllables.back_mut().expect("Not empty");
                        for _ in 0..seg_len.get() {
                            syll.segments.push_back(*segment);
                        }
                        if let Some(m) = mods {
                            syll.apply_syll_mods(&self.alphas, m, *err_pos)?;
                        }
                    }
                },
                ActionKind::DeleteSegment(seg_len) => {
                    // if only segment in word, err
                    if res_phrase[action.pos.word_index].syllables.len() <= 1 && res_phrase[action.pos.word_index].syllables[action.pos.syll_index].segments.len() <= seg_len.get() as usize {
                        return Err(RuleRuntimeError::DeletionOnlySeg)
                    }
                    // remove segment
                    let syll = res_phrase[action.pos.word_index].syllables.get_mut(action.pos.syll_index).unwrap();
                    for _ in 0..seg_len.get() {
                        syll.segments.remove(action.pos.seg_index);
                    }
                    // if that was the only segment in that syllable, remove the syllable
                    if syll.segments.is_empty() {
                        res_phrase[action.pos.word_index].syllables.remove(action.pos.syll_index);
                        word_len_change[action.pos.word_index] -= 1;
                    }
                },
                ActionKind::InsertSyllable(insert_syll) => {
                    if !res_phrase.in_bounds(action.pos) {
                        // Push to end
                        if res_phrase[action.pos.word_index].syllables.len() > action.pos.syll_index+1 {
                            res_phrase[action.pos.word_index].syllables.insert(action.pos.syll_index+1, insert_syll.clone());
                            word_len_change[action.pos.word_index] += 1;
                        } else {
                            res_phrase[action.pos.word_index].syllables.push_back(insert_syll.clone());
                            word_len_change[action.pos.word_index] += 1;
                        }
                        continue;
                    }
                    if action.pos.at_syll_start() {
                        // Insert before
                        res_phrase[action.pos.word_index].syllables.insert(action.pos.syll_index, insert_syll.clone());
                        word_len_change[action.pos.word_index] += 1;
                        continue;
                    }

                    let old_syll = res_phrase[action.pos.word_index].syllables.get_mut(action.pos.syll_index).unwrap();

                    let mut new_syll = Syllable::new();
                    new_syll.stress = old_syll.stress;
                    new_syll.tone = old_syll.tone;

                    while old_syll.segments.len() > action.pos.seg_index {
                        new_syll.segments.push_front(old_syll.segments.pop_back().unwrap());
                    }

                    let mut adjustment = 0;
                    if old_syll.segments.is_empty() {
                        *old_syll = insert_syll.clone();
                    } else {
                        res_phrase[action.pos.word_index].syllables.insert(action.pos.syll_index+1, insert_syll.clone());
                        adjustment = 1;
                        word_len_change[action.pos.word_index] += 1
                    }
                    if !new_syll.segments.is_empty() {
                        res_phrase[action.pos.word_index].syllables.insert(action.pos.syll_index+1+adjustment, new_syll);
                    }
                },
                ActionKind::DeleteSyllable => {
                    if ((!self.has_cross_bound()) || res_phrase.len() == 1) && res_phrase[action.pos.word_index].syllables.len() <= 1 {
                        return Err(RuleRuntimeError::DeletionOnlySyll)
                    }
                    res_phrase[action.pos.word_index].syllables.remove(action.pos.syll_index);
                    word_len_change[action.pos.word_index] -= 1;
                },
                ActionKind::InsertBoundary => {
                    // Break syllable into two at position
                    let mut new_syll = Syllable::new();

                    let Some(syll) = res_phrase[action.pos.word_index].syllables.get_mut(action.pos.syll_index) else { continue };

                    while !syll.segments.is_empty() && syll.segments.len() > action.pos.seg_index {
                        new_syll.segments.push_front(syll.segments.pop_back().unwrap());
                    }
                    res_phrase[action.pos.word_index].syllables.insert(action.pos.syll_index+1, new_syll);

                    word_len_change[action.pos.word_index] += 1;
                },
                ActionKind::DeleteBoundary => {
                    if action.pos.syll_index == 0 || action.pos.syll_index >= res_phrase[action.pos.word_index].syllables.len() {
                        continue; // can't delete a word boundary
                    }

                    if let Some(last_action) = actions.get(i+1) && matches!(last_action.kind, ActionKind::InsertSyllable(_)) {
                        continue; // So that we are not immediately removing the leftside boundary of the syll we just inserted
                    }

                    let mut syll_segs = res_phrase[action.pos.word_index].syllables[action.pos.syll_index].segments.clone();
                    res_phrase[action.pos.word_index].syllables[action.pos.syll_index-1].segments.append(&mut syll_segs);

                    let last_stress = res_phrase[action.pos.word_index].syllables[action.pos.syll_index-1].stress;
                    let this_stress = res_phrase[action.pos.word_index].syllables[action.pos.syll_index].stress;
                    
                    res_phrase[action.pos.word_index].syllables[action.pos.syll_index-1].stress = match (last_stress, this_stress) {
                        (StressKind::Primary, _) | (_, StressKind::Primary) => StressKind::Primary,
                        (StressKind::Secondary,  StressKind::Unstressed) | 
                        (StressKind::Secondary,  StressKind::Secondary)  | 
                        (StressKind::Unstressed, StressKind::Secondary)  => StressKind::Secondary,
                        (StressKind::Unstressed, StressKind::Unstressed) => StressKind::Unstressed,
                    };

                    res_phrase[action.pos.word_index].syllables[action.pos.syll_index-1].tone = Self::concat_tone(
                        res_phrase[action.pos.word_index].syllables[action.pos.syll_index-1].tone, 
                        res_phrase[action.pos.word_index].syllables[action.pos.syll_index].tone
                    );
                    res_phrase[action.pos.word_index].syllables.remove(action.pos.syll_index);

                    word_len_change[action.pos.word_index] -= 1;
                },
                // NOTE: This is will never happen because of line 120 in self.apply()
                ActionKind::InsertWord(word) => {
                    if !res_phrase.in_bounds(action.pos) {
                        if res_phrase.len() > action.pos.word_index + 1 {
                            res_phrase.insert(action.pos.word_index + 1, word.clone());
                            word_len_change.push(0);
                        } else {
                            res_phrase.push(word.clone());
                            word_len_change.push(0);
                        }

                        continue;
                    }

                    unimplemented!()
                }
                ActionKind::InsertWordBound => {
                    let mut new_word = Word::new("").unwrap();

                    if action.pos.seg_index == 0 && action.pos.syll_index > 0 {
                        // push all syllables > action.pos.syll_index
                        let Some(word) = res_phrase.get_mut(action.pos.word_index) else { unreachable!() };
                        while word.syllables.len() > action.pos.syll_index {
                            new_word.syllables.push_front(word.syllables.pop_back().unwrap());
                        }
                    } else {
                        // push all syllables > action.pos.syll_index+1
                        // then split syllable and push that
                        let Some(word) = res_phrase.get_mut(action.pos.word_index) else { unreachable!() };
                        while word.syllables.len() > action.pos.syll_index + 1 {
                            new_word.syllables.push_front(word.syllables.pop_back().unwrap());
                        }

                        let mut new_syll = Syllable::new();
                        let syll = res_phrase[action.pos.word_index].syllables.get_mut(action.pos.syll_index).unwrap();
                        while !syll.segments.is_empty() && syll.segments.len() > action.pos.seg_index {
                            new_syll.segments.push_front(syll.segments.pop_back().unwrap());
                        }

                        new_word.syllables.push_front(new_syll);
                    }

                    let new_word = Word { syllables: new_word.syllables.into_iter().filter(|s| !s.segments.is_empty()).collect()};

                    if !new_word.syllables.is_empty() {
                        res_phrase.insert(action.pos.word_index+1, new_word);
                        word_len_change.insert(action.pos.word_index+1, 0);
                        phrase_len_change += 1;
                    }

                    continue;
                }
                ActionKind::DeleteWordBound => {
                    if res_phrase.len() <= action.pos.word_index {
                        continue;
                    }

                    let x = res_phrase[action.pos.word_index+1].syllables.clone();
                    res_phrase[action.pos.word_index].syllables.extend(x);

                    res_phrase.remove(action.pos.word_index+1);
                    word_len_change.remove(action.pos.word_index+1);
                    phrase_len_change -= 1;
                    continue
                }
            }
        }

        for word in res_phrase.iter_mut() { word.syllables.retain(|s| !s.segments.is_empty()); }

        let last_syll_change = if actions.is_empty() {
            0
        } else {
            word_len_change[actions.last().expect("actions is not empty").pos.word_index.saturating_add_signed(phrase_len_change)]
        };
        Ok((res_phrase, last_syll_change))
    }

    // TODO: Explore instead of trying to calc new position, inserting an
    // uncreatable segment as a "pin" before transforming. Then after, 
    // find the "pin", remove it and start from there. 
    fn calc_next_pos(&self, last_action: &Action, res_phrase: &Phrase, old_phrase: &Phrase, word_len_change: isize) -> Option<SegPos> {
        // find index of first segment/syllable after the last SubAction Position
        // reference the value in the original phrase then find it the the res_phrase
        // or take the afters as a "subarray" and find the subarray in the phrase
        match &last_action.kind {
            ActionKind::ReplaceSegment((old_len, _), payload, _) => match payload {
                Payload::Segment(..) => {
                    let old_next_pos = SegPos { 
                        word_index: last_action.pos.word_index,
                        syll_index: last_action.pos.syll_index,
                        seg_index: last_action.pos.seg_index + old_len.get() as usize,
                    };

                    if !old_phrase.in_bounds(old_next_pos) {
                        let old_next_syll = old_phrase[last_action.pos.word_index].syllables.get(last_action.pos.syll_index+1)?;
                        // Find next syllable in result, making sure that we don't accidentally match a previous syllable
                        let sp = res_phrase[last_action.pos.word_index].syllables.iter().enumerate().position(|(i, s)| *s == *old_next_syll && i > last_action.pos.syll_index.saturating_add_signed(word_len_change))?;
                        return Some(SegPos { word_index: last_action.pos.word_index, syll_index: sp, seg_index: 0 })
                    }

                    let old_syll = &old_phrase[last_action.pos.word_index].syllables[last_action.pos.syll_index];

                    let sub_arr: Vec<Segment> = old_syll.segments.range(old_next_pos.seg_index..old_syll.segments.len()).cloned().rev().collect::<Vec<_>>();

                    for (i, syll) in res_phrase[old_next_pos.word_index].syllables.iter().enumerate().skip(old_next_pos.syll_index.saturating_add_signed(word_len_change)) {
                        let mut segs = syll.segments.clone();
                        segs.make_contiguous().reverse();
                        
                        match segs.as_slices().0.windows(sub_arr.len()).position(|w| w == sub_arr) {
                            Some(s) => if s == 0 {
                                return Some(SegPos { word_index: old_next_pos.word_index, syll_index: i, seg_index: syll.segments.len() - sub_arr.len() })
                            },
                            None => continue,
                        }
                        
                    }

                    None
                },
                Payload::Syllable(_) => {
                    Some(SegPos{
                        word_index: last_action.pos.word_index,
                        syll_index: (last_action.pos.syll_index+1).saturating_add_signed(word_len_change),
                        seg_index: 0
                    })
                },
            },
            ActionKind::ReplaceSyllable(_) | ActionKind::ModifySyllable(..) => {
                Some(SegPos{
                    word_index: last_action.pos.word_index,
                    syll_index: (last_action.pos.syll_index+1).saturating_add_signed(word_len_change),
                    seg_index: 0
                })
            },
            ActionKind::PassBoundary => {
                // Needed incase of `$ > $` which loops forever
                if old_phrase == res_phrase {
                    Some(SegPos{
                        word_index: last_action.pos.word_index,
                        syll_index: (last_action.pos.syll_index).saturating_add_signed(word_len_change),
                        seg_index: 1
                    })
                } else {
                    Some(SegPos{
                        word_index: last_action.pos.word_index,
                        syll_index: (last_action.pos.syll_index).saturating_add_signed(word_len_change),
                        seg_index: 0
                    })
                }
            },
            ActionKind::InsertSegment(seg_len, ..) | ActionKind::DeleteSegment(seg_len) => {
                let old_next_pos = SegPos { 
                    word_index: last_action.pos.word_index,
                    syll_index: last_action.pos.syll_index,
                    seg_index: last_action.pos.seg_index + seg_len.get() as usize,
                };

                if !old_phrase.in_bounds(old_next_pos) {
                    let Some(old_next_syll) = old_phrase[last_action.pos.word_index].syllables.get(last_action.pos.syll_index+1) else {
                        return Some(old_next_pos)
                    };
                    // Find next syllable in result, making sure that we don't accidentally match a previous syllable
                    let sp = res_phrase[last_action.pos.word_index].syllables.iter().enumerate().position(|(i, s)| *s == *old_next_syll && i > last_action.pos.syll_index.saturating_add_signed(word_len_change))?;
                    return Some(SegPos { word_index: last_action.pos.word_index, syll_index: sp, seg_index: 0 })
                }

                let old_syll = &old_phrase[last_action.pos.word_index].syllables[last_action.pos.syll_index];

                let sub_arr: Vec<Segment> = old_syll.segments.range(old_next_pos.seg_index..old_syll.segments.len()).cloned().rev().collect::<Vec<_>>();

                for (i, syll) in res_phrase[old_next_pos.word_index].syllables.iter().enumerate().skip(old_next_pos.syll_index.saturating_add_signed(word_len_change)) {
                    let mut segs = syll.segments.clone();
                    segs.make_contiguous().reverse();
                    
                    match segs.as_slices().0.windows(sub_arr.len()).position(|w| w == sub_arr) {
                        Some(s) => if s == 0 {
                            return Some(SegPos { word_index: old_next_pos.word_index, syll_index: i, seg_index: (syll.segments.len() - 1).saturating_sub(sub_arr.len()) })
                        },
                        None => continue,
                    }
                    
                }
                None
            },
            // These may not be right
            ActionKind::InsertSyllable(_) => {
                Some(SegPos{
                    word_index: last_action.pos.word_index,
                    syll_index: (last_action.pos.syll_index+1).saturating_add_signed(word_len_change),
                    seg_index: 0
                })
            },
            // NOTE: This is will never happen because of line 120 in self.apply()
            ActionKind::InsertWord(_) => {
                Some(SegPos{
                    word_index: last_action.pos.word_index+1,
                    syll_index: 0,
                    seg_index: 0
                })
            }
            ActionKind::InsertWordBound => {
                Some(SegPos{
                    word_index: last_action.pos.word_index+1,
                    syll_index: 0,
                    seg_index: 0
                })
            }
            ActionKind::DeleteWordBound => {
                Some(SegPos{
                    word_index: last_action.pos.word_index,
                    syll_index: last_action.pos.word_index,
                    seg_index: last_action.pos.word_index
                })
            }
            ActionKind::DeleteSyllable => {
                Some(SegPos{
                    word_index: last_action.pos.word_index,
                    syll_index: (last_action.pos.syll_index+1).saturating_add_signed(word_len_change),
                    seg_index: 0
                })
            },
            ActionKind::InsertBoundary => {
                // So that rules such as '$ < a$' don't hang
                let offset = if self.input.first().unwrap().kind == ParseElement::SyllBound { 1 } else { 0 };

                Some(SegPos{
                    word_index: last_action.pos.word_index,
                    syll_index: (last_action.pos.syll_index).saturating_add_signed(word_len_change) + offset,
                    seg_index: 0
                })
            },
            ActionKind::DeleteBoundary => {
                Some(SegPos{
                    word_index: last_action.pos.word_index,
                    syll_index: (last_action.pos.syll_index).saturating_add_signed(word_len_change),
                    seg_index: 0
                })
            },
        }
    }
}
