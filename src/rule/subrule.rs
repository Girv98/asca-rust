#![allow(clippy::too_many_arguments)]

// NOTE(girv): lots of duplication here atm (and starting to look like spaghetti), focusing on getting things done before optimising

use std ::{
    cell::RefCell, 
    collections::{HashMap, VecDeque},
    num::NonZeroU8
};

use crate  :: {
    error  :: RuleRuntimeError, 
    rule   :: { Alpha, AlphaMod, BinMod, EnvItem, SpecMod, ModKind, Modifiers, ParseElement, ParseItem, PlaceMod, Position, Reference, RuleType, SupraSegs, UnderlineStruct }, 
    word   :: { FeatKind, NodeKind, Phrase, SegPos, Segment, StressKind, Syllable, Tone, Word },
};

type WrdPos = usize;            // the index of the word in the phrase, for WordBound, the word before the boundary
type SylPos = usize;            // the index of the syllable in the word.syllables array
type BndPos = usize;            // the index of the syllable that has the boundary as it's start
type SetInd = Option<usize>;    // if matched in a set, the index within that set that was matched

#[derive(Debug, Clone, Copy)]
pub(crate) enum MatchElement {
    Segment  (SegPos, SetInd),
    LongSegment(SegPos, SetInd),
    Syllable (WrdPos, SylPos, SetInd),
    SyllBound(WrdPos, BndPos, SetInd),
    WordBound(WrdPos),
}

impl MatchElement {
    pub(crate) fn set_ind(&mut self, si: SetInd) {
        *self = match self {
            MatchElement::Segment(sp, _) => MatchElement::Segment(*sp, si),
            MatchElement::LongSegment(sp, _) => MatchElement::LongSegment(*sp, si),
            MatchElement::Syllable(wp, sp, _) => MatchElement::Syllable(*wp, *sp, si),
            MatchElement::SyllBound(wp, bp, _) => MatchElement::SyllBound(*wp, *bp, si),
            MatchElement::WordBound(..) => unimplemented!()
        }
    }
}

#[derive(Debug)]
enum Payload {
    Segment(Segment, Option<SupraSegs>),
    Syllable(Syllable),
}

type OldLen = NonZeroU8;
type NewLen = NonZeroU8;
type SegLen = NonZeroU8;
type ErrPos = Position;

#[derive(Debug)]
enum ActionKind {
    DeleteSyllable,
    DeleteSegment(SegLen),
    InsertSegment(SegLen, Segment, Option<SupraSegs>, ErrPos),
    InsertSyllable(Syllable),
    ReplaceSegment((OldLen, NewLen), Payload, ErrPos),
    ReplaceSyllable(Syllable),
    ModifySyllable(SupraSegs, ErrPos),
    PassBoundary,
    InsertBoundary,
    DeleteBoundary,
}


#[derive(Debug)]
struct SubAction {
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
    fn get_contexts(&self) -> Vec<(&Vec<ParseItem>, &Option<UnderlineStruct>, &Vec<ParseItem>)> {
        match &self.context {
            Some(item) => item.envs.iter().map(|e| (&e.before, &e.center, &e.after)).collect(),
            None => vec![],
        }
    }

    fn get_exceptions(&self) -> Vec<(&Vec<ParseItem>, &Option<UnderlineStruct>, &Vec<ParseItem>)> {
        match &self.except {
            Some(item) => item.envs.iter().map(|e| (&e.before, &e.center, &e.after)).collect(),
            None => vec![],
        }
    }

    fn is_cross_bound(&self) -> bool { self.inp_x_bound || self.env_x_bound }

    pub(crate) fn apply(&self, phrase: Phrase) -> Result<Phrase, RuleRuntimeError> {
        if phrase.is_empty() || (phrase.len() == 1 && phrase[0].syllables.is_empty()) { return Ok(phrase) }

        // '##' will not match if there's less that 2 words, 
        if self.is_cross_bound() && phrase.len() < 2 { return Ok(phrase) }

        let res = self.apply_phrase(if self.is_reversed { phrase.reversed() } else { phrase })?;

        if self.is_reversed { Ok(res.reversed()) } else { Ok(res) }
    }

    fn apply_phrase(&self, phrase: Phrase) -> Result<Phrase, RuleRuntimeError> {
        if self.rule_type == RuleType::Insertion {
            self.transform(&phrase, vec![], &mut None)
        } else {
            self.apply_other(phrase)
        }
    }

    fn set_start(&self, res: &[MatchElement], phrase: &Phrase) -> (SegPos, bool) {
        match *res.first().expect("res is not empty") {
            MatchElement::Segment(sp, _) | MatchElement::LongSegment(sp, _)  => (sp, true),
            MatchElement::Syllable(wp, s, _)  |
            MatchElement::SyllBound(wp, s, _) => (SegPos::new(wp, s, 0), true),
            MatchElement::WordBound(wp) => {
                let mut pos = SegPos::new(wp+1, 0, 0);
                pos.word_decrement(phrase);
                (pos, false)
            },
        }
    }

    fn set_end(&self, res: &[MatchElement], phrase: &Phrase) -> (SegPos, bool) {
        match *res.last().expect("res is not empty") {
            MatchElement::Segment(mut sp, _) | MatchElement::LongSegment(mut sp, _)  => {
                // So that long vowels work
                let mut seg_len = phrase.seg_length_at(sp);
                while seg_len > 1 {
                    sp.increment(phrase);
                    seg_len -= 1;
                }
                (sp, true)
            },
            MatchElement::Syllable(wp, s, _)  => (SegPos::new(wp, s, phrase[wp].syllables[s].segments.len()-1), true),
            MatchElement::SyllBound(wp, s, _) => (SegPos::new(wp, s, 0), false),
            MatchElement::WordBound(wp) => (SegPos::new(wp+1, 0, 0), false),
        }
    }

    fn apply_other(&self, phrase: Phrase) -> Result<Phrase, RuleRuntimeError> {
        let mut phrase = phrase;
        let mut cur_index = SegPos::new(0, 0, 0);
        // TODO(girv): `$ > *` or any broad deletion rule without context/exception should give a warning to the user
        loop {
            self.alphas.borrow_mut().clear();
            self.references.borrow_mut().clear();
            let (res, mut next_index) = self.input_match_at(&phrase, cur_index, 0)?;
            if res.is_empty() {
                // No match
                if cur_index.word_index < phrase.len() - 1 {
                    cur_index.word_increment(&phrase);
                    continue
                } else {
                    break
                }
            }

            let (start, inc_start) = self.set_start(&res,  &phrase);
            let (end, inc_end) = self.set_end(&res, &phrase);

            if !self.match_contexts_and_exceptions(&phrase, &res, start, end, inc_start, inc_end)? {
                if let Some(ni) = next_index { 
                    cur_index = ni;
                    continue;
                }
                // end of word
                if cur_index.word_index < phrase.len() - 1 {
                    cur_index.word_increment(&phrase);
                    continue;
                } else {
                    break;
                }
            }

            phrase = self.transform(&phrase, res, &mut next_index)?;
            
            if let Some(ci) = next_index { 
                cur_index = ci;
            } else {
                // End of Word
                if cur_index.word_index < phrase.len() - 1 {
                    cur_index.word_increment(&phrase);
                    continue
                } else {
                    break
                }
            }
        }
        Ok(phrase)
    }

    fn metathesis(&self, phrase: &Phrase, input: Vec<MatchElement>, _next_pos: &mut Option<SegPos>) -> Result<Phrase, RuleRuntimeError> {
        let mut res_phrase = phrase.clone();
        for z in 0..(input.len() / 2) {
            match (input[z], input[input.len()-1-z]) {
                (MatchElement::Segment(li, _), MatchElement::Segment(ri, _)) => {
                    // FIXME: If we swap syllables or boundaries then do this, these SegPos may not be correct
                    let sl = res_phrase.get_seg_at(li).unwrap();
                    let sr = res_phrase.get_seg_at(ri).unwrap();
                    res_phrase[li.word_index].syllables[li.syll_index].segments[li.seg_index] = sr;
                    res_phrase[ri.word_index].syllables[ri.syll_index].segments[ri.seg_index] = sl;
                },
                (MatchElement::Segment(li, _), MatchElement::LongSegment(ri, _)) => {
                    let sl = res_phrase.get_seg_at(li).unwrap();
                    let sr = res_phrase.get_seg_at(ri).unwrap();
                    let sr_length = res_phrase.seg_length_at(ri);

                    res_phrase[li.word_index].syllables[li.syll_index].segments[li.seg_index] = sr;
                    for _ in 0..sr_length-1 { res_phrase[li.word_index].syllables[li.syll_index].segments.insert(li.seg_index, sr); }
                    res_phrase[ri.word_index].syllables[ri.syll_index].segments[ri.seg_index] = sl;
                    for _ in 0..sr_length-1 { res_phrase[ri.word_index].syllables[ri.syll_index].segments.remove(ri.seg_index + 1); }
                },
                (MatchElement::LongSegment(li, _), MatchElement::Segment(ri, _)) => {
                    let sl = res_phrase.get_seg_at(li).unwrap();
                    let sr = res_phrase.get_seg_at(ri).unwrap();
                    let sl_length = res_phrase.seg_length_at(li);

                    res_phrase[li.word_index].syllables[li.syll_index].segments[li.seg_index] = sr;
                    for _ in 0..sl_length-1 { res_phrase[li.word_index].syllables[li.syll_index].segments.remove(li.seg_index + 1); }
                    res_phrase[ri.word_index].syllables[ri.syll_index].segments[ri.seg_index] = sl;
                    for _ in 0..sl_length-1 { res_phrase[ri.word_index].syllables[ri.syll_index].segments.insert(ri.seg_index, sl); }
                },
                (MatchElement::LongSegment(li, _), MatchElement::LongSegment(ri, _)) => {
                    let sl = res_phrase.get_seg_at(li).unwrap();
                    let sr = res_phrase.get_seg_at(ri).unwrap();
                    let sl_length = res_phrase.seg_length_at(li);
                    let sr_length = res_phrase.seg_length_at(ri);

                    // Swap initial segments
                    res_phrase[li.word_index].syllables[li.syll_index].segments[li.seg_index] = sr;
                    res_phrase[ri.word_index].syllables[ri.syll_index].segments[ri.seg_index] = sl;
                    // Remove long
                    for _ in 0..sl_length-1 { res_phrase[li.word_index].syllables[li.syll_index].segments.remove(li.seg_index + 1); }
                    for _ in 0..sr_length-1 { res_phrase[ri.word_index].syllables[ri.syll_index].segments.remove(ri.seg_index + 1); }
                    // Add long
                    for _ in 0..sr_length-1 { res_phrase[li.word_index].syllables[li.syll_index].segments.insert(li.seg_index, sr); }
                    for _ in 0..sl_length-1 { res_phrase[ri.word_index].syllables[ri.syll_index].segments.insert(ri.seg_index, sl); }
                },
                (MatchElement::Syllable(lw, ls, _), MatchElement::Syllable(rw, rs, _)) => {
                    res_phrase.swap_sylls(lw, ls, rw, rs);
                },
                (MatchElement::SyllBound(..), MatchElement::SyllBound(..)) => {/* Do nothing */},
                (MatchElement::Segment(si, _), MatchElement::SyllBound(wp, bi, _)) => {
                    // FIXME(girv): this won't work for rules with `...`, it may be necessary to disallow `$` in `...` rules
                    let seg = res_phrase[si.word_index].syllables[si.syll_index].segments[si.seg_index];
                    // If before word end
                    if bi < res_phrase[wp].syllables.len() {
                        res_phrase[wp].syllables[bi].segments.push_front(seg);
                        res_phrase[si.word_index].syllables[si.syll_index].segments.remove(si.seg_index); // pop_back()                                
                    } else { // if at word end
                        res_phrase[wp].syllables.push(Syllable { segments: VecDeque::new(), stress: StressKind::Unstressed, tone: 0 });
                        res_phrase[wp].syllables.last_mut().unwrap().segments.push_front(seg);
                        res_phrase[si.word_index].syllables[si.syll_index].segments.remove(si.seg_index); // pop_back()
                    }                 
                    // Remove syllable if empty
                    if res_phrase[si.word_index].syllables[si.syll_index].segments.is_empty() {
                        res_phrase[si.word_index].syllables.remove(si.syll_index);
                    }
                },
                (MatchElement::LongSegment(si, _), MatchElement::SyllBound(wp, bi, _)) => { 
                    let seg = res_phrase[si.word_index].syllables[si.syll_index].segments[si.seg_index];
                    let seg_length = res_phrase.seg_length_at(si);
                    // If before word end
                    if bi < res_phrase[wp].syllables.len() {
                        for _ in 0..seg_length {
                            res_phrase[wp].syllables[bi].segments.push_front(seg);
                            res_phrase[si.word_index].syllables[si.syll_index].segments.remove(si.seg_index);
                        }
                    } else { // if at word end
                        res_phrase[wp].syllables.push(Syllable { segments: VecDeque::new(), stress: StressKind::Unstressed, tone: 0 });
                        for _ in 0..seg_length {
                            res_phrase[wp].syllables.last_mut().unwrap().segments.push_front(seg);
                            res_phrase[si.word_index].syllables[si.syll_index].segments.remove(si.seg_index); // pop_back()
                        }
                    }
                    // Remove syllable if empty
                    if res_phrase[si.word_index].syllables[si.syll_index].segments.is_empty() {
                        res_phrase[si.word_index].syllables.remove(si.syll_index);
                    }
                },
                (MatchElement::SyllBound(wp, bi, _), MatchElement::Segment(si, _)) => {
                    // FIXME(girv): this won't work for rules with `...`, it may be necessary to disallow `$` in `...` rules
                    let seg = res_phrase[si.word_index].syllables[si.syll_index].segments[si.seg_index];
                    // If after word start
                    if bi > 0 {
                        res_phrase[wp].syllables[bi-1].segments.push_back(seg);
                        res_phrase[si.word_index].syllables[si.syll_index].segments.remove(si.seg_index); // pop_front()                                
                    } else { // if at word start
                        res_phrase[si.word_index].syllables[si.syll_index].segments.remove(si.seg_index); // pop_front()
                        res_phrase[wp].syllables.insert(0, Syllable { segments: VecDeque::new(), stress: StressKind::Unstressed, tone: 0 });
                        res_phrase[wp].syllables.first_mut().unwrap().segments.push_back(seg);
                    }
                    // Remove syllable if empty
                    if res_phrase[si.word_index].syllables[si.syll_index].segments.is_empty() {
                        res_phrase[si.word_index].syllables.remove(si.syll_index);
                    }
                },
                (MatchElement::SyllBound(wp, bi, _), MatchElement::LongSegment(si, _)) => {
                    let seg = res_phrase[si.word_index].syllables[si.syll_index].segments[si.seg_index];
                    let seg_length = res_phrase.seg_length_at(si);
                    if bi > 0 {
                        for _ in 0..seg_length {
                            res_phrase[wp].syllables[bi-1].segments.push_back(seg);
                            res_phrase[si.word_index].syllables[si.syll_index].segments.remove(si.seg_index); // pop_front()                                
                        }
                    } else {
                        res_phrase[wp].syllables.insert(0, Syllable { segments: VecDeque::new(), stress: StressKind::Unstressed, tone: 0 });
                        for _ in 0..seg_length {
                            res_phrase[si.word_index].syllables[si.syll_index+1].segments.remove(si.seg_index); // pop_front()
                            res_phrase[wp].syllables.first_mut().unwrap().segments.push_back(seg);
                        }
                    }
                    if res_phrase[si.word_index].syllables[si.syll_index].segments.is_empty() {
                        res_phrase[si.word_index].syllables.remove(si.syll_index);
                    }
                },
                
                // Move a segment from one word to another i.e. a napron => an apron
                (MatchElement::WordBound(wp), MatchElement::Segment(sp, _)) => {
                    debug_assert!(wp < sp.word_index);
                    let seg = res_phrase[sp.word_index].syllables[sp.syll_index].segments[sp.seg_index];
                    // Move seg to end of previous word
                    res_phrase[wp].syllables.last_mut().unwrap().segments.push_back(seg);
                    res_phrase[sp.word_index].syllables[sp.syll_index].segments.remove(sp.seg_index);
                    // Remove segment's syllable if now empty
                    if res_phrase[sp.word_index].syllables[sp.syll_index].segments.is_empty() {
                        res_phrase[sp.word_index].syllables.remove(sp.syll_index);
                    }

                },
                (MatchElement::Segment(sp, _), MatchElement::WordBound(wp)) => {
                    debug_assert!(sp.word_index <= wp);
                    let seg = res_phrase[sp.word_index].syllables[sp.syll_index].segments[sp.seg_index];
                    // if not at phrase end
                    if wp < res_phrase.len() - 1 {
                        res_phrase[wp+1].syllables[0].segments.push_front(seg);
                        res_phrase[sp.word_index].syllables[sp.syll_index].segments.remove(sp.seg_index);
                    } else { // if at phrase end
                        let mut new_syll = Syllable::new();
                        new_syll.segments.push_front(seg);
                        res_phrase.push(Word { syllables: vec![new_syll] });
                        res_phrase[sp.word_index].syllables[sp.syll_index].segments.remove(sp.seg_index);
                    }
                    // Remove segment's syllable if now empty
                    if res_phrase[sp.word_index].syllables[sp.syll_index].segments.is_empty() {
                        res_phrase[sp.word_index].syllables.remove(sp.syll_index);
                    }
                },
                (MatchElement::WordBound(wp), MatchElement::LongSegment(sp, _)) => {
                    debug_assert!(wp < sp.word_index);
                    let seg = res_phrase[sp.word_index].syllables[sp.syll_index].segments[sp.seg_index];
                    let seg_length = res_phrase.seg_length_at(sp);

                    let syll_len = res_phrase[wp].syllables.len()-1;
                    for _ in 0..seg_length {
                        res_phrase[wp].syllables[syll_len].segments.push_back(seg);
                        res_phrase[sp.word_index].syllables[sp.syll_index].segments.remove(sp.seg_index);
                    }
                    // Remove syllable if empty
                    if res_phrase[sp.word_index].syllables[sp.syll_index].segments.is_empty() {
                        res_phrase[sp.word_index].syllables.remove(sp.syll_index);
                    }
                },
                (MatchElement::LongSegment(sp, _), MatchElement::WordBound(wp)) => {
                    debug_assert!(sp.word_index <= wp);
                    let seg = res_phrase[sp.word_index].syllables[sp.syll_index].segments[sp.seg_index];
                    let seg_length = res_phrase.seg_length_at(sp);
                    // if not at phrase end
                    if wp < res_phrase.len() - 1 {
                        for _ in 0..seg_length {
                            res_phrase[wp+1].syllables[0].segments.push_front(seg);
                            res_phrase[sp.word_index].syllables[sp.syll_index].segments.remove(sp.seg_index);
                        }
                    } else { // if at phrase end
                        let mut new_syll = Syllable::new();
                        for _ in 0..seg_length {
                            new_syll.segments.push_front(seg);
                            res_phrase[sp.word_index].syllables[sp.syll_index].segments.remove(sp.seg_index);
                        }
                        res_phrase.push(Word { syllables: vec![new_syll] });
                    }   
                    // Remove syllable if empty
                    if res_phrase[sp.word_index].syllables[sp.syll_index].segments.is_empty() {
                        res_phrase[sp.word_index].syllables.remove(sp.syll_index);
                    }
                },
                // Move a syllable from one word to another
                (MatchElement::WordBound(wp), MatchElement::Syllable(swp, sp, _)) => {
                    debug_assert!(wp < swp);
                    let syll = res_phrase[swp].syllables[sp].clone();
                    // Move syll to end of previous word
                    res_phrase[wp].syllables.push(syll);
                    res_phrase[swp].syllables.remove(sp);
                    // Remove word that syllable was in if now empty
                    if res_phrase[swp].syllables.is_empty() {
                        res_phrase.remove(swp);
                    }
                },
                (MatchElement::Syllable(swp, sp, _), MatchElement::WordBound(wp)) => {
                    debug_assert!(swp <= wp);
                    let syll = res_phrase[swp].syllables[sp].clone();
                    // if not at phrase end
                    if wp < res_phrase.len() - 1 {
                        res_phrase[wp+1].syllables.insert(0, syll);
                        res_phrase[swp].syllables.remove(sp);
                    } else { // if at phrase end
                        res_phrase.push(Word { syllables: vec![syll] });
                        res_phrase[swp].syllables.remove(sp);
                    }
                },
                (MatchElement::SyllBound(..), MatchElement::WordBound(_)) | 
                (MatchElement::WordBound(_), MatchElement::SyllBound(..)) => {
                    let end = input.len()-1-z;
                    return Err(RuleRuntimeError::MetathWordBoundary(self.input[z].position, self.input[end].position))
                },
                (MatchElement::WordBound(_), MatchElement::WordBound(_)) => {/* Do nothing */},
                // I think we're just gonna disallow these, I can't think of a valid rule where these make sense
                (MatchElement::Segment(..), MatchElement::Syllable(..)) |
                (MatchElement::LongSegment(..), MatchElement::Syllable(..)) |
                (MatchElement::Syllable(..), MatchElement::Segment(..)) |
                (MatchElement::Syllable(..), MatchElement::LongSegment(..)) => {
                    let end = input.len()-1-z;
                    return Err(RuleRuntimeError::MetathSyllSegment(self.input[z].position, self.input[end].position))
                },
                (MatchElement::Syllable(..), MatchElement::SyllBound(..)) |
                (MatchElement::SyllBound(..), MatchElement::Syllable(..)) => {
                    let end = input.len()-1-z;
                    return Err(RuleRuntimeError::MetathSyllBoundary(self.input[z].position, self.input[end].position))
                },
            }
        }
        // TODO: Update current position?
        Ok(res_phrase)
    }

    fn deletion(&self, phrase: &Phrase, input: Vec<MatchElement>, next_pos: &mut Option<SegPos>) -> Result<Phrase, RuleRuntimeError> {
        let word_pos = if let Some(np) = next_pos { np.word_index } else { 0 };
        let mut pos = SegPos::new(word_pos, 0, 0);
        let mut res_phrase = phrase.clone();
        for z in input.into_iter().rev() {
            match z {
                MatchElement::Segment(i, _) => {
                    pos = i;
                    if res_phrase[i.word_index].syllables.len() <= 1 && res_phrase[i.word_index].syllables[i.syll_index].segments.len() <= 1 {
                        return Err(RuleRuntimeError::DeletionOnlySeg)
                    }
                    // remove segment
                    res_phrase[i.word_index].syllables[i.syll_index].segments.remove(i.seg_index);
                    // if that was the only segment in that syllable, remove the syllable
                    if res_phrase[i.word_index].syllables[i.syll_index].segments.is_empty() {
                        res_phrase[i.word_index].syllables.remove(i.syll_index);
                    }
                },
                MatchElement::LongSegment(i, _) => {
                    pos = i;
                    if res_phrase[i.word_index].syllables.len() <= 1 && res_phrase[i.word_index].syllables[i.syll_index].segments.len() <= 1 {
                        return Err(RuleRuntimeError::DeletionOnlySeg)
                    }
                    // remove segment
                    
                    for _ in 0..phrase.seg_length_at(i) {
                        res_phrase[i.word_index].syllables[i.syll_index].segments.remove(i.seg_index);
                    }
                    // if that was the only segment in that syllable, remove the syllable
                    if res_phrase[i.word_index].syllables[i.syll_index].segments.is_empty() {
                        res_phrase[i.word_index].syllables.remove(i.syll_index);
                    }
                },
                MatchElement::Syllable(wp, i, _) => {
                    // remove syllable
                    if ((!self.inp_x_bound && !self.env_x_bound) || res_phrase.len() == 1) && res_phrase[wp].syllables.len() <= 1 {
                        return Err(RuleRuntimeError::DeletionOnlySyll)
                    }
                    pos.syll_index = i;
                    pos.seg_index = 0;
                    pos.decrement(&res_phrase);
                    res_phrase[wp].remove_syll(i);

                    if res_phrase[wp].syllables.is_empty() && !self.inp_x_bound {
                        res_phrase.remove(wp);
                    }
                },
                MatchElement::WordBound(wp) => {
                    // wp ## wp+1 -> wp.wp+1
                    
                    pos.word_index = wp+1;
                    pos.word_decrement(phrase);

                    let w1 = res_phrase[wp+1].syllables.clone();
                    res_phrase[wp].syllables.extend(w1);
                    res_phrase.remove(wp+1);
                },
                MatchElement::SyllBound(wp, i, _) => {
                    // join the two neighbouring syllables
                    // if one has stress and/or tone, joined syll gets them
                    // if they both have stress, highest wins
                    // if they both have tone, join them i.e. ma5a1 > ma:51

                    if i == 0 || i >= res_phrase[wp].syllables.len() {
                        // can't delete a word boundary
                        continue;
                    }
                    pos.syll_index = i;
                    pos.seg_index = 0;
                    pos.decrement(&res_phrase);

                    let mut syll_segs = res_phrase[wp].syllables[i].segments.clone();
                    res_phrase[wp].syllables[i-1].segments.append(&mut syll_segs);

                    res_phrase[wp].syllables[i-1].stress = match (res_phrase[wp].syllables[i-1].stress, res_phrase[wp].syllables[i].stress) {
                        (StressKind::Primary, _) | (_, StressKind::Primary) => StressKind::Primary,
                        (StressKind::Secondary, StressKind::Secondary)  | 
                        (StressKind::Secondary, StressKind::Unstressed) | 
                        (StressKind::Unstressed, StressKind::Secondary)  => StressKind::Secondary,
                        (StressKind::Unstressed, StressKind::Unstressed) => StressKind::Unstressed,
                    };

                    res_phrase[wp].syllables[i-1].tone = Self::concat_tone(res_phrase[wp].syllables[i-1].tone, res_phrase[wp].syllables[i].tone);
                    res_phrase[wp].syllables.remove(i);
                },
            }
        }
        if let Some(next) = next_pos {
            pos.increment(&res_phrase);
            *next = pos;
        }
        Ok(res_phrase)
    }

    fn syll_inc(phrase: &Phrase, pos: &mut SegPos) {
        pos.syll_index += 1; pos.seg_index = 0;
        if !phrase.in_bounds(*pos) {
            pos.word_increment(phrase);
        }
    }

    fn insertion_by_syllable(&self, phrase: &Phrase, center_cont: &UnderlineStruct, before_cont: &[ParseItem], after_cont: &[ParseItem]) -> Result<Phrase, RuleRuntimeError> {

        let mut phrase = phrase.clone();
        let mut pos = SegPos::new(0, 0, 0);
        
        // center_cont.before is reversed so we have to re-reverse it
        // in order to join it with the center_cont.after (efficient!)
        let mut items: Vec<_> = center_cont.before.iter().rev().cloned().collect();
        items.extend_from_slice(&center_cont.after);

        let mut before_cont = before_cont.to_vec();
        before_cont.reverse();

        'outer: while phrase.in_bounds(pos) {
            self.alphas.borrow_mut().clear();
            self.references.borrow_mut().clear();
            let syll = &phrase[pos.word_index].syllables[pos.syll_index];

            if items.is_empty() && !syll.segments.is_empty() {
                Self::syll_inc(&phrase, &mut pos); continue;
            }
            if !self.match_stress(&center_cont.stress, syll, center_cont.position)? {
                Self::syll_inc(&phrase, &mut pos); continue;
            }
            if let Some(tone) = center_cont.tone && !self.match_tone(&tone, syll) {
                Self::syll_inc(&phrase, &mut pos); continue;
            }

            let mut ins_pos = pos;
            if !self.match_underline_struct_items(&phrase, &items, pos, true, &mut Some((center_cont.before.len(), &mut ins_pos)))? {
                Self::syll_inc(&phrase, &mut pos); continue;
            }

            let mut end_pos = pos; end_pos.syll_index += 1; end_pos.decrement(&phrase);
            let phrase_rev = phrase.reversed();

            // Check before and after contexts
            if !(before_cont.is_empty() || self.match_before_env(&before_cont, &phrase_rev, &pos.reversed(&phrase), false, true, true)?) 
            || !(after_cont.is_empty() || self.match_after_env(after_cont, &phrase, &end_pos, false, true, true)?) {
                Self::syll_inc(&phrase, &mut pos); continue;
            }

            let insert_pos = ins_pos;
            // If '_' is flanked by ellipses then error, e.g.
            // valid: <_..C> <..C_..C> <C_..>
            // not valid: <.._..> <C.._..C> <.._..C>
            // let insert_pos = match (center_cont.before.first(), center_cont.after.first()) {
            //     (None, _) => pos,
            //     (Some(_), None) => todo!("insert at end of syllable"),
            //     (Some(l), Some(r)) => match (&l.kind, &r.kind) {
            //         (ParseElement::OptEllipsis, ParseElement::OptEllipsis) |
            //         (ParseElement::Ellipsis, ParseElement::OptEllipsis) |
            //         (ParseElement::OptEllipsis, ParseElement::Ellipsis) |
            //         (ParseElement::Ellipsis, ParseElement::Ellipsis) => todo!("Err: Can't discern insertion position"),
                    
            //         _ => todo!("Find actual insert point")
            //     },
            // };


            // Allows for | :{...}:
            'inner: for (before_expt, center_expt, after_expt) in self.get_exceptions() {
                // TODO FIXME: We must take insertion position into account, else it would treat <s_n> the same as <sn_>
                // but this isn't entirely right. "| <C.._..C>" does not care about insert position
                if let Some(ce) = &center_expt {
                    let mut items: Vec<_> = ce.before.iter().rev().cloned().collect(); items.extend_from_slice(&ce.after);                    
                    let mut ip = insert_pos;
                    if (items.is_empty() && syll.segments.is_empty()) || self.match_underline_struct_items(&phrase, &items, pos, true, &mut Some((ce.before.len(), &mut ip)))? || ip != insert_pos {
                        if !self.match_stress(&ce.stress, syll, ce.position)? {
                            continue 'inner;
                        }
                        if let Some(tone) = ce.tone && !self.match_tone(&tone, syll) {
                            continue 'inner;
                        }
                        let mut before_expt = before_expt.clone(); before_expt.reverse();
                        if (before_expt.is_empty() || self.match_before_env(&before_expt, &phrase_rev, &pos.reversed(&phrase), false, true, false)?) 
                        && (after_expt.is_empty()  || self.match_after_env(after_expt, &phrase, &end_pos, false, true, false)?) {
                            Self::syll_inc(&phrase, &mut pos); continue 'outer;
                        }
                    }
                } else {
                    if before_expt.is_empty() && after_expt.is_empty() && center_expt.is_some() {
                        continue 'inner;
                    }
                    let mut before_expt = before_expt.clone(); before_expt.reverse();
                    if (before_expt.is_empty() || self.match_before_env(&before_expt, &phrase_rev, &pos.reversed(&phrase), false, true, false)?) 
                    && (after_expt.is_empty()  || self.match_after_env(after_expt, &phrase, &end_pos, false, true, false)?) {
                        Self::syll_inc(&phrase, &mut pos); continue 'outer;
                    }
                }
            }
            
            // TODO: Sanity check output? Or allow syllables to be inserted?
            let (res, _) = self.insert(&phrase, insert_pos, false)?;
            phrase = res;

            Self::syll_inc(&phrase, &mut pos);
        }

        Ok(phrase)
    }

    fn insertion_by_segment(&self, 
        phrase: &Phrase, before_cont: &[ParseItem], after_cont: &[ParseItem],
        before_expt: &[ParseItem], center_expt: &Option<UnderlineStruct>, after_expt: &[ParseItem]
    ) -> Result<Phrase, RuleRuntimeError> {
        // find insertion position using context
        // "Parse" and insert output
        if before_cont.is_empty() && after_cont.is_empty() && before_expt.is_empty() && after_expt.is_empty() && center_expt.is_none() {
            return Err(RuleRuntimeError::InsertionNoEnv(self.output.last().unwrap().position))
        }
        
        let mut res_phrase = phrase.clone();
        let mut pos = SegPos::new(0, 0, 0);
        
        let is_context_after = before_cont.is_empty() && !after_cont.is_empty();

        while res_phrase.in_bounds(pos) {
            self.alphas.borrow_mut().clear();
            self.references.borrow_mut().clear();
            match self.insertion_match(&res_phrase, pos)? {
                Some(ins) => {                                    
                    if self.insertion_match_exceptions(&res_phrase, ins)? {
                        if pos.word_index < res_phrase.len() - 1 {
                            pos.word_increment(&res_phrase);
                        } else {
                            pos.increment(&res_phrase);
                        }
                        continue
                    }
                    let (res, next_pos) = self.insert(&res_phrase, ins, is_context_after)?;
                    res_phrase = res;
        
                    if let Some(np) = next_pos {
                        pos = np;
                        if !is_context_after && pos.at_syll_end(&res_phrase) {
                            pos.increment(&res_phrase);
                        }
                    } else {
                        // End of Word
                        if pos.word_index < res_phrase.len() - 1 {
                            pos.word_increment(&res_phrase);
                            continue
                        } else {
                            break
                        }
                    }
                },
                None => if pos.word_index < res_phrase.len() - 1 {
                    pos.word_increment(&res_phrase);
                    continue
                } else {
                    break
                }
            }
        }
        Ok(res_phrase)
    }

    fn insertion(&self, phrase: &Phrase) -> Result<Phrase, RuleRuntimeError> {
        let empty = Vec::new();
        let context = self.get_contexts();
        let exceptions = self.get_exceptions();

        let ((before_cont, center_cont, after_cont), (before_expt, center_expt, after_expt)) = match (context.len().cmp(&1), exceptions.len().cmp(&1)) {
            // TODO FIXME: fix insertion_by_segment to allow this
            // (std::cmp::Ordering::Equal, std::cmp::Ordering::Greater) |
            (std::cmp::Ordering::Equal,  std::cmp::Ordering::Less) => (context[0],(&empty, &None, &empty)),
            (std::cmp::Ordering::Equal, std::cmp::Ordering::Equal) => (context[0], exceptions[0]),
            (std::cmp::Ordering::Less,  std::cmp::Ordering::Equal) => ((&empty, &None, &empty), exceptions[0]),
            (std::cmp::Ordering::Less,   std::cmp::Ordering::Less) => return Err(RuleRuntimeError::InsertionNoEnv(self.output.last().unwrap().position)),
            (std::cmp::Ordering::Greater, _) => return Err(RuleRuntimeError::InsertionGroupedEnv(self.context.clone().unwrap().position)),
            // TODO FIXME: this is implemented and works fine when inserting by syllable
            (_, std::cmp::Ordering::Greater) => return Err(RuleRuntimeError::InsertionGroupedEnv(self.except.clone().unwrap().position)),
        };

        if let Some(center) = center_cont {
            self.insertion_by_syllable(phrase, center, before_cont, after_cont)
        } else {
            self.insertion_by_segment(phrase, before_cont, after_cont, before_expt, center_expt, after_expt)
        }
    }

    fn transform(&self, phrase: &Phrase, input: Vec<MatchElement>, next_pos: &mut Option<SegPos>) -> Result<Phrase, RuleRuntimeError> {
        match self.rule_type {
            RuleType::Substitution => self.substitution(phrase, input, next_pos),
            RuleType::Metathesis   => self.metathesis(phrase, input, next_pos),
            RuleType::Deletion     => self.deletion(phrase, input, next_pos),
            RuleType::Insertion    => self.insertion(phrase),
        }
    }

    fn gen_syll_from_struct(&self, items: &[ParseItem], stress: &Option<SpecMod>, tone: &Option<Tone>, refr: &Option<usize>, err_pos: Position, is_inserting: bool) -> Result<Syllable, RuleRuntimeError> {
        let mut syll = Syllable::new();
        syll.apply_syll_mods(&self.alphas, &SupraSegs { stress: *stress, length: None, tone: *tone }, err_pos)?;

        for item in items {
            match &item.kind {
                ParseElement::Ellipsis | ParseElement::OptEllipsis => return Err(if is_inserting {RuleRuntimeError::InsertionEllipsis(item.position)} else {RuleRuntimeError::SubstitutionEllipsis(item.position)}),
                ParseElement::Matrix(..) => return Err(if is_inserting {RuleRuntimeError::InsertionMatrix(item.position)} else {RuleRuntimeError::SubstitutionMatrix(item.position)}),
                ParseElement::Optional(..) => return Err(if is_inserting {RuleRuntimeError::InsertionOpt(item.position)} else {RuleRuntimeError::SubstitutionOpt(item.position)}),
                ParseElement::Set(_) => return Err(if is_inserting {RuleRuntimeError::InsertionSet(item.position)} else {RuleRuntimeError::SubstitutionSet(item.position)}),
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
                
                },
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
                },
                _ => unreachable!()
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

    fn matrix_increment(&self, phrase: &Phrase, pos: &mut SegPos) {
        // the way we implement `long` vowels means we need to do this
        let seg_length = phrase.seg_length_at(*pos);
        for _ in 1..seg_length {
            pos.increment(phrase);
        }
    }
}

impl SubRule { // Substitution
    const ONE: NonZeroU8 = NonZeroU8::new(1).unwrap();
    
    fn non_zero_len(seg_len: u8) -> NonZeroU8 { NonZeroU8::new(seg_len).unwrap_or(Self::ONE) }

    fn substitution(&self, phrase: &Phrase, input: Vec<MatchElement>, next_pos: &mut Option<SegPos>) -> Result<Phrase, RuleRuntimeError> {
        let input_filt = self.input.iter().filter(|x| x.kind != ParseElement::Ellipsis && x.kind != ParseElement::OptEllipsis).cloned().collect::<Vec<_>>();
        let output_filt = self.output.iter().filter(|x| x.kind != ParseElement::Ellipsis && x.kind != ParseElement::OptEllipsis).cloned().collect::<Vec<_>>();

        // Just in case input/output is somehow only ellipses TODO: probs should error
        if input_filt.is_empty() || output_filt.is_empty() { return Ok(phrase.clone()) }

        if self.input.len() != input_filt.len() || self.output.len() != output_filt.len() {
            return self.substitution_ellipses(phrase, input, next_pos)
        }
        
        let actions = self.substitution_gen_actions(phrase, &input_filt, &output_filt, &input)?;
        let (res_phrase, last_syll_len_change) = self.apply_sub_actions(phrase, &actions)?;
        
        if let Some(next) = next_pos {
            let last_action = actions.last().expect("Output is not empty");
            match self.sub_calc_next_pos(last_action, &res_phrase, phrase, last_syll_len_change) {
                Some(np) => *next = np,
                None => *next_pos = None,
            }
        }

        Ok(res_phrase)
    }

    fn substitution_ellipses(&self, phrase: &Phrase, input: Vec<MatchElement>, next_pos: &mut Option<SegPos>) -> Result<Phrase, RuleRuntimeError> {
        // Ellipses change the semantics
        // E.G. p...k > pf...g  is different to p...k > p...fg

        let mut in_parts = Vec::new();
        for x in self.input.split(|x| x.kind == ParseElement::Ellipsis || x.kind == ParseElement::OptEllipsis) {
            in_parts.push(x);
        }

        let mut out_parts = Vec::new();
        for x in self.output.split(|x| x.kind == ParseElement::Ellipsis || x.kind == ParseElement::OptEllipsis) {
            out_parts.push(x);
        }

        if in_parts.len() != out_parts.len() {
            let all_pos = self.input.iter().chain(self.output.iter()).filter_map(|x| {
                if x.kind == ParseElement::Ellipsis || x.kind == ParseElement::OptEllipsis {
                    Some(x.position)
                } else {
                    None
                }
            }).collect::<Vec<_>>();
            return Err(RuleRuntimeError::UnevenEllipsis(all_pos))
        }

        let mut actions: Vec<SubAction> = Vec::with_capacity(input.len());
        let mut input = input;

        for (i, o) in in_parts.iter().zip(out_parts.iter()) {
            actions.extend(self.substitution_gen_actions(phrase, i, o, &input)?);
            
            for _ in 0..i.len() {
                input.remove(0);
            }
        }
        
        let (res_phrase, last_syll_len_change) = self.apply_sub_actions(phrase, &actions)?;

        if let Some(next) = next_pos {
            let last_action = actions.last().expect("Output is not empty");
            match self.sub_calc_next_pos(last_action, &res_phrase, phrase, last_syll_len_change) {
                Some(np) => *next = np,
                None => *next_pos = None,
            }
        }

        Ok(res_phrase)
    }
    
    fn sub_structure(&self, phrase: &Phrase, items: &[ParseItem], stress: &Option<SpecMod>, tone: &Option<Tone>, refr: &Option<usize>, state: MatchElement, in_pos: Position, out_pos: Position) -> Result<SubAction, RuleRuntimeError> {
        if items.is_empty() { return Err(RuleRuntimeError::SubstitutionSyll(out_pos)) }
        match state {
            MatchElement::Syllable(wp, sp, _) => {
                Ok(SubAction {
                    kind: ActionKind::ReplaceSyllable(self.gen_syll_from_struct(items, stress, tone, refr, out_pos, false)?),
                    pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 },
                })
            },
            MatchElement::Segment(pos, _) => {
                Ok(SubAction {
                    kind: ActionKind::ReplaceSegment(
                        (Self::ONE, Self::ONE), 
                        Payload::Syllable(self.gen_syll_from_struct(items, stress, tone, refr, out_pos, false)?),
                        out_pos
                    ),
                    pos,
                })
            },
            MatchElement::LongSegment(pos, _) => {
                Ok(SubAction {
                    kind: ActionKind::ReplaceSegment(
                        (Self::non_zero_len(phrase.seg_length_at(pos) as u8), Self::ONE),
                        Payload::Syllable(self.gen_syll_from_struct(items, stress, tone, refr, out_pos, false)?),
                        out_pos
                    ),
                    pos,
                })
            },
            MatchElement::WordBound(..) => Err(RuleRuntimeError::SubstitutionWordBound(in_pos, out_pos)),
            MatchElement::SyllBound(..) => unreachable!("Should be handled in gen_actions()"),
        }
    }

    fn sub_matrix(&self, phrase: &Phrase, mods: &Modifiers, refr: &Option<usize>, state: MatchElement, in_pos: Position, out_pos: Position) -> Result<SubAction, RuleRuntimeError> {
        // SAFETY: pos points to a valid index
        match state {
            MatchElement::LongSegment(pos, _) => {
                let old_len = phrase.seg_length_at(pos) as u8;
                let (seg, new_len) = self.gen_seg(unsafe { phrase.get_seg_at(pos).unwrap_unchecked() }, old_len,Some(mods), refr, out_pos)?;
                Ok(SubAction {
                    // SAFETY: old_len > 0
                    kind: ActionKind::ReplaceSegment(
                        (Self::non_zero_len(old_len), Self::non_zero_len(new_len)),
                        Payload::Segment(seg, Some(mods.suprs)),
                        out_pos,
                    ),
                    pos,
                })
            },
            MatchElement::Segment(pos, _) => {
                let (seg, new_len) = self.gen_seg(unsafe { phrase.get_seg_at(pos).unwrap_unchecked() }, 1,Some(mods), refr, out_pos)?;
                Ok(SubAction {
                    kind: ActionKind::ReplaceSegment(
                        (Self::ONE, Self::non_zero_len(new_len)), 
                        Payload::Segment(seg, Some(mods.suprs)),
                        out_pos,
                    ),
                    pos,
                })
            },
            MatchElement::Syllable(wp, sp, _)  => {
                let mut syll = phrase[wp].syllables[sp].clone();
                syll.apply_syll_mods(&self.alphas, &mods.suprs, out_pos)?;
                if let Some(r) = refr {
                    self.references.borrow_mut().insert(*r, RefKind::Syllable(syll.clone()));
                }
                Ok(SubAction {
                    kind: ActionKind::ReplaceSyllable(syll),
                    pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 },
                })
            },
            MatchElement::WordBound(..) => Err(RuleRuntimeError::SubstitutionWordBound(in_pos, out_pos)),
            // This shouldn't occur
            MatchElement::SyllBound(..)  => Err(RuleRuntimeError::SubstitutionBoundMod(in_pos, out_pos)),
        }
    }

    fn sub_ipa(&self, phrase: &Phrase, seg: &Segment, mods: &Option<Modifiers>, state: MatchElement, in_pos: Position, out_pos: Position) -> Result<SubAction, RuleRuntimeError> {
        match state {
            MatchElement::LongSegment(pos, _) => {
                let suprs = mods.as_ref().map(|m| m.suprs);
                let out_has_length = if let Some(s) = suprs { s.length.is_some() } else { false };

                let old_len = phrase.seg_length_at(pos) as u8;
                let (seg, new_len) = self.gen_seg(*seg, if out_has_length { old_len } else { 1 }, mods.as_ref(), &None, out_pos)?;

                Ok(SubAction {
                    kind: ActionKind::ReplaceSegment(
                        (Self::non_zero_len(old_len), Self::non_zero_len(new_len)), 
                        Payload::Segment(seg, suprs),
                        out_pos
                    ),
                    pos,
                })
            },
            MatchElement::Segment(pos, _)=> {
                let (seg, new_len) = self.gen_seg(*seg, 1,  mods.as_ref(), &None, out_pos)?;
                let suprs = mods.as_ref().map(|m| m.suprs);

                Ok(SubAction {
                    kind: ActionKind::ReplaceSegment(
                        (Self::ONE, Self::non_zero_len(new_len)), 
                        Payload::Segment(seg, suprs),
                        out_pos
                    ),
                    pos,
                })
            },
            MatchElement::WordBound(..) => Err(RuleRuntimeError::SubstitutionWordBound(in_pos, out_pos)),
            MatchElement::Syllable(..)  => Err(RuleRuntimeError::SubstitutionSylltoMatrix(in_pos, out_pos)),
            MatchElement::SyllBound(..) => unreachable!("Should be handled in gen_actions()"),
        }
    }

    fn sub_ref(&self, phrase: &Phrase, num: &Reference, mods: &Option<Modifiers>, state: MatchElement, in_pos: Position, out_pos: Position) -> Result<SubAction, RuleRuntimeError> {
        let binding = self.references.borrow();
        let Some(refr) = binding.get(&num.value) else { return Err(RuleRuntimeError::UnknownReference(*num)) };
        match (state, refr) {
            (MatchElement::LongSegment(pos, _), RefKind::Segment(seg)) => {
                let old_len = phrase.seg_length_at(pos) as u8;
                let (seg, new_len) = self.gen_seg(*seg, old_len, mods.as_ref(), &None, num.position)?;
                let suprs = mods.as_ref().map(|m| m.suprs);

                Ok(SubAction {
                    kind: ActionKind::ReplaceSegment(
                        (Self::non_zero_len(old_len), Self::non_zero_len(new_len)),
                        Payload::Segment(seg, suprs),
                        num.position
                    ),
                    pos,
                })
            },
            (MatchElement::Segment(pos, _), RefKind::Segment(seg)) => {
                // TODO: Pass 1 here instead of the actual seg length may cause some issues, need to test once implemented
                let (seg, new_len) = self.gen_seg(*seg, 1, mods.as_ref(), &None, num.position)?;
                let suprs = mods.as_ref().map(|m| m.suprs);

                Ok(SubAction {
                    kind: ActionKind::ReplaceSegment(
                        (Self::ONE, Self::non_zero_len(new_len)), 
                        Payload::Segment(seg, suprs), 
                        num.position
                    ),
                    pos,
                })
            },
            (MatchElement::LongSegment(pos, _), RefKind::Syllable(insert_syll)) => {
                let mut syll = insert_syll.clone();
                let seg_len = phrase.seg_length_at(pos) as u8;
                if let Some(m) = mods { syll.apply_syll_mods(&self.alphas, &m.suprs, num.position)?; }

                Ok(SubAction {
                    kind: ActionKind::ReplaceSegment(
                        (Self::non_zero_len(seg_len), Self::ONE),
                        Payload::Syllable(syll),
                        num.position
                    ),
                    pos,
                })
            },
            (MatchElement::Segment(pos, _), RefKind::Syllable(insert_syll)) => {
                let mut syll = insert_syll.clone();

                if let Some(m) = mods { syll.apply_syll_mods(&self.alphas, &m.suprs, num.position)?; }

                Ok(SubAction {
                    kind: ActionKind::ReplaceSegment(
                        (Self::ONE, Self::ONE), 
                        Payload::Syllable(syll),
                        num.position
                    ),
                    pos,
                })
            },
            (MatchElement::Syllable(wp, sp, _), RefKind::Syllable(syll)) => {
                let mut syll = syll.clone();
                if let Some(m) = mods {
                    syll.apply_syll_mods(&self.alphas, &m.suprs, num.position)?;
                }

                Ok(SubAction {
                    kind: ActionKind::ReplaceSyllable(syll),
                    pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 },
                })
            },
            (MatchElement::Syllable(..),  RefKind::Segment(..)) => Err(RuleRuntimeError::SubstitutionSylltoSeg(in_pos, out_pos)),
            (MatchElement::WordBound(_), _) => Err(RuleRuntimeError::SubstitutionWordBound(in_pos, out_pos)),

            (MatchElement::SyllBound(..), ..) => unreachable!("Should be handled in gen_actions()"),
        }
    }

    fn sub_set(&self, phrase: &Phrase, set_output: &[ParseItem], state: MatchElement, in_state: &ParseItem, out_pos: Position) -> Result<Vec<SubAction>, RuleRuntimeError> {
        let ParseElement::Set(set_input) = &in_state.kind else { return Err(RuleRuntimeError::LonelySet(out_pos)) };
        if set_input.len() != set_output.len() { return Err(RuleRuntimeError::UnevenSet(in_state.position, out_pos)) }

        match state {
            MatchElement::WordBound(..) => Err(RuleRuntimeError::SubstitutionWordBound(in_state.position, out_pos)),
            MatchElement::LongSegment(mut pos, set_index) => {
                let Some(i) = set_index else { return Err(RuleRuntimeError::LonelySet(in_state.position)) };
                match &set_output[i].kind {
                    ParseElement::Ipa(seg, mods) => {
                        let old_len = phrase.seg_length_at(pos) as u8; 
                        let (seg, new_len) = self.gen_seg(*seg, old_len, mods.as_ref(), &None, out_pos)?;
                        let suprs = mods.as_ref().map(|m| m.suprs);

                        Ok(vec![SubAction {
                            kind: ActionKind::ReplaceSegment(
                                (Self::non_zero_len(old_len), Self::non_zero_len(new_len)),
                                Payload::Segment(seg, suprs),
                                out_pos
                            ),
                            pos,
                        }])
                    },
                    ParseElement::Matrix(mods, refr) => {
                        let old_len = phrase.seg_length_at(pos) as u8;
                        // SAFETY: pos points to a valid index
                        let (seg, new_len)  = self.gen_seg(unsafe { phrase.get_seg_at(pos).unwrap_unchecked() }, old_len,Some(mods), refr, set_output[i].position)?;
                        Ok(vec![SubAction {
                            kind: ActionKind::ReplaceSegment(
                                (Self::non_zero_len(old_len), Self::non_zero_len(new_len)),
                                Payload::Segment(seg, Some(mods.suprs)),
                                set_output[i].position
                            ),
                            pos,
                        }])
                    },
                    ParseElement::Reference(num, mods) => {
                        let binding = self.references.borrow();
                        let Some(refr) = binding.get(&num.value) else {
                            return Err(RuleRuntimeError::UnknownReference(*num))
                        };

                        let old_len = phrase.seg_length_at(pos) as u8; 


                        match refr {
                            RefKind::Segment(seg) => {
                                let (seg, new_len) = self.gen_seg(*seg, 1, mods.as_ref(), &None, num.position)?;
                                let suprs = mods.as_ref().map(|m| m.suprs);

                                Ok(vec![SubAction {
                                    kind: ActionKind::ReplaceSegment(
                                        (Self::non_zero_len(old_len), Self::non_zero_len(new_len)), 
                                        Payload::Segment(seg, suprs),
                                        num.position,
                                    ),
                                    pos,
                                }])
                            },
                            RefKind::Syllable(insert_syll) => {
                                let mut syll = insert_syll.clone();
                                if let Some(m) = mods { syll.apply_syll_mods(&self.alphas, &m.suprs, num.position)?; }

                                Ok(vec![SubAction {
                                    kind: ActionKind::ReplaceSegment(
                                        (Self::non_zero_len(old_len), Self::ONE),
                                        Payload::Syllable(syll),
                                        num.position,
                                    ),
                                    pos,
                                }])
                            },
                        }
                    },
                    ParseElement::Structure(items, stress, tone, refr) => {
                        if items.is_empty() { return Err(RuleRuntimeError::SubstitutionSyll(out_pos)) }
                        Ok(vec![SubAction {
                            kind: ActionKind::ReplaceSegment(
                                (Self::non_zero_len(phrase.seg_length_at(pos) as u8), Self::ONE),
                                Payload::Syllable(self.gen_syll_from_struct(items, stress, tone, refr, out_pos, false)?),
                                out_pos
                            ),
                            pos,
                        }])
                    },
                    
                    ParseElement::SyllBound => {
                        let mut v = Vec::with_capacity(2);
                        v.push(SubAction {
                            kind: ActionKind::DeleteSegment(Self::non_zero_len(phrase.seg_length_at(pos) as u8)),
                            pos,
                        });
                        pos.seg_index += phrase.seg_length_at(pos);
                        v.push(SubAction {
                            kind: ActionKind::InsertBoundary,
                            pos,
                        });
                        Ok(v)
                    },

                    ParseElement::Syllable(..) => Err(RuleRuntimeError::SubstitutionSyll(set_output[i].position)),
                    ParseElement::WordBound => Err(RuleRuntimeError::WordBoundSetLocError(set_output[i].position)),

                    ParseElement::EmptySet | ParseElement::ExtlBound  | 
                    ParseElement::Ellipsis | ParseElement::Metathesis | 
                    ParseElement::Set(..)  | ParseElement::OptEllipsis  | 
                    ParseElement::Optional(..) => unreachable!()
                }
            },
            MatchElement::Segment(mut pos, set_index) => {
                let Some(i) = set_index else { return Err(RuleRuntimeError::LonelySet(in_state.position)) };
                match &set_output[i].kind {
                    ParseElement::Ipa(seg, mods) => {
                        let (seg, new_len) = self.gen_seg(*seg, 1, mods.as_ref(), &None, out_pos)?;
                        let suprs = mods.as_ref().map(|m| m.suprs);

                        Ok(vec![SubAction {
                            kind: ActionKind::ReplaceSegment(
                                (Self::ONE, Self::non_zero_len(new_len)), 
                                Payload::Segment(seg, suprs),
                                out_pos
                        ),
                            pos,
                        }])
                    }
                    ParseElement::Matrix(mods, refr) => {
                        // SAFETY: pos points to a valid index
                        let (seg, new_len)  = self.gen_seg(unsafe { phrase.get_seg_at(pos).unwrap_unchecked() }, 1, Some(mods), refr, set_output[i].position)?;
                        Ok(vec![SubAction {
                            kind: ActionKind::ReplaceSegment(
                                (Self::ONE, Self::non_zero_len(new_len)), 
                                Payload::Segment(seg, Some(mods.suprs)),
                                set_output[i].position,
                            ),
                            pos,
                        }])
                    },
                    ParseElement::Reference(num, mods) => { 
                        let binding = self.references.borrow();
                        let Some(refr) = binding.get(&num.value) else {
                            return Err(RuleRuntimeError::UnknownReference(*num))
                        };

                        match refr {
                            RefKind::Segment(seg) => {
                                let (seg, new_len) = self.gen_seg(*seg, 1, mods.as_ref(), &None, num.position)?;
                                let suprs = mods.as_ref().map(|m| m.suprs);

                                Ok(vec![SubAction {
                                    kind: ActionKind::ReplaceSegment(
                                        (Self::ONE, Self::non_zero_len(new_len)), 
                                        Payload::Segment(seg, suprs),
                                        num.position,
                                    ),
                                    pos,
                                }])
                            },
                            RefKind::Syllable(insert_syll) => {
                                let mut syll = insert_syll.clone();
                                if let Some(m) = mods { syll.apply_syll_mods(&self.alphas, &m.suprs, num.position)?; }

                                Ok(vec![SubAction {
                                    kind: ActionKind::ReplaceSegment(
                                        (Self::ONE, Self::ONE), 
                                        Payload::Syllable(syll),
                                        num.position,
                                    ),
                                    pos,
                                }])
                            },
                        }
                    },
                    ParseElement::Structure(items, stress, tone, refr) => {
                        if items.is_empty() { return Err(RuleRuntimeError::SubstitutionSyll(out_pos)) }
                        Ok(vec![SubAction {
                            kind: ActionKind::ReplaceSegment(
                                (Self::ONE, Self::ONE), 
                                Payload::Syllable(self.gen_syll_from_struct(items, stress, tone, refr, out_pos, false)?),
                                out_pos
                            ),
                            pos,
                        }])
                    },

                    ParseElement::SyllBound => {
                        let mut v = Vec::with_capacity(2);
                        v.push(SubAction {
                            kind: ActionKind::DeleteSegment(Self::ONE),
                            pos,
                        });
                        pos.seg_index += 1;
                        v.push(SubAction {
                            kind: ActionKind::InsertBoundary,
                            pos,
                        });

                        Ok(v)
                    },
                    ParseElement::Syllable(..) => Err(RuleRuntimeError::SubstitutionSyll(set_output[i].position)),
                    ParseElement::WordBound => Err(RuleRuntimeError::WordBoundSetLocError(set_output[i].position)),
                    
                    ParseElement::EmptySet | ParseElement::ExtlBound   | 
                    ParseElement::Ellipsis | ParseElement::Metathesis  | 
                    ParseElement::Set(..)  | ParseElement::OptEllipsis | 
                    ParseElement::Optional(..) => unreachable!()
                }
            },
            MatchElement::Syllable(wp, sp, set_index) => {
                let Some(i) = set_index else { return Err(RuleRuntimeError::LonelySet(in_state.position)) };

                match &set_output[i].kind {
                    ParseElement::Ipa(..) => Err(RuleRuntimeError::SubstitutionSylltoMatrix(in_state.position, set_output[i].position)),
                    ParseElement::SyllBound => Err(RuleRuntimeError::SubstitutionSylltoBound(in_state.position, set_output[i].position)),
                    ParseElement::WordBound => Err(RuleRuntimeError::WordBoundSetLocError(set_output[i].position)),

                    ParseElement::Matrix(mods, refr) => {
                        let mut syll = phrase[wp].syllables[sp].clone();
                        syll.apply_syll_mods(&self.alphas, &mods.suprs, set_output[i].position)?;
                        if let Some(r) = refr { self.references.borrow_mut().insert(*r, RefKind::Syllable(syll.clone())); }
                        // FIXME: This is wasteful as we are discarding the changed syllable to later apply the same changes
                        Ok(vec![SubAction {
                            kind: ActionKind::ModifySyllable(mods.suprs, set_output[i].position),
                            pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 },
                        }])
                    }
                    ParseElement::Syllable(stress, tone, refr) => { 
                        let mut syll = phrase[wp].syllables[sp].clone();
                        let sups = SupraSegs::from(*stress, None, *tone);
                        syll.apply_syll_mods(&self.alphas, &sups, set_output[i].position)?;
                        if let Some(r) = refr { self.references.borrow_mut().insert(*r, RefKind::Syllable(syll.clone())); }
                        // FIXME: This is wasteful as we are discarding the changed syllable to later apply the same changes
                        Ok(vec![SubAction {
                            kind: ActionKind::ModifySyllable(sups, set_output[i].position),
                            pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 },
                        }])
                    }
                    ParseElement::Reference(num, mods) => {
                        let binding = self.references.borrow();
                        let Some(refr) = binding.get(&num.value) else {
                            return Err(RuleRuntimeError::UnknownReference(*num))
                        };

                        match refr {
                            RefKind::Segment(_) => Err(RuleRuntimeError::SubstitutionSylltoMatrix(in_state.position, num.position)),
                            RefKind::Syllable(syll) => {
                                let mut syll = syll.clone();

                                if let Some(m) = mods {
                                    syll.apply_syll_mods(&self.alphas, &m.suprs, num.position)?;
                                }
                                Ok(vec![SubAction {
                                    kind: ActionKind::ReplaceSyllable(syll),
                                    pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 },
                                }])
                            },
                        }
                    },
                    ParseElement::Structure(items, stress, tone, refr) => {
                        if items.is_empty() { return Err(RuleRuntimeError::SubstitutionSyll(out_pos)) }
                        Ok(vec![SubAction {
                            kind: ActionKind::ReplaceSyllable(self.gen_syll_from_struct(items, stress, tone, refr, out_pos, false)?),
                            pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 },
                        }])
                    },

                    ParseElement::EmptySet | ParseElement::ExtlBound   | 
                    ParseElement::Ellipsis | ParseElement::Metathesis  | 
                    ParseElement::Set(..)  | ParseElement::OptEllipsis | 
                    ParseElement::Optional(..) => unreachable!()

                }
            },
            MatchElement::SyllBound(wp, sp, set_index) => {
                let Some(i) = set_index else { return Err(RuleRuntimeError::LonelySet(in_state.position)) };

                match &set_output[i].kind {
                    ParseElement::WordBound => Err(RuleRuntimeError::WordBoundSetLocError(set_output[i].position)),
                    ParseElement::Syllable(..) => Err(RuleRuntimeError::SubstitutionSyll(set_output[i].position)),
                    ParseElement::Matrix(..) => Err(RuleRuntimeError::SubstitutionBoundMod(in_state.position, set_output[i].position)),

                    ParseElement::SyllBound => Ok(vec![SubAction {
                        kind: ActionKind::PassBoundary,
                        pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 },
                    }]),
                    
                    ParseElement::Ipa(segment, mods) => {
                        let mut v = Vec::with_capacity(2);
                        v.push(SubAction {
                            kind: ActionKind::DeleteBoundary,
                            pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 }
                        });
                        let (seg, len) = self.gen_seg(*segment, 1, mods.as_ref(), &None, in_state.position)?;
                        let suprs = mods.as_ref().map(|m| m.suprs);

                        v.push(SubAction {
                            kind: ActionKind::InsertSegment(Self::non_zero_len(len), seg, suprs, in_state.position),
                            pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 }
                        });

                        Ok(v)
                    }
                    ParseElement::Structure(items, stress, tone, refr) => {
                        if items.is_empty() { return Err(RuleRuntimeError::SubstitutionSyll(out_pos)) }
                        let syll = self.gen_syll_from_struct(items, stress, tone, refr, out_pos, false)?;

                        Ok(vec![SubAction { 
                            kind: ActionKind::InsertSyllable(syll),
                            pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 }
                        }])
                    }
                    ParseElement::Reference(num, mods) => {
                        let binding = self.references.borrow();
                        let Some(refr) = binding.get(&num.value) else { return Err(RuleRuntimeError::UnknownReference(*num)) };
                        match refr {
                            RefKind::Segment(segment) => {
                                let mut v = Vec::with_capacity(2);
                                v.push(SubAction {
                                    kind: ActionKind::DeleteBoundary,
                                    pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 }
                                });

                                let (seg, len) = self.gen_seg(*segment, 1, mods.as_ref(), &None, in_state.position)?;
                                let suprs = mods.as_ref().map(|m| m.suprs);

                                v.push(SubAction {
                                    kind: ActionKind::InsertSegment(Self::non_zero_len(len), seg, suprs, in_state.position),
                                    pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 }
                                });

                                Ok(v)
                            }
                            RefKind::Syllable(insert_syll) => {
                                let mut syll = insert_syll.clone();
                                if let Some(m) = mods { syll.apply_syll_mods(&self.alphas, &m.suprs, num.position)?; }
                                Ok(vec![SubAction { 
                                    kind: ActionKind::InsertSyllable(syll.clone()),
                                    pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 }
                                }])
                            },
                        }
                    }

                    ParseElement::EmptySet | ParseElement::ExtlBound   | 
                    ParseElement::Ellipsis | ParseElement::Metathesis  | 
                    ParseElement::Set(..)  | ParseElement::OptEllipsis | 
                    ParseElement::Optional(..) => unreachable!()
                }
            },
        }
    }

    fn sub_bound(&self, phrase: &Phrase, state: MatchElement) -> Result<Vec<SubAction>, RuleRuntimeError> {
        match state {
            MatchElement::SyllBound(wp, sp, _) => {
                Ok(vec![SubAction { 
                    kind: ActionKind::PassBoundary, 
                    pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 } 
                }])
            },
            MatchElement::Segment(mut pos, _) => {
                let mut v = Vec::with_capacity(2);
                v.push(SubAction {
                    kind: ActionKind::DeleteSegment(Self::ONE),
                    pos,
                });
                pos.seg_index += 1;
                v.push(SubAction {
                    kind: ActionKind::InsertBoundary,
                    pos,
                });

                Ok(v)
            },
            MatchElement::LongSegment(mut pos, _) => {
                let mut v = Vec::with_capacity(2);
                v.push(SubAction {
                    kind: ActionKind::DeleteSegment(Self::non_zero_len(phrase.seg_length_at(pos) as u8)),
                    pos,
                });
                pos.seg_index += phrase.seg_length_at(pos);
                v.push(SubAction {
                    kind: ActionKind::InsertBoundary,
                    pos,
                });

                Ok(v)
            },
            MatchElement::Syllable(wp, sp, _) => {
                Ok(vec![SubAction { 
                    kind: ActionKind::DeleteSyllable,
                    pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 },
                }])
            },
            MatchElement::WordBound(_) => unreachable!("Should be dealt with in gen_actions()"),
        }
    }

    fn gen_seg(&self, seg: Segment, old_len: u8, mods: Option<&Modifiers>, refr: &Option<usize>, err_pos: Position) -> Result<(Segment, u8), RuleRuntimeError> {
        let mut seg = seg;
        let mut new_len = old_len;
        if let Some(m) = mods {
            seg.apply_seg_mods(&self.alphas, m.nodes, m.feats, err_pos, false)?;
            new_len = Syllable::calc_new_length(&self.alphas, &m.suprs, old_len, err_pos)?;
        }

        if let Some(r) = refr {
            self.references.borrow_mut().insert(*r, RefKind::Segment(seg));
        }

        Ok((seg, new_len))
    }

    fn sub_set_insertion_position(&self, insert_pos: &mut Option<SegPos>, actions: &[SubAction]) {
        if insert_pos.is_none() {
            let last_action = actions.last().expect("not first item");
            let SegPos { word_index, syll_index, seg_index } = last_action.pos;
            
            *insert_pos = Some(match &last_action.kind {
                ActionKind::DeleteSyllable     |
                ActionKind::ModifySyllable(..) |
                ActionKind::ReplaceSyllable(..) => SegPos { word_index, syll_index: syll_index+1, seg_index: 0 },
                
                ActionKind::DeleteSegment(old_len) |
                ActionKind::ReplaceSegment((old_len, _), ..) => SegPos { word_index, syll_index, seg_index: seg_index + old_len.get() as usize },
                
                ActionKind::PassBoundary => last_action.pos,
                ActionKind::DeleteBoundary => last_action.pos,

                ActionKind::InsertBoundary => last_action.pos,
                ActionKind::InsertSyllable(..) |
                ActionKind::InsertSegment(..) => last_action.pos,
            });
        }
    }

    fn substitution_gen_actions(&self, phrase: &Phrase, input_filt: &[ParseItem], output_filt: &[ParseItem], input: &[MatchElement]) -> Result<Vec<SubAction>, RuleRuntimeError> {
        // FIXME: This shouldn't be happening
        if input_filt.len() > input.len() || input_filt.is_empty() || input.is_empty() {
            // Should error if input_filt is empty as this means we are inserting between empty ellipses e.g. p.. ..p > p..p..p
            return Ok(vec![])
        }

        let mut actions = Vec::with_capacity(input_filt.len());
        let mut insert_pos: Option<SegPos> = None;

        let mut in_index = 0;
        let mut out_index = 0;

        loop {
            match (input_filt.get(in_index), output_filt.get(out_index)) {
                (None, None) => break,
                (None, Some(out_item)) => {
                    self.sub_set_insertion_position(&mut insert_pos, &actions);
                
                    match &out_item.kind {
                        ParseElement::Ellipsis  | ParseElement::OptEllipsis | ParseElement::Metathesis |
                        ParseElement::EmptySet  | ParseElement::WordBound | ParseElement::Optional(..) | 
                        ParseElement::ExtlBound => unreachable!(),

                        ParseElement::Syllable(..) => return Err(RuleRuntimeError::SubstitutionSyll(out_item.position)),
                        ParseElement::Set(_) => return Err(RuleRuntimeError::LonelySet(out_item.position)),
                        ParseElement::Matrix(..) => return Err(RuleRuntimeError::InsertionMatrix(out_item.position)),

                        ParseElement::Ipa(segment, mods) => {
                            let pos = insert_pos.expect("insert_pos is set");
                            let (seg, len) = self.gen_seg(*segment, 1, mods.as_ref(), &None, out_item.position)?;
                            let suprs = mods.as_ref().map(|m| m.suprs);

                            actions.push(SubAction { 
                                kind: ActionKind::InsertSegment(Self::non_zero_len(len), seg, suprs, out_item.position), 
                                pos
                            });
                            out_index += 1;
                        }
                        ParseElement::Structure(items, stress, tone, refr) => {
                            if items.is_empty() { return Err(RuleRuntimeError::SubstitutionSyll(out_item.position)) }
                            let pos = insert_pos.expect("insert_pos is set");
                            let syll = self.gen_syll_from_struct(items, stress, tone, refr, out_item.position, true)?;

                            actions.push(SubAction { 
                                kind: ActionKind::InsertSyllable(syll), 
                                pos
                            });
                            out_index += 1;
                        }
                        ParseElement::Reference(num, mods) => {
                            let binding = self.references.borrow();
                            let Some(refr) = binding.get(&num.value) else { return Err(RuleRuntimeError::UnknownReference(*num)) };
                            match refr {
                                RefKind::Segment(segment) => {
                                    let pos = insert_pos.expect("insert_pos is set");
                                    let (seg, len) = self.gen_seg(*segment, 1, mods.as_ref(), &None, num.position)?;
                                    let suprs = mods.as_ref().map(|m| m.suprs);

                                    actions.push(SubAction {
                                        kind: ActionKind::InsertSegment(Self::non_zero_len(len), seg, suprs, num.position), 
                                        pos,
                                    });
                                },
                                RefKind::Syllable(insert_syll) => {
                                    let pos = insert_pos.expect("insert_pos is set");
                                    let mut syll = insert_syll.clone();
                                    if let Some(m) = mods { syll.apply_syll_mods(&self.alphas, &m.suprs, num.position)?; }

                                    actions.push(SubAction {
                                        kind: ActionKind::InsertSyllable(syll), 
                                        pos,
                                    });

                                },
                            }
                            out_index += 1;
                        }
                        ParseElement::SyllBound => {
                            let pos = insert_pos.unwrap_or(actions.last().unwrap().pos);
                            actions.push(SubAction {
                                kind: ActionKind::InsertBoundary,
                                pos,
                            });
                            out_index += 1;
                        }
                    }
                },
                (Some(_), None) => match input[in_index] {
                    MatchElement::Segment(pos, _) | MatchElement::LongSegment(pos, _) => {
                        let seg_len = phrase.seg_length_at(pos) as u8;
                        actions.push(SubAction { kind: ActionKind::DeleteSegment(Self::non_zero_len(seg_len)), pos });
                        in_index += 1;
                    }
                    MatchElement::Syllable(wp, sp, _)  => {
                        actions.push(SubAction { kind: ActionKind::DeleteSyllable, pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 } });
                        in_index += 1;
                    }
                    MatchElement::SyllBound(wp, sp, _) => {
                        actions.push(SubAction { kind: ActionKind::DeleteBoundary, pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 } });
                        in_index += 1;
                    }
                    MatchElement::WordBound(_) => return Err(RuleRuntimeError::SubstitutionWordBound(input_filt[in_index].position, output_filt.last().expect("Output isn't empty").position)),
                },
                (Some(_), Some(out_item)) => {
                    let in_item = &input_filt[in_index];
                    let match_el = input[in_index];
                    match (match_el, &out_item.kind) {
                        (_, ParseElement::Ellipsis ) | (_, ParseElement::OptEllipsis)  | (_, ParseElement::Metathesis) | (_, ParseElement::ExtlBound) | 
                        (_, ParseElement::WordBound) | (_, ParseElement::Optional(..)) | (_, ParseElement::EmptySet  ) => unreachable!(),

                        (_, ParseElement::Syllable(..)) => return Err(RuleRuntimeError::SubstitutionSyll(out_item.position)),
                        (MatchElement::WordBound(..), _) => return Err(RuleRuntimeError::SubstitutionWordBound(in_item.position, out_item.position)),

                        
                        (_, ParseElement::SyllBound) => {
                            actions.extend(self.sub_bound(phrase, match_el)?);
                            in_index += 1; out_index += 1;
                        }
                        // Note: Must be placed after sub_bound and before everything else
                        (MatchElement::SyllBound(.., Some(_)), ParseElement::Set(set_output)) => {
                            actions.extend(self.sub_set(phrase, set_output, match_el, in_item, out_item.position)?);
                            in_index += 1; out_index += 1;
                        }
                        // Note: Must be placed after sub_bound and before everything else
                        (MatchElement::SyllBound(wp, sp, _), _) => {
                            actions.push(SubAction { 
                                kind: ActionKind::DeleteBoundary, 
                                pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 },
                            });
                            in_index += 1;
                        }
                        (_, ParseElement::Structure(items, stress, tone, refr)) => {
                            actions.push(self.sub_structure(phrase, items, stress, tone, refr, match_el, in_item.position, out_item.position)?);
                            in_index += 1; out_index += 1;
                        }
                        (_, ParseElement::Matrix(mods, refr)) => { 
                            actions.push(self.sub_matrix(phrase, mods, refr, match_el, in_item.position, out_item.position)?);
                            in_index += 1; out_index += 1;
                        }
                        (_, ParseElement::Ipa(seg, mods) )=> {
                            actions.push(self.sub_ipa(phrase, seg, mods, match_el, in_item.position, out_item.position)?);
                            in_index += 1; out_index += 1;
                        }
                        (_, ParseElement::Reference(num, mods)) => {
                            actions.push(self.sub_ref(phrase, num, mods, match_el, in_item.position, out_item.position)?);
                            in_index += 1; out_index += 1;
                        }
                        (_, ParseElement::Set(set_output)) => {
                            actions.extend(self.sub_set(phrase, set_output, match_el, in_item, out_item.position)?);
                            in_index += 1; out_index += 1;
                        }
                    }
                }
            }
        }
        
        Ok(actions)
    }

    fn apply_sub_actions(&self, phrase: &Phrase, actions: &[SubAction]) -> Result<(Phrase, isize), RuleRuntimeError> {
        let mut res_phrase = phrase.clone();
        // this will work as long as we dont allow inserting/deleting `##` in substitution rules
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
                ActionKind::PassBoundary => continue,
                ActionKind::InsertSegment(seg_len, segment, mods, err_pos) => {
                    if let Some(syll) = res_phrase[action.pos.word_index].syllables.get_mut(action.pos.syll_index) {
                        for _ in 0..seg_len.get() {
                            syll.segments.insert(action.pos.seg_index, *segment);
                        }
                        if let Some(m) = mods {
                            syll.apply_syll_mods(&self.alphas, m, *err_pos)?;
                        }
                    } else {
                        let syll = res_phrase[action.pos.word_index].syllables.last_mut().expect("Not empty");
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
                            res_phrase[action.pos.word_index].syllables.push(insert_syll.clone());
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
                    if ((!self.inp_x_bound && !self.env_x_bound) || res_phrase.len() == 1) && res_phrase[action.pos.word_index].syllables.len() <= 1 {
                        return Err(RuleRuntimeError::DeletionOnlySyll)
                    }
                    res_phrase[action.pos.word_index].syllables.remove(action.pos.syll_index);
                    word_len_change[action.pos.word_index] -= 1;
                },
                ActionKind::InsertBoundary => {
                    // Break syllable into two at position
                    let mut new_syll = Syllable::new();
                    let syll = res_phrase[action.pos.word_index].syllables.get_mut(action.pos.syll_index).unwrap();

                    while syll.segments.len() > action.pos.seg_index {
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

                    word_len_change[action.pos.word_index] -= 1
                },
            }
        }

        for word in res_phrase.iter_mut() { word.syllables.retain(|s| !s.segments.is_empty()); }

        Ok((res_phrase, word_len_change[actions.last().expect("Output is not empty").pos.word_index]))
    }

    fn sub_calc_next_pos(&self, last_action: &SubAction, res_phrase: &Phrase, old_phrase: &Phrase, word_len_change: isize) -> Option<SegPos> {
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
                        match res_phrase[last_action.pos.word_index].syllables.iter().enumerate().position(|(i, s)| *s == *old_next_syll && i > last_action.pos.syll_index.saturating_add_signed(word_len_change)) {
                            Some(sp) => return Some(SegPos { word_index: last_action.pos.word_index, syll_index: sp, seg_index: 0 }),
                            None => unimplemented!("Word Boundary change"),
                        }
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
                    unimplemented!("Word Boundary change")
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
                    match res_phrase[last_action.pos.word_index].syllables.iter().enumerate().position(|(i, s)| *s == *old_next_syll && i > last_action.pos.syll_index.saturating_add_signed(word_len_change)) {
                        Some(sp) => return Some(SegPos { word_index: last_action.pos.word_index, syll_index: sp, seg_index: 0 }),
                        None => unimplemented!("Word Boundary change"),
                    }
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
                unimplemented!("Word Boundary change")
            },
            // These may not be right
            ActionKind::InsertSyllable(_) => {
                Some(SegPos{
                    word_index: last_action.pos.word_index,
                    syll_index: (last_action.pos.syll_index+1).saturating_add_signed(word_len_change),
                    seg_index: 0
                })
            },
            ActionKind::DeleteSyllable => {
                Some(SegPos{
                    word_index: last_action.pos.word_index,
                    syll_index: (last_action.pos.syll_index+1).saturating_add_signed(word_len_change),
                    seg_index: 0
                })
            },
            ActionKind::InsertBoundary => {
                Some(SegPos{
                    word_index: last_action.pos.word_index,
                    syll_index: (last_action.pos.syll_index).saturating_add_signed(word_len_change),
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

impl SubRule { // Context Matching
    fn context_match(&self, states: &[ParseItem], state_index: &mut usize, phrase: &Phrase, pos: &mut SegPos, forwards: bool, ins_match_before: bool, within_struct: bool) -> Result<bool, RuleRuntimeError> {
        let state = &states[*state_index];
        match &state.kind {
            ParseElement::SyllBound | ParseElement::WordBound | ParseElement::ExtlBound if within_struct => Err(RuleRuntimeError::BoundaryInsideStruct(state.position)),

            ParseElement::Structure(..) if within_struct => Err(RuleRuntimeError::StructInsideStruct(state.position)),
            ParseElement::Syllable (..) if within_struct => Err(RuleRuntimeError::SyllbleInsideStruct(state.position)),

            ParseElement::WordBound => Ok(phrase[pos.word_index].out_of_bounds(*pos)),
            ParseElement::SyllBound => if ins_match_before {
                Ok(!pos.at_word_start() && pos.at_syll_start())
            } else {
                Ok(pos.at_syll_start())
            },
            ParseElement::Ipa(s, m) => if self.context_match_ipa(s, m, phrase, *pos, state.position)? {
                self.matrix_increment(phrase, pos);
                pos.increment(phrase); 
                Ok(true)
            } else { Ok(false) },
            ParseElement::Matrix(m, v) => self.context_match_matrix(m, v, phrase, pos, state.position),
            ParseElement::Syllable(s, t, v) => if ins_match_before {
                Ok(!pos.at_word_start() && self.context_match_syll(s, t, v, phrase, pos, forwards, state.position)?)
            } else {
                self.context_match_syll(s, t, v, phrase, pos, forwards, state.position)
            },
            ParseElement::Structure(segs, stress, tone, refr) => if ins_match_before {
                Ok(!pos.at_word_start() && self.context_match_structure(segs, stress, tone, refr, phrase, pos, forwards, state.position)?)
            } else {
                self.context_match_structure(segs, stress, tone, refr, phrase, pos, forwards, state.position)
            },
            ParseElement::Reference(vt, mods) => self.context_match_ref(vt, mods, phrase, pos, forwards, state.position, within_struct),
            ParseElement::Set(s) => self.context_match_set(s, phrase, pos, forwards, within_struct),
            ParseElement::Optional(opt_states, min, max) => self.context_match_option(states, state_index, phrase, pos, forwards, opt_states, *min, *max, within_struct),
            ParseElement::Ellipsis => self.context_match_ellipsis(states, state_index, phrase, pos, forwards, true),
            ParseElement::OptEllipsis => self.context_match_ellipsis(states, state_index, phrase, pos, forwards, false),
            
            ParseElement::ExtlBound => if phrase[pos.word_index].out_of_bounds(*pos) && !pos.at_phrase_end(phrase) {
                pos.word_increment(phrase);
                Ok(true)
            } else { Ok(false) },

            ParseElement::EmptySet | ParseElement::Metathesis => unreachable!(),
        }
    }

    fn match_before_env(&self, states: &[ParseItem], phrase_rev: &Phrase, pos: &SegPos, ins_match_before: bool, inc: bool, is_context: bool) -> Result<bool, RuleRuntimeError> {
        // NOTE: assumes parent has done reversals
        let mut start_pos = *pos;
        if inc {
            start_pos.increment(phrase_rev);
        }
        let mut is_match = if is_context {
            true
        } else {
            !states.is_empty()
        };
        let mut si = 0;
        while si < states.len() {
            if !self.context_match(states, &mut si, phrase_rev, &mut start_pos, false, ins_match_before, false)? {
                is_match = false;
                if is_context { break; }
            }
            si += 1;
        }

        Ok(is_match)
    }

    fn match_after_env(&self, states: &[ParseItem], phrase: &Phrase, pos: &SegPos, ins_match_before: bool, inc: bool, is_context: bool) -> Result<bool, RuleRuntimeError> {
        let mut start_pos = *pos;
        if inc {
            start_pos.increment(phrase);
        }
        let mut is_match = if is_context {
            true
        } else {
            !states.is_empty()
        };
        let mut si = 0;
        while si < states.len() {
            if !self.context_match(states, &mut si, phrase, &mut start_pos, true, ins_match_before, false)? {
                is_match = false;
                if is_context { break; }
            }
            si += 1;
        }

        Ok(is_match)
    }

    fn match_underline_struct_items(&self, phrase: &Phrase, items: &[ParseItem], pos: SegPos, forwards: bool, ins_info: &mut Option<(usize, &mut SegPos)>) -> Result<bool, RuleRuntimeError> {
        let cur_syll_index = pos.syll_index;

        let mut pos = pos;

        for (mut i, item) in items.iter().enumerate() {
            if pos.syll_index != cur_syll_index && item.kind != ParseElement::OptEllipsis {
                return Ok(false)
            }
            match &item.kind {
                ParseElement::Ellipsis => if i == items.len() - 1 || self.context_match_ellipsis_struct(items, &mut i, phrase, &mut pos, cur_syll_index, forwards, true, ins_info)? {
                    break;
                } else { return Ok(false) }, 
                ParseElement::OptEllipsis => if i == items.len() - 1 || self.context_match_ellipsis_struct(items, &mut i, phrase, &mut pos, cur_syll_index, forwards,false, ins_info)? {
                    break;
                } else { return Ok(false) },
                ParseElement::Ipa(s, mods) => if self.context_match_ipa(s, mods, phrase, pos, item.position)? {
                    self.matrix_increment(phrase, &mut pos);
                    pos.increment(phrase);
                } else { return Ok(false) },
                ParseElement::Matrix(mods, refr) => if !self.context_match_matrix(mods, refr, phrase, &mut pos, item.position)? {
                    return Ok(false) 
                },
                // NOTE: since syllables are invalid, passing `forwards` won't matter
                ParseElement::Reference(num, mods) => if !self.context_match_ref(num, mods, phrase, &mut pos, forwards, item.position, true)? {
                   return Ok(false) 
                },
                ParseElement::Set(set) => if !self.context_match_set(set, phrase, &mut pos, forwards, true)? {
                    return Ok(false) 
                },
                ParseElement::Optional(states, min, max) => if self.context_match_option(items, &mut i, phrase, &mut pos, forwards, states, *min, *max, true)? {
                    break;
                } else { return Ok(false) },
                _ => unreachable!()
            }
            // TODO FIXME: This sucks, bef_len does not need to be mutable
            if let Some((bef_len, ins_pos)) = ins_info && *bef_len - 1 == i {
                **ins_pos = pos;
            }
        }
        Ok(true)
    }

    fn match_underline_struct(&self, phrase_rev: &Phrase, phrase: &Phrase, matches: &[MatchElement], start_pos: SegPos, end_pos: SegPos, center: &UnderlineStruct) -> Result<bool, RuleRuntimeError> {
        
        { // Sanity Check Input
            for (i, m) in matches.iter().enumerate() {
                match m {
                    MatchElement::Syllable (..) => {
                        if let Some(input_item) = self.input.get(i) {
                            return Err(RuleRuntimeError::SyllbleInsideUnderlineStruct(input_item.position, center.position))
                        } else {
                            return Err(RuleRuntimeError::SyllbleInsideStruct(center.position))
                        }
                    }
                    
                    MatchElement::SyllBound(..) | MatchElement::WordBound(..) => {
                        if let Some(input_item) = self.input.get(i) {
                            return Err(RuleRuntimeError::BoundaryInsideUnderlineStruct(input_item.position, center.position))
                        } else {
                            return Err(RuleRuntimeError::BoundaryInsideStruct(center.position))
                        }
                    }

                    MatchElement::LongSegment(..) | MatchElement::Segment(..) => {}
                }
            }
            // Check first and last input are in the same syllable
            if start_pos.word_index != end_pos.word_index || start_pos.syll_index != end_pos.syll_index { return Ok(false) }
        }

        // Check Suprasegmentals
        let syll = &phrase[start_pos.word_index].syllables[start_pos.syll_index];
        if let Some(tone) = center.tone && !self.match_tone(&tone, syll) { return Ok(false) }
        if !self.match_stress(&center.stress, syll, center.position)? { return Ok(false) }

        // Increment start_pos and end_pos before matching
        let cur_syll_index = start_pos.syll_index;
        let mut start_pos = start_pos.reversed(phrase);
        let cur_syll_index_rev = start_pos.syll_index;
        let mut end_pos = end_pos;
        start_pos.increment(phrase_rev);
        end_pos.increment(phrase);

        // Check we are not outside syllable (for an invalid reason)
        if start_pos.syll_index != cur_syll_index_rev && let Some(x) = center.before.first() {
            if let ParseElement::OptEllipsis | ParseElement::Optional(_,0,_) = x.kind {} else {
                return Ok(false)
            }
        }
        if end_pos.syll_index != cur_syll_index && let Some(x) = center.after.first() {
            if let ParseElement::OptEllipsis | ParseElement::Optional(_,0,_) = x.kind {} else {
                return Ok(false)
            }
        }

        if center.before.is_empty() && start_pos.syll_index == cur_syll_index {
            return Ok(false)
        }

        if center.after.is_empty() && end_pos.syll_index == cur_syll_index {
            return Ok(false)
        }
        
        Ok(
            self.match_underline_struct_items(phrase_rev, &center.before, start_pos, false, &mut None)? 
         && self.match_underline_struct_items(phrase, &center.after, end_pos, true, &mut None)?
        )
    }

    fn match_contexts_and_exceptions(&self, phrase: &Phrase, matches: &[MatchElement], start_pos: SegPos, end_pos: SegPos, inc_start:bool, inc_end: bool) -> Result<bool, RuleRuntimeError> {
        let contexts = self.get_contexts();
        let exceptions = self.get_exceptions();
        if contexts.is_empty() && exceptions.is_empty() {
            return Ok(true)
        }

        let phrase_rev = phrase.reversed();
        let mut is_cont_match = contexts.is_empty();
        let mut is_expt_match = false;

        for (bef_cont_states, center, aft_cont_states) in contexts {
            let (start_pos, end_pos) = if let Some(cent) = center {
                if self.match_underline_struct(&phrase_rev, phrase, matches, start_pos, end_pos, cent)? {
                    if start_pos.syll_index == 0 && !bef_cont_states.is_empty() { break }                                            // TODO: Check if this works with ##
                    if end_pos.syll_index == phrase[end_pos.word_index].syllables.len() - 1 && !aft_cont_states.is_empty() { break } // TODO: Check if this works with ##
                    let start_pos = SegPos::new(start_pos.word_index, start_pos.syll_index, 0);
                    let mut end_pos = SegPos::new(end_pos.word_index, end_pos.syll_index + 1, 0);
                    end_pos.decrement(phrase);
                    (start_pos, end_pos)
                } else { break }
            } else { (start_pos, end_pos) };

            let mut bef_cont_states = bef_cont_states.clone();
            bef_cont_states.reverse();
            if (bef_cont_states.is_empty() || self.match_before_env(&bef_cont_states, &phrase_rev, &start_pos.reversed(phrase), false, inc_start, true)?) 
            && (aft_cont_states.is_empty() || self.match_after_env(aft_cont_states, phrase, &end_pos, false, inc_end, true)?) {
                is_cont_match = true;
                break;
            }
        }
        for (bef_expt_states, center, aft_expt_states) in exceptions {
            let (start_pos, end_pos) = if let Some(cent) = center {
                if self.match_underline_struct(&phrase_rev, phrase, matches, start_pos, end_pos, cent)? {
                    if start_pos.syll_index == 0 && !bef_expt_states.is_empty() { break }                                            // TODO: Check if this works with ##
                    if end_pos.syll_index == phrase[end_pos.word_index].syllables.len() - 1 && !aft_expt_states.is_empty() { break } // TODO: Check if this works with ##
                    let start_pos = SegPos::new(start_pos.word_index, start_pos.syll_index, 0);
                    let mut end_pos = SegPos::new(end_pos.word_index, end_pos.syll_index + 1, 0);
                    end_pos.decrement(phrase);
                    (start_pos, end_pos)
                } else { break }
            } else { (start_pos, end_pos) };

            let mut bef_expt_states = bef_expt_states.clone();
            bef_expt_states.reverse();
            if (bef_expt_states.is_empty() || self.match_before_env(&bef_expt_states, &phrase_rev, &start_pos.reversed(phrase), false, inc_start,false)?) 
            && (aft_expt_states.is_empty() || self.match_after_env(aft_expt_states, phrase, &end_pos, false, inc_end, false)?) {
                is_expt_match = true;
                break;
            }
        }
        Ok(!is_expt_match && is_cont_match)
    }

    fn context_match_structure(&self, items: &[ParseItem], stress: &Option<SpecMod>, tone: &Option<Tone>, refr: &Option<usize>, phrase: &Phrase, pos: &mut SegPos, forwards: bool, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        if items.is_empty() {
            return self.context_match_syll(stress, tone, refr, phrase, pos, forwards, err_pos)
        }
        if !pos.at_syll_start() {
            return Ok(false)
        }
        let cur_syll = if phrase.in_bounds(*pos) { 
            &phrase[pos.word_index].syllables[pos.syll_index] 
        } else { return Ok(false) };
        if !self.match_stress(stress, cur_syll, err_pos)? {
            return Ok(false)
        }
        if let Some(t) = tone && !self.match_tone(t, cur_syll) {
            return Ok(false)
        }

        let mut items = items.to_vec().clone();
        if !forwards {
            items.reverse();
            for i in &mut items {
                i.reverse();
            }
        }

        let cur_word_index = pos.word_index;
        let cur_syll_index = pos.syll_index;

        for (mut i, item) in items.iter().enumerate() {
            if pos.syll_index != cur_syll_index && item.kind != ParseElement::OptEllipsis {
                return Ok(false)
            }
            match &item.kind {
                ParseElement::Ellipsis => if i == items.len() - 1 {
                    // if last item, jump to end of syll and break loop
                    pos.syll_index += 1;
                    pos.seg_index = 0;
                    break;
                } else if self.context_match_ellipsis_struct(&items, &mut i, phrase, pos, cur_syll_index, forwards, true, &mut None)? {
                    break;
                } else { return Ok(false) }, 
                ParseElement::OptEllipsis => if i == items.len() - 1 {
                    // if last item, jump to end of syll and break loop
                    pos.syll_index += 1;
                    pos.seg_index = 0;
                    break;
                } else if self.context_match_ellipsis_struct(&items, &mut i, phrase, pos, cur_syll_index, forwards,false, &mut None)? {
                    break;
                } else { return Ok(false) },
                ParseElement::Ipa(s, mods) => if self.context_match_ipa(s, mods, phrase, *pos, item.position)? {
                    self.matrix_increment(phrase, pos);
                    pos.increment(phrase);
                } else { return Ok(false) },
                ParseElement::Matrix(mods, refr) => if !self.context_match_matrix(mods, refr, phrase, pos, item.position)? {
                    return Ok(false) 
                },
                // NOTE: since syllables are invalid, passing `forwards` won't matter
                ParseElement::Reference(num, mods) => if !self.context_match_ref(num, mods, phrase, pos, forwards, item.position, true)? {
                   return Ok(false) 
                },
                ParseElement::Set(set) => if !self.context_match_set(set, phrase, pos, forwards, true)? {
                    return Ok(false) 
                },
                ParseElement::Optional(states, min, max) => if self.context_match_option(&items, &mut i, phrase, pos, forwards, states, *min, *max, true)? {
                    // This works i guess
                    if pos.syll_index == cur_syll_index {
                        pos.syll_index += 1;
                        pos.seg_index = 0;
                    }
                    debug_assert!(pos.syll_index == cur_syll_index+1);
                    debug_assert!(pos.seg_index == 0);
                    break;
                } else { return Ok(false) },
                _ => unreachable!()
            }
        }

        if pos.seg_index != 0 { return Ok(false) }

        if let Some(r) = refr {
            if forwards {
                self.references.borrow_mut().insert(*r, RefKind::Syllable(phrase[cur_word_index].syllables[cur_syll_index].clone()));
            } else {
                let mut syll = phrase[cur_word_index].syllables[cur_syll_index].clone();
                syll.segments.make_contiguous().reverse();
                self.references.borrow_mut().insert(*r, RefKind::Syllable(syll));
            }
        }

        Ok(true)
    }

    // TODO: more tests, I'm not sold that this is correct
    fn context_match_ellipsis_struct(&self, items: &[ParseItem], index: &mut usize, phrase: &Phrase, pos: &mut SegPos, syll_index: usize, forwards: bool, inc: bool, ins_info: &mut Option<(usize, &mut SegPos)>) -> Result<bool, RuleRuntimeError> {
        
        if inc { pos.increment(phrase) }
        // TODO FIXME: This sucks, bef_len does not need to be mutable
        if let Some((bef_len, ins_pos)) = ins_info && *bef_len -1  == *index {
            **ins_pos = *pos;
        }
        *index += 1;
        
        if *index >= items.len() {
            return Ok(!inc || phrase.in_bounds(*pos))
        }

        let back_index = *index;
        let back_alphas = self.alphas.borrow().clone();
        let back_refs = self.references.borrow().clone();
        let mut back_pos = *pos;
        while back_pos.syll_index == syll_index {
            *index = back_index;
            *pos = back_pos;
            self.matrix_increment(phrase, &mut back_pos);
            back_pos.increment(phrase);
            let mut m = true;
            while *index < items.len() {
                if pos.syll_index != syll_index && items[*index].kind != ParseElement::OptEllipsis {
                    m = false; break;
                }
                let pos_bef = *pos;
                match &items[*index].kind {
                    ParseElement::Ellipsis => if *index == items.len() - 1 {
                        // if last item, jump to end of syll and break loop
                        pos.syll_index = syll_index + 1;
                        pos.seg_index = 0;

                        if let Some((bef_len, ins_pos)) = ins_info && *bef_len - 1 == *index {
                            **ins_pos = pos_bef;
                        }
                        return Ok(true)
                    } else if self.context_match_ellipsis_struct(items, index, phrase, pos, syll_index, forwards, true, ins_info)? {
                        pos.syll_index = syll_index + 1;
                        pos.seg_index = 0;
                        return Ok(true)
                    } else { m = false; break; }, 
                    ParseElement::OptEllipsis => if *index == items.len() - 1 {
                        // if last item, jump to end of syll and break loop
                        pos.syll_index = syll_index + 1;
                        pos.seg_index = 0;
                        if let Some((bef_len, ins_pos)) = ins_info && *bef_len == *index - 1 {
                            **ins_pos = pos_bef;
                        }
                        return Ok(true)
                    } else if self.context_match_ellipsis_struct(items, index, phrase, pos, syll_index, forwards, false, ins_info)? {
                        pos.syll_index = syll_index + 1;
                        pos.seg_index = 0;
                        return Ok(true)
                    } else { m = false; break; }, 
                    ParseElement::Ipa(s, mods) => if self.context_match_ipa(s, mods, phrase, *pos, items[*index].position)? {
                        self.matrix_increment(phrase, pos);
                        pos.increment(phrase);
                    } else { m = false; break; },
                    ParseElement::Matrix(mods, refr) => if !self.context_match_matrix(mods, refr, phrase, pos, items[*index].position)? {
                        m = false; break;
                    },
                    // since syllables are invalid, passing `forwards` won't matter
                    ParseElement::Reference(num, mods) => if !self.context_match_ref(num, mods, phrase, pos, forwards, items[*index].position, true)? {
                        m = false; break;
                    },
                    ParseElement::Set(set) => if !self.context_match_set(set, phrase, pos, forwards, true)? {
                        m = false; break;
                    },
                    ParseElement::Optional(states, min, max) => if self.context_match_option(items, index, phrase, pos, forwards, states, *min, *max, true)? {
                        // This works i guess
                        if pos.syll_index == syll_index {
                            pos.syll_index += 1;
                            pos.seg_index = 0;
                        }
                        return Ok(true)
                    } else {m = false; break; },
                    _ => unreachable!()
                }
                *index += 1;
                if let Some((bef_len, ins_pos)) = ins_info && *bef_len == *index - 1 {
                    **ins_pos = pos_bef;
                }
            }
            if m && pos.seg_index == 0 && pos.syll_index == syll_index + 1 { 
                return Ok(true)
            }
            *self.alphas.borrow_mut() = back_alphas.clone();
            *self.references.borrow_mut() = back_refs.clone();
        }

        *self.alphas.borrow_mut() = back_alphas;
        *self.references.borrow_mut() = back_refs;
        Ok(false)
    }

    fn context_match_ellipsis(&self, states: &[ParseItem], state_index: &mut usize, phrase: &Phrase, pos: &mut SegPos, forwards: bool, inc: bool) -> Result<bool, RuleRuntimeError> {
        
        *state_index += 1;
        if inc { pos.increment(phrase) }
        
        if *state_index >= states.len() {
            return Ok(!inc || phrase.in_bounds(*pos))
        }

        let back_state = *state_index;
        let back_alphas = self.alphas.borrow().clone();
        let back_refs = self.references.borrow().clone();
        let mut back_pos = *pos;
        while phrase.in_bounds(back_pos) {
            *state_index = back_state;
            *pos = back_pos;
            // if let ParseElement::Matrix(mods, ..) = &states[back_state].kind && let [None, None] = mods.suprs.length {} else {
            //         self.matrix_increment(phrase, &mut back_pos)
            // }
            self.matrix_increment(phrase, &mut back_pos);
            back_pos.increment(phrase);
            let mut m = true;
            while *state_index < states.len() {
                if !self.context_match(states, state_index, phrase, pos, forwards, false, false)? {
                    m = false;
                    break;
                }
                *state_index += 1;
            }
            if m { return Ok(true) }
            *self.alphas.borrow_mut() = back_alphas.clone();
            *self.references.borrow_mut() = back_refs.clone();
        }
        *self.alphas.borrow_mut() = back_alphas;
        *self.references.borrow_mut() = back_refs;
        Ok(false)
    }

    fn match_opt_states(&self, opt_states: &[ParseItem], phrase: &Phrase, pos: &mut SegPos, forwards: bool, within_struct: bool) -> Result<bool, RuleRuntimeError> {
        let mut si = 0;
        while si < opt_states.len() {
            if !self.context_match(opt_states, &mut si, phrase, pos, forwards, false, within_struct)? {
                return Ok(false)
            }
            si += 1;
        }
        Ok(true)
    }

    // This is an absolute mess
    fn context_match_option(&self, states: &[ParseItem], state_index: &mut usize, phrase: &Phrase, pos: &mut SegPos, forwards: bool, opt_states: &[ParseItem], match_min: usize, match_max: usize, within_struct: bool) -> Result<bool, RuleRuntimeError> {
        // should work like regex (...){min, max}? 
        let match_max = if match_max == 0 { None } else { Some(match_max) };
        let back_pos = *pos;
        let back_alphas = self.alphas.borrow().clone();
        let back_refs = self.references.borrow().clone();

        let start_syll = pos.syll_index;

        // match states min times i.e. (nd, 2:5) matches ndnd minimum
        let mut index = 0;
        while index < match_min {
            if !self.match_opt_states(opt_states, phrase, pos, forwards, within_struct)? {
                *pos = back_pos;
                *self.alphas.borrow_mut() = back_alphas;
                *self.references.borrow_mut() = back_refs;
                return Ok(false)
            }
            index += 1;
        }

        // after matching min times, check the surrounding states
        *state_index += 1;
        let back_state = *state_index;
        let back_pos = *pos;
        let back_alphas = self.alphas.borrow().clone();
        let back_refs = self.references.borrow().clone();

        if *state_index >= states.len() && within_struct {
            if pos.at_syll_end(phrase) {
                pos.increment(phrase);
                return Ok(true)
            } else if pos.at_syll_start() {
                return Ok(true)
            }
        } else {
            let mut m = true;
            while *state_index < states.len() {
                if !self.context_match(states, state_index, phrase, pos, forwards, false, within_struct)? {
                    m = false;
                    break;
                }
                *state_index += 1;
            }
            if m {
                return Ok(true)
            } else {
                *self.alphas.borrow_mut() = back_alphas.clone();
                *self.references.borrow_mut() = back_refs.clone();
            }
        }

        // If no match at this point, try matching max times
        *pos = back_pos;
        *state_index = back_state;
        *self.alphas.borrow_mut() = back_alphas.clone();
        *self.references.borrow_mut() = back_refs.clone();
        let max = match_max.unwrap_or(usize::MAX);
        while index < max {
            *state_index = back_state;
            if self.match_opt_states(opt_states, phrase, pos, forwards, within_struct)? {
                let mut m = true;
                while *state_index < states.len() {
                    if !self.context_match(states, state_index, phrase, pos, forwards, false, within_struct)? {
                        m = false;
                        break;
                    }
                    *state_index += 1;
                }
                if !within_struct && m {                    
                    return Ok(true)
                } else if m {
                    if pos.at_syll_end(phrase) || pos.at_syll_start() && pos.syll_index == start_syll + 1 {
                        return Ok(true)
                    } else if *state_index >= states.len() && index < max {
                            index += 1;
                            *self.alphas.borrow_mut() = back_alphas.clone();
                            *self.references.borrow_mut() = back_refs.clone();
                            continue;
                    } else {
                        *self.alphas.borrow_mut() = back_alphas.clone();
                        *self.references.borrow_mut() = back_refs.clone();
                        return Ok(false)
                    }
                } else {
                    index += 1;
                    *self.alphas.borrow_mut() = back_alphas.clone();
                    *self.references.borrow_mut() = back_refs.clone();
                    continue;
                }
            } else {
                *self.alphas.borrow_mut() = back_alphas.clone();
                *self.references.borrow_mut() = back_refs.clone();
                return Ok(false)
            }
        }

        *self.alphas.borrow_mut() = back_alphas.clone();
        *self.references.borrow_mut() = back_refs.clone();
        Ok(false)
    }

    fn context_match_set(&self, set: &[ParseItem], phrase: &Phrase, pos: &mut SegPos, forwards: bool, within_struct: bool) -> Result<bool, RuleRuntimeError> {
        let back_pos= *pos;
        let back_alphas = self.alphas.borrow().clone();
        let back_refs = self.references.borrow().clone();
        
        for s in set {
            let res = match &s.kind {
                ParseElement::SyllBound if within_struct => Err(RuleRuntimeError::BoundaryInsideStruct(s.position)),
                ParseElement::WordBound if within_struct => Err(RuleRuntimeError::BoundaryInsideStruct(s.position)),
                // Since the lexer doesn't allow nesting, this will never happen. But, it's nice to have.
                ParseElement::Structure(..) if within_struct => Err(RuleRuntimeError::StructInsideStruct(s.position)),
                ParseElement::Syllable (..) if within_struct => Err(RuleRuntimeError::SyllbleInsideStruct(s.position)),

                ParseElement::Structure(items, stress, tone, refr) => self.context_match_structure(items, stress, tone, refr, phrase, pos, forwards, s.position),

                ParseElement::Reference(vt, mods) => self.context_match_ref(vt, mods, phrase, pos, forwards, s.position, within_struct),
                ParseElement::Ipa(seg, mods) => if self.context_match_ipa(seg, mods, phrase, *pos, s.position)? {
                    self.matrix_increment(phrase, pos);
                    pos.increment(phrase);
                    Ok(true)
                } else {Ok(false)},
                ParseElement::Matrix(mods, refr) => self.context_match_matrix(mods, refr, phrase, pos, s.position),
                ParseElement::Syllable(stress, tone, refr) => self.context_match_syll(stress, tone, refr, phrase, pos, forwards, s.position),
                ParseElement::WordBound => Ok(phrase[pos.word_index].out_of_bounds(*pos)),
                ParseElement::SyllBound => Ok(pos.at_syll_start()),
                _ => unimplemented!(),
            };
            if res? {
                return Ok(true)
            }
            *pos = back_pos;
            // TODO: Deal with these clones
            *self.alphas.borrow_mut() = back_alphas.clone();
            *self.references.borrow_mut() = back_refs.clone();
        }
        Ok(false)
    }

    fn context_match_ref(&self, refr: &Reference, mods: &Option<Modifiers>, phrase: &Phrase, pos: &mut SegPos, forwards: bool, err_pos: Position, within_struct: bool) -> Result<bool, RuleRuntimeError> {
        if let Some(rk) = self.references.borrow().get(&refr.value) {
            match rk {
                RefKind::Segment(s) => if self.context_match_ipa(s, mods, phrase, *pos, err_pos)? {
                    self.matrix_increment(phrase, pos);
                    pos.increment(phrase);
                    Ok(true)
                } else { Ok(false) },
                RefKind::Syllable(_) if within_struct => Err(RuleRuntimeError::SyllRefInsideStruct(refr.position)),
                RefKind::Syllable(s) => self.context_match_syll_ref(s, mods, phrase, pos, forwards, err_pos),
            }            
        } else {
            Err(RuleRuntimeError::UnknownReference(*refr))
        }
    }

    fn context_match_syll_ref(&self, syll_to_match: &Syllable, mods: &Option<Modifiers>, phrase: &Phrase, pos: &mut SegPos, forwards: bool, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        if !pos.at_syll_start()  {
            return Ok(false)
        }
        let cur_syll = if phrase.in_bounds(*pos) {
             &phrase[pos.word_index].syllables[pos.syll_index]
        } else { return Ok(false) };

        let segs_to_match = if forwards {
            syll_to_match.segments.clone() // i hate this
        } else {
            let mut segs = syll_to_match.segments.clone();
            segs.make_contiguous().reverse();
            segs
        };
        
        if let Some(Modifiers { nodes: _, feats: _, suprs }) = mods {
            if !self.match_stress(&suprs.stress, cur_syll, err_pos)? {
                return Ok(false)
            }
            if let Some(t) = suprs.tone.as_ref() && !self.match_tone(t, cur_syll) {
                return Ok(false)
            }
            if cur_syll.segments != syll_to_match.segments {
                return Ok(false)
            }
        } else if cur_syll.segments != segs_to_match || cur_syll.stress != syll_to_match.stress || cur_syll.tone != syll_to_match.tone {
            return Ok(false)
        }
        pos.syll_index += 1;
        pos.seg_index = 0;
        Ok(true)
    }

    fn context_match_syll(&self, stress: &Option<SpecMod>, tone: &Option<Tone>, refr: &Option<usize>, phrase: &Phrase, pos: &mut SegPos, forwards: bool, err_pos: Position) -> Result<bool, RuleRuntimeError> {        
        if !pos.at_syll_start() {
            return Ok(false)
        }
        let cur_syll = if phrase.in_bounds(*pos) { 
            &phrase[pos.word_index].syllables[pos.syll_index] 
        } else { return Ok(false) };

        if !self.match_stress(stress, cur_syll, err_pos)? {
            return Ok(false)
        }
        if let Some(t) = tone.as_ref() && !self.match_tone(t, cur_syll) {
            return Ok(false)
        }
        if let Some(r) = refr {
            if forwards {
                self.references.borrow_mut().insert(*r, RefKind::Syllable(phrase[pos.word_index].syllables[pos.syll_index].clone()));
            } else {
                let mut syll = phrase[pos.word_index].syllables[pos.syll_index].clone();
                syll.segments.make_contiguous().reverse();
                self.references.borrow_mut().insert(*r, RefKind::Syllable(syll));
            }
        }
        pos.syll_index += 1;
        pos.seg_index = 0;
        Ok(true)
    }

    fn context_match_ipa(&self, s: &Segment, mods: &Option<Modifiers>, phrase: &Phrase, pos: SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        if phrase[pos.word_index].out_of_bounds(pos) {
            return Ok(false)
        }
        // SAFETY: pos is in bounds and phrase is not empty
        let seg = unsafe { phrase.get_seg_at(pos).unwrap_unchecked() };
        if let Some(m) = mods {
            Ok(self.match_ipa_with_modifiers(s, m, phrase, &pos, err_pos)?)
        } else {
            Ok(*s == seg)
        }
    }

    fn context_match_matrix(&self, mods: &Modifiers, refr: &Option<usize>, phrase: &Phrase, pos: &mut SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> {        
        if phrase[pos.word_index].out_of_bounds(*pos) { return Ok(false) }
        if self.match_modifiers(mods, phrase, pos, err_pos)? {
            if let Some(r) = refr {
                // SAFETY: pos is in bounds and phrase is not empty
                self.references.borrow_mut().insert(*r, RefKind::Segment(unsafe { phrase.get_seg_at(*pos).unwrap_unchecked() }));
            }
            self.matrix_increment(phrase, pos);
            pos.increment(phrase);
            Ok(true)
        } else {
            self.matrix_increment(phrase, pos);
            Ok(false)
        }
    }
}

impl SubRule { // Input Matching
    fn input_match_at(&self, phrase: &Phrase, start_index: SegPos, state_index: usize) -> Result<(Vec<MatchElement>, Option<SegPos>), RuleRuntimeError> {
        let mut cur_index = start_index;
        let mut match_begin = None;
        let mut state_index = state_index;
        let mut captures: Vec<_> = Vec::new();

        while phrase.in_bounds(cur_index) {
            if self.input_match_item(&mut captures, &mut cur_index, &mut state_index, phrase, &self.input)? {
                // if we have a full match
                if state_index >= self.input.len() { 
                    // As matching a syllbound doesn't increment, this is to avoid an infinite loop
                    if matches!(captures.last(), Some(MatchElement::SyllBound(..))) {
                        cur_index.increment(phrase);
                    }
                    return Ok((captures, Some(cur_index)));
                }
                // if we haven't started matching, we have now
                if match_begin.is_none() { 
                    debug_assert!(!captures.is_empty());
                    // SAFETY: captures is not empty
                    match unsafe { captures.last().unwrap_unchecked() } {
                        MatchElement::Segment(sp, _) => match_begin = Some(*sp),
                        MatchElement::LongSegment(sp, _) => {
                            let len = phrase.seg_length_at(*sp);
                            match_begin = Some(SegPos { word_index: sp.word_index, syll_index: sp.syll_index, seg_index: sp.seg_index + len - 1 })
                        },
                        MatchElement::Syllable(wp, sp, _) |
                        MatchElement::SyllBound(wp, sp, _) => match_begin = Some(SegPos { word_index: *wp, syll_index: *sp, seg_index: 0 }),
                        MatchElement::WordBound(wp) => match_begin = Some(SegPos { word_index: wp+1, syll_index: 0, seg_index: 0 })
                    }
                }
                // else continue 
            } else if let Some (x) = match_begin { 
                // if we were in the middle of matching but now don't match, go back to when we started matching +1 and start again
                cur_index = x;
                cur_index.increment(phrase);
                state_index = 0;
                captures = vec![];
                match_begin = None;
                self.alphas.borrow_mut().clear();
                self.references.borrow_mut().clear();
            } else {
                // if we weren't in the middle of matching, move on
                cur_index.increment(phrase);
                self.alphas.borrow_mut().clear();
                self.references.borrow_mut().clear();
                // NOTE(girv): Should be unnecessary, but safety first!
                state_index = 0;
                captures = vec![];
            }
        }

        if let Some(pi) = self.input.get(state_index) && pi.kind == ParseElement::ExtlBound && !cur_index.at_phrase_end(phrase) {
            cur_index.decrement(phrase);
            let (res, next_index) = self.input_match_at(phrase, cur_index, state_index)?;
            captures.extend(res);
            return Ok((captures, next_index))
        }

        debug_assert!(!self.input.is_empty());

        if match_begin.is_none() { // if we've got to the end of the word and we haven't began matching
            if self.input.len() == 1 && self.input[0].kind == ParseElement::SyllBound  {
                captures.push(MatchElement::SyllBound(phrase.len()-1, phrase[phrase.len()-1].syllables.len(), None));
                Ok((captures, None))
            } else if self.input.len() == 1 && let Some(si) =  self.contains_syll_bound(&self.input[0]) {
                captures.push(MatchElement::SyllBound(phrase.len()-1, phrase[phrase.len()-1].syllables.len(), Some(si)));
                Ok((captures, None))
            } else {
                Ok((vec![], None))
            }
        } else if unsafe { self.input.last().unwrap_unchecked().kind == ParseElement::SyllBound } { // SAFETY: input is not empty
            // if we've reached the end of the word and the last state is a word boundary
            captures.push(MatchElement::SyllBound(phrase.len()-1, phrase[phrase.len()-1].syllables.len(), None));
            Ok((captures, None))
        } else if let Some(si) = self.contains_syll_bound(unsafe { self.input.last().unwrap_unchecked() }) { // SAFETY: input is not empty
            // if we've reached the end of the word and the last state is a set containing a word boundary
            captures.push(MatchElement::SyllBound(phrase.len()-1, phrase[phrase.len()-1].syllables.len(), Some(si)));
            Ok((captures, None))
        } else { // No Match
            Ok((vec![], None))
        }
    }

    fn contains_syll_bound(&self, x: &ParseItem) -> Option<usize> {
        let ParseElement::Set(set) = &x.kind else { return None };

        for (i, item) in set.iter().enumerate() {
            if item.kind == ParseElement::SyllBound {
                return Some(i)
            }
        }

        None
    }

    fn input_match_item(
        &self, 
        captures: &mut Vec<MatchElement>, 
        seg_pos: &mut SegPos, 
        state_index: &mut usize,
        phrase: &Phrase, 
        states: &[ParseItem], 
    ) -> Result<bool, RuleRuntimeError> {
        let err_pos = states[*state_index].position;
        match &states[*state_index].kind {
            ParseElement::Reference(refr, m) => if self.input_match_ref(captures, state_index, refr, m, phrase, seg_pos, err_pos)? {
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseElement::Ipa(s, m) => if self.input_match_ipa(captures, s, m, phrase, seg_pos, err_pos)? {
                seg_pos.increment(phrase);
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseElement::Matrix(m, v) => if self.input_match_matrix(captures, m, v, phrase, seg_pos, err_pos)? {
                seg_pos.increment(phrase);
                *state_index += 1;
                Ok(true) 
            } else { Ok(false) },
            ParseElement::Set(s) => if self.input_match_set(captures, state_index, s, phrase, seg_pos)? {
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseElement::SyllBound => if self.input_match_syll_bound(captures, *seg_pos) {
                // NOTE(girv): Boundaries do not advance seg_index 
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseElement::ExtlBound => if seg_pos.at_word_end(phrase) && !seg_pos.at_phrase_end(phrase) {
                captures.push(MatchElement::WordBound(seg_pos.word_index));
                seg_pos.word_increment(phrase);
                *state_index += 1;
                Ok(true)
            } else {
                Ok(false)
            },
            ParseElement::Syllable(s, t, v) => self.input_match_syll(captures, state_index, s, t, v, phrase, seg_pos, err_pos),
            ParseElement::Structure(segs, stress, tone, refr) => self.input_match_structure(captures, state_index, segs, stress, tone, refr, phrase, seg_pos, err_pos),
            ParseElement::Ellipsis  => self.input_match_ellipsis(captures, phrase, seg_pos, states, state_index, true),
            ParseElement::OptEllipsis => self.input_match_ellipsis(captures, phrase, seg_pos, states, state_index, false),

            ParseElement::Optional(..) | ParseElement::EmptySet  | 
            ParseElement::Metathesis   | ParseElement::WordBound => unreachable!(),
        }
    }

    fn input_match_structure(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, items: &[ParseItem], stress: &Option<SpecMod>, tone: &Option<Tone>, refr: &Option<usize>, phrase: &Phrase, pos: &mut SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        if items.is_empty() {
            return self.input_match_syll(captures, state_index, stress, tone, refr, phrase, pos, err_pos)
        }
        if !(phrase.in_bounds(*pos) && pos.seg_index == 0) {
            return Ok(false)
        }

        let cur_word_index = pos.word_index;
        let cur_syll_index = pos.syll_index;
        let cur_syll = &phrase[cur_word_index].syllables[cur_syll_index];

        if !self.match_stress(stress, cur_syll, err_pos)? {
            return Ok(false)
        }
        if let Some(t) = tone.as_ref() && !self.match_tone(t, cur_syll) {
            return Ok(false)
        }

        for (mut i, item) in items.iter().enumerate() {
            if pos.syll_index != cur_syll_index && item.kind != ParseElement::OptEllipsis {
                return Ok(false)
            }
            match &item.kind {
                ParseElement::Ellipsis => if i == items.len() - 1 {
                    pos.syll_index += 1;
                    pos.seg_index = 0;
                    break;
                } else if self.context_match_ellipsis_struct(items, &mut i, phrase, pos, cur_syll_index, true, true, &mut None)? {
                    break;
                } else { return Ok(false) },
                ParseElement::OptEllipsis => if i == items.len() - 1 {
                    pos.syll_index += 1;
                    pos.seg_index = 0;
                    break;
                } else if self.context_match_ellipsis_struct(items, &mut i, phrase, pos, cur_syll_index, true, false, &mut None)? {
                    break;
                } else { return Ok(false) },
                ParseElement::Ipa(s, mods) => if self.context_match_ipa(s, mods, phrase, *pos, item.position)? {
                    self.matrix_increment(phrase, pos);
                    pos.increment(phrase);
                } else { return Ok(false) },
                ParseElement::Matrix(mods, refr) => if !self.context_match_matrix(mods, refr, phrase, pos, item.position)? {
                    return Ok(false)
                },
                ParseElement::Reference(num, mods) => match self.references.borrow().get(&num.value) {
                    Some(refr) => match refr {
                        RefKind::Segment(s) => if self.context_match_ipa(s, mods, phrase, *pos, item.position)? {
                            self.matrix_increment(phrase, pos);
                            pos.increment(phrase);
                        } else { return Ok(false) },
                        RefKind::Syllable(_) => return Err(RuleRuntimeError::SyllRefInsideStruct(item.position)),
                    },
                    None => return Err(RuleRuntimeError::UnknownReference(*num)),
                }
                ParseElement::Optional(states, min, max) => if self.context_match_option(items, &mut i, phrase, pos, true, states, *min, *max, true)? {
                    // This works i guess
                    if pos.syll_index == cur_syll_index {
                        pos.syll_index += 1;
                        pos.seg_index = 0;
                    }                    
                    break;
                } else { return Ok(false) },
                ParseElement::Set(set) => if !self.context_match_set(set, phrase, pos, true, true)? {
                    return Ok(false) 
                },
                ParseElement::WordBound  | ParseElement::SyllBound | ParseElement::EmptySet | 
                ParseElement::Metathesis | ParseElement::ExtlBound | 
                ParseElement::Syllable(..) | ParseElement::Structure(..) => unreachable!(),
            }
        }
        if pos.seg_index != 0 { return Ok(false) }

        if let Some(r) = refr {
            self.references.borrow_mut().insert(*r, RefKind::Syllable(phrase[cur_word_index].syllables[cur_syll_index].clone()));
        }
        captures.push(MatchElement::Syllable(cur_word_index, cur_syll_index, None));
        *state_index += 1;
        Ok(true)
    }

    fn input_match_ellipsis(&self, captures: &mut Vec<MatchElement>, phrase: &Phrase, pos: &mut SegPos, states: &[ParseItem], state_index: &mut usize, inc: bool) -> Result<bool, RuleRuntimeError> {
        // should work akin to '.+?' or '.*?' (depending on value of inc) in Regex, 
        // that is, a lazy-match of one-or-more elements or lazy-match of zero-or-more elements
        
        *state_index += 1;
        if inc { pos.increment(phrase) }
        
        if *state_index >= states.len() {
            return Ok(!inc || phrase.in_bounds(*pos))
        }

        let back_state = *state_index;
        let back_alphas = self.alphas.borrow().clone();
        let back_refs = self.references.borrow().clone();
        let back_captures_len = captures.len();
        let mut back_pos = *pos;
        while phrase.in_bounds(back_pos) {
            *state_index = back_state;
            *pos = back_pos;
            self.matrix_increment(phrase, &mut back_pos);
            back_pos.increment(phrase);
            let mut m = true;
            while *state_index < states.len() {
                if !self.input_match_item(captures, pos, state_index, phrase, states)? {
                    m = false;
                    break;
                }
            }
            if m { return Ok(true) }
            *self.alphas.borrow_mut() = back_alphas.clone();
            *self.references.borrow_mut() = back_refs.clone();
            captures.truncate(back_captures_len);
        }
        
        *self.alphas.borrow_mut() = back_alphas;
        *self.references.borrow_mut() = back_refs;
        captures.truncate(back_captures_len);
        Ok(false)
    }

    fn input_match_syll(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, stress: &Option<SpecMod>, tone: &Option<Tone>, refr: &Option<usize>, phrase: &Phrase, pos: &mut SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        // checks current segment is at start of syll
        // matches stress and tone
        // jumps to end of syllable if match
        if phrase.in_bounds(*pos) && pos.seg_index == 0 {
        // if word.seg_is_syll_initial(*seg_index) {
            let cur_word_index = pos.word_index;
            let cur_syll_index = pos.syll_index;
            let cur_syll = &phrase[cur_word_index].syllables[cur_syll_index];

            if !self.match_stress(stress, cur_syll, err_pos)? {
                return Ok(false)
            }
            if let Some(t) = tone.as_ref() && !self.match_tone(t, cur_syll) {
                return Ok(false)
            }
            if let Some(r) = refr {
                self.references.borrow_mut().insert(*r, RefKind::Syllable(phrase[cur_word_index].syllables[cur_syll_index].clone()));
            }
            captures.push(MatchElement::Syllable(cur_word_index, cur_syll_index, None));
            *state_index += 1;
            pos.syll_index += 1;
            pos.seg_index = 0;

            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn input_match_syll_bound(&self, captures: &mut Vec<MatchElement>, pos: SegPos) -> bool {
        if pos.seg_index == 0 {
            captures.push(MatchElement::SyllBound(pos.word_index, pos.syll_index, None));
            true
        } else {
            false
        }
    }

    fn input_match_set(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, set: &[ParseItem], phrase: &Phrase, pos: &mut SegPos) -> Result<bool, RuleRuntimeError> {
        let back_pos = *pos;
        let back_alphas = self.alphas.borrow().clone();
        let back_refs = self.references.borrow().clone();

        for (i,s) in set.iter().enumerate() {
            let res = match &s.kind {
                ParseElement::Reference(vt, mods) => self.input_match_ref(captures, state_index, vt, mods, phrase, pos, s.position),
                ParseElement::Ipa(seg, mods) => if self.input_match_ipa(captures, seg, mods, phrase, pos, s.position)? {
                    pos.increment(phrase);
                    Ok(true)
                } else { Ok(false) },
                ParseElement::Matrix(mods, refr) => if self.input_match_matrix(captures, mods, refr, phrase, pos, s.position)? {
                    pos.increment(phrase);
                    Ok(true)
                } else { Ok(false) },
                ParseElement::Syllable(stress, tone, refr) => self.input_match_syll(captures, state_index, stress, tone, refr, phrase, pos, s.position),
                ParseElement::SyllBound => if pos.at_syll_start() {
                    captures.push(MatchElement::SyllBound(pos.word_index, pos.syll_index, Some(i)));
                    Ok(true)
                } else { Ok(false) },
                ParseElement::WordBound => Err(RuleRuntimeError::WordBoundSetLocError(s.position)),
                ParseElement::Structure(items, stress, tone, refr) => self.input_match_structure(captures, state_index, items, stress, tone, refr, phrase, pos, s.position),
                _ => unreachable!(),
            };
            if res? {
                debug_assert!(!captures.is_empty());
                // SAFETY: captures is not empty
                unsafe { captures.last_mut().unwrap_unchecked().set_ind(Some(i)) };
                return Ok(true)
            }
            *pos = back_pos;
            // TODO: Deal with these clones
            *self.alphas.borrow_mut() = back_alphas.clone();
            *self.references.borrow_mut() = back_refs.clone();
        }
        Ok(false)
    }

    fn input_match_ipa(&self, captures: &mut Vec<MatchElement>, s: &Segment, mods: &Option<Modifiers>, phrase: &Phrase, pos: &mut SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        let Some(seg) = phrase.get_seg_at(*pos) else { return Ok(false) };

        if let Some(m) = mods {
            if self.match_ipa_with_modifiers(s, m, phrase, pos, err_pos)? {
                if m.suprs.length.is_some() {
                    captures.push(MatchElement::LongSegment(*pos, None));
                } else {
                    captures.push(MatchElement::Segment(*pos, None));
                }
                self.matrix_increment(phrase, pos);
                Ok(true)
            } else {
                self.matrix_increment(phrase, pos);
                Ok(false)
            }
        } else if *s == seg {
            captures.push(MatchElement::Segment(*pos, None));
            self.matrix_increment(phrase, pos);
            Ok(true)
        } else {
            self.matrix_increment(phrase, pos);
            Ok(false)
        }
    }

    fn input_match_syll_ref(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, syll_to_match: &Syllable, mods: &Option<Modifiers>, phrase: &Phrase, pos: &mut SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        if pos.seg_index != 0 || phrase[pos.word_index].out_of_bounds(*pos) {
            return Ok(false)
        }
        let cwi = pos.word_index;
        let csi = pos.syll_index;
        let cur_syll = &phrase[cwi].syllables[csi];

        if let Some(m) = mods {
            if !self.match_stress(&m.suprs.stress, cur_syll, err_pos)? {
                return Ok(false)
            } 
            if let Some(t) = &m.suprs.tone.as_ref() && !self.match_tone(t, cur_syll) {
                return Ok(false)
            }
            if cur_syll.segments != syll_to_match.segments {
                return Ok(false)
            }
        } else if *cur_syll != *syll_to_match {
            return Ok(false)
        }
        captures.push(MatchElement::Syllable(cwi, csi, None));

        *state_index += 1;
        pos.syll_index += 1;
        pos.seg_index = 0;
        
        Ok(true)
        
    }

    fn input_match_ref(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, refr: &Reference, mods: &Option<Modifiers>, phrase: &Phrase, pos: &mut SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        match self.references.borrow_mut().get(&refr.value) {
            Some(rk) => match rk {
                RefKind::Segment(s)  => if self.input_match_ipa(captures, s, mods, phrase, pos, err_pos)? {
                    pos.increment(phrase);
                    Ok(true)
                } else { Ok(false) },
                RefKind::Syllable(s) => self.input_match_syll_ref(captures, state_index , s, mods, phrase, pos, err_pos),
            },
            None => Err(RuleRuntimeError::UnknownReference(*refr)),
        }
    }

    fn input_match_matrix(&self, captures: &mut Vec<MatchElement>, mods: &Modifiers, refr: &Option<usize>, phrase: &Phrase, pos: &mut SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> { 
        if phrase[pos.word_index].out_of_bounds(*pos) { return Ok(false) }
        if self.match_modifiers(mods, phrase, pos, err_pos)? {
            if let Some(r) = refr {
                let Some(seg) = phrase.get_seg_at(*pos) else { return Ok(false) };
                self.references.borrow_mut().insert(*r, RefKind::Segment(seg));
            }
            captures.push(MatchElement::LongSegment(*pos, None));
            self.matrix_increment(phrase, pos);
            Ok(true)
        } else {
            self.matrix_increment(phrase, pos);
            Ok(false)
        }
    }
}

impl SubRule { // Insertion
    fn insert(&self, phrase: &Phrase, pos: SegPos, is_context_after: bool) -> Result<(Phrase, Option<SegPos>), RuleRuntimeError> {
        let mut res_phrase = phrase.clone();
        let mut pos = pos;
        for state in &self.output {
            match &state.kind {
                ParseElement::Ipa(seg, mods) => {
                    if let Some(syll) = res_phrase[pos.word_index].syllables.get_mut(pos.syll_index) { 
                        let lc = syll.insert_segment(pos.seg_index, seg, mods, &self.alphas, state.position)?;
                        if lc > 0 {
                            pos.seg_index += lc.unsigned_abs() as usize + 1;
                        }
                    } else {
                        res_phrase[pos.word_index].syllables.last_mut().unwrap().segments.push_back(*seg);
                        // If out of bounds, move to added segment
                        pos.syll_index = res_phrase[pos.word_index].syllables.len() - 1;
                        pos.seg_index = res_phrase[pos.word_index].syllables[pos.syll_index].segments.len() - 1;
                        if let Some(m) = mods {
                            let lc = res_phrase[pos.word_index].apply_seg_mods(&self.alphas, m, pos, state.position)?;
                            if lc > 0 {
                                pos.seg_index += lc.unsigned_abs() as usize + 1;
                            }
                        } 
                    };
                    pos.increment(&res_phrase);
                },
                ParseElement::SyllBound => {
                    // FIXME(girv): Issue
                    // if we are part sequence of rules that are inserting a new syllable to the start of the word
                    // then we want the original syllable (which in this case will become the second syllable) to have the stress/tone
                    // however, this edge case is not possible to discern from

                    if pos.at_syll_start() {
                        continue;
                    }
                    // split current syll into two at pos
                    let mut second_syll = Syllable::new();
                    let first_syll = res_phrase[pos.word_index].syllables.get_mut(pos.syll_index).expect("A word has at least 1 syllable");

                    while first_syll.segments.len() > pos.seg_index {
                        // SAFETY: pos.seg_index > 0 and first_syll.segments.len() > pos.seg_index
                        second_syll.segments.push_front(unsafe { first_syll.segments.pop_back().unwrap_unchecked() });
                    }

                    res_phrase[pos.word_index].syllables.insert(pos.syll_index+1, second_syll);

                    pos.syll_index += 1;
                    pos.seg_index = 0;
                },
                ParseElement::Syllable(stress, tone, refr) => {
                    if pos.at_syll_start() {
                        // apply mods to current syllable, possibly not a good idea
                        if let Some(syll) = res_phrase[pos.word_index].syllables.get_mut(pos.syll_index) {
                            syll.apply_syll_mods(&self.alphas, &SupraSegs { stress: *stress, length: None, tone: *tone }, state.position)?;
                            if let Some(r) = refr {
                                self.references.borrow_mut().insert(*r, RefKind::Syllable(res_phrase[pos.word_index].syllables[pos.syll_index].clone()));
                            }
                            continue;
                        } else {
                            // NOTE: Same behaviour as SyllBound above
                            continue;
                        }
                    } 
                    // split current syll into two at insert_pos
                    // Apply mods to second syll
                    let mut new_syll = Syllable::new();
                    new_syll.apply_syll_mods(&self.alphas, &SupraSegs { stress: *stress, length: None, tone: *tone }, state.position)?;

                    let syll = res_phrase[pos.word_index].syllables.get_mut(pos.syll_index).expect("pos should not be out of bounds");

                    while syll.segments.len() > pos.seg_index {
                        // SAFETY: pos.seg_index > 0 and syll.segments.len() > pos.seg_index
                        new_syll.segments.push_front(unsafe { syll.segments.pop_back().unwrap_unchecked() });
                    }
                    res_phrase[pos.word_index].syllables.insert(pos.syll_index+1, new_syll);

                    pos.syll_index += 2;
                    pos.seg_index = 0;

                    if let Some(r) = refr {
                        self.references.borrow_mut().insert(*r, RefKind::Syllable(res_phrase[pos.word_index].syllables[pos.syll_index -1].clone()));
                    }
                },
                ParseElement::Structure(items, stress, tone, refr) => {
                    let insert_syll = self.gen_syll_from_struct(items, stress, tone, refr, state.position, true)?;

                    if pos.at_syll_start() {
                        res_phrase[pos.word_index].syllables.insert(pos.syll_index, insert_syll);
                        pos.syll_index += 1;
                        pos.seg_index = 0;
                        continue;
                    }

                    let old_syll = res_phrase[pos.word_index].syllables.get_mut(pos.syll_index).expect("pos should not be out of bounds");
                    
                    let mut new_syll = Syllable::new();
                    new_syll.stress = old_syll.stress;
                    new_syll.tone = old_syll.tone;

                    while old_syll.segments.len() > pos.seg_index {
                        // SAFETY: pos.seg_index > 0 and old_syll.segments.len() > pos.seg_index
                        new_syll.segments.push_front(unsafe { old_syll.segments.pop_back().unwrap_unchecked() });
                    }

                    let mut adjustment = 0;
                    if old_syll.segments.is_empty() {
                        *old_syll = insert_syll;
                    } else {
                        res_phrase[pos.word_index].syllables.insert(pos.syll_index+1, insert_syll);
                        adjustment = 1;
                    }
                    
                    if !new_syll.segments.is_empty() {
                        res_phrase[pos.word_index].syllables.insert(pos.syll_index+1+adjustment, new_syll);
                        pos.syll_index += 2 + adjustment;
                    } else {
                        pos.syll_index += 1 + adjustment;
                    }
                    
                    pos.seg_index = 0;
                },
                ParseElement::Reference(num, mods) => {
                    if let Some(refr) = self.references.borrow().get(&num.value) {
                        match refr {
                            RefKind::Segment(seg) => {
                                if let Some(syll) = res_phrase[pos.word_index].syllables.get_mut(pos.syll_index) { 
                                    let lc = syll.insert_segment(pos.seg_index, seg, mods, &self.alphas, state.position)?;
                                    if lc > 0 {
                                        pos.seg_index += lc.unsigned_abs() as usize;
                                    }
                                } else {
                                    res_phrase[pos.word_index].syllables.last_mut().unwrap().segments.push_back(*seg);
                                    // If out of bounds, move to added segment
                                    pos.syll_index = res_phrase[pos.word_index].syllables.len() - 1;
                                    pos.seg_index = res_phrase[pos.word_index].syllables[pos.syll_index].segments.len() - 1;
                                    if let Some(m) = mods {
                                        let lc = res_phrase[pos.word_index].apply_seg_mods(&self.alphas, m, pos, state.position)?;
                                        if lc > 0 {
                                            pos.seg_index += lc.unsigned_abs() as usize;
                                        }
                                    } 
                                };
                                pos.increment(&res_phrase);
                            },
                            RefKind::Syllable(syll) => {
                                let mut new_syll = syll.clone();
                                if let Some(m) = mods {
                                    new_syll.apply_syll_mods(&self.alphas, &m.suprs, num.position)?;
                                }
                                if pos.at_syll_start() {
                                    res_phrase[pos.word_index].syllables.insert(pos.syll_index, new_syll.clone());
                                    pos.syll_index += 1;
                                } else {
                                    // split current syllable in two, insert ref_syll in between them
                                    let before_syll = res_phrase[pos.word_index].syllables.get_mut(pos.syll_index).unwrap();
                                    let mut after_syll = Syllable::new();
                                    while before_syll.segments.len() > pos.seg_index {
                                        after_syll.segments.push_front(before_syll.segments.pop_back().unwrap());
                                    }
                                    res_phrase[pos.word_index].syllables.insert(pos.syll_index+1, after_syll);
                                    res_phrase[pos.word_index].syllables.insert(pos.syll_index+1, new_syll);

                                    pos.syll_index += 3;
                                    pos.seg_index = 0;
                                }
                            },
                        }
                    } else {
                        return Err(RuleRuntimeError::UnknownReference(*num))
                    }
                },
                ParseElement::Ellipsis | ParseElement::OptEllipsis => {/* Do Nothing */},

                ParseElement::Matrix(..) => return Err(RuleRuntimeError::InsertionMatrix(state.position)),
                ParseElement::Set(_) => return Err(RuleRuntimeError::LonelySet(state.position)),

                ParseElement::WordBound  | ParseElement::EmptySet  | ParseElement::Optional(..) |
                ParseElement::Metathesis | ParseElement::ExtlBound => unreachable!(),
            }
        }

        if is_context_after {
            pos.increment(&res_phrase);
        }
        
        Ok((res_phrase, Some(pos)))
    }

    fn insertion_match_exceptions(&self, phrase: &Phrase, ins_pos: SegPos) -> Result<bool, RuleRuntimeError> {
        let empty = Vec::new();
        let exceptions = self.get_exceptions();

        let (before_expt, center_expt, after_expt) = match exceptions.len().cmp(&1) {
            std::cmp::Ordering::Less => (&empty, &None, &empty),
            std::cmp::Ordering::Equal => exceptions[0],
            std::cmp::Ordering::Greater => return Err(RuleRuntimeError::InsertionGroupedEnv(self.except.clone().unwrap().position)),
        };

        // TODO: test this
        if center_expt.is_some() {
            unreachable!("We only enter this function when matching by segment")
        }

        let mut before_expt = before_expt.clone();
        before_expt.reverse();

        match (before_expt.is_empty(), after_expt.is_empty()) {
            // _
            (true, true) => Ok(false),
            (false, true) => {
                // #_
                let phrase_rev = phrase.reversed();
                let pos_rev = ins_pos.reversed(phrase);
                let match_bef = self.match_before_env(&before_expt, &phrase_rev, &pos_rev, false, true, false)?;
                Ok(match_bef)
            },
            (true, false) => {
                // _#
                // edge case for when insertion position is out of bounds but not at the word end
                if after_expt.len() == 1 && after_expt[0].kind == ParseElement::WordBound && !ins_pos.at_word_end(phrase) {
                    return Ok(false)
                }
                let match_aft = self.match_after_env(after_expt, phrase, &ins_pos, true, false, false)?;
                Ok(match_aft)
            },
            // #_#
            (false, false) => {
                let phrase_rev = phrase.reversed();
                let pos_rev = ins_pos.reversed(phrase);
                let match_bef = self.match_before_env(&before_expt, &phrase_rev, &pos_rev, false, true, false)?;
                let match_aft = self.match_after_env(after_expt, phrase, &ins_pos, false, false, false)?;

                Ok(match_bef && match_aft)
            },
        }
    }

    fn insertion_match(&self, phrase: &Phrase, start_pos: SegPos) -> Result<Option<SegPos>, RuleRuntimeError> {
        let empty = Vec::new();
        let context = self.get_contexts();
        let exceptions = self.get_exceptions();

        let ((before_cont, center_cont, after_cont), (before_expt, center_expt, after_expt)) = match (context.len().cmp(&1), exceptions.len().cmp(&1)) {
            (std::cmp::Ordering::Equal,  std::cmp::Ordering::Less) => (context[0],(&empty, &None, &empty)),
            (std::cmp::Ordering::Equal, std::cmp::Ordering::Equal) => (context[0], exceptions[0]),
            (std::cmp::Ordering::Less,  std::cmp::Ordering::Equal) => ((&empty, &None, &empty), exceptions[0]),
            (std::cmp::Ordering::Less,   std::cmp::Ordering::Less) => return Err(RuleRuntimeError::InsertionNoEnv(self.output.last().unwrap().position)),
            (std::cmp::Ordering::Greater, _) => return Err(RuleRuntimeError::InsertionGroupedEnv(self.context.clone().unwrap().position)),
            (_, std::cmp::Ordering::Greater) => return Err(RuleRuntimeError::InsertionGroupedEnv(self.except.clone().unwrap().position)),
        };

        if before_cont.is_empty() && after_cont.is_empty() && center_cont.is_none() && before_expt.is_empty() && after_expt.is_empty() && center_expt.is_none() {
            return Err(RuleRuntimeError::InsertionNoEnv(self.output.last().unwrap().position))
        }

        // TODO: test this
        if center_cont.is_some() {
            unreachable!("We only enter this function when matching by segment")
        }
        let maybe_ins = match (before_cont.is_empty(), after_cont.is_empty()) {
            (true, true) => Some(start_pos), // _
            (false, true) => self.insertion_after(before_cont, phrase, start_pos)?, // #_
            (true, false) => self.insertion_before(after_cont, phrase, start_pos)?, // _#
            (false, false) => self.insertion_between(before_cont, after_cont, phrase, start_pos)?, // #_#
        };

        Ok(maybe_ins)
    }

    fn insertion_between(&self, bef_states: &[ParseItem], aft_states: &[ParseItem], phrase: &Phrase, start_pos: SegPos) -> Result<Option<SegPos>, RuleRuntimeError> {
        let mut start_pos = start_pos;
        
        // FIXME: This is scuffed
        'outer: while phrase.in_bounds(start_pos) {
            match self.insertion_after(bef_states, phrase, start_pos)? {
                Some(mut ins_pos) => {
                    let mut pos = ins_pos;
                    let mut state_index = 0;
                    start_pos = ins_pos;
                    while state_index < aft_states.len() {
                        if !self.context_match(aft_states, &mut state_index, phrase, &mut pos, true, false, false)? {
                            match bef_states.last().unwrap().kind {
                                ParseElement::WordBound => return Ok(None),
                                ParseElement::SyllBound => start_pos.increment(phrase),
                                _ => {}
                            }
                            continue 'outer;
                        }
                        state_index +=1;
                    }
                    // Fix for insertion before a mid-word syllable boundary
                    if let ParseElement::SyllBound | ParseElement::Structure(..) = aft_states[0].kind && ins_pos.at_syll_start() {
                        ins_pos.decrement(phrase);
                        ins_pos.seg_index += 1;
                    }
                    return Ok(Some(ins_pos)) 
                },
                None => return Ok(None),
            }
        }

        Ok(None)
    }

    fn insertion_after(&self, states: &[ParseItem], phrase: &Phrase, start_pos: SegPos) -> Result<Option<SegPos>, RuleRuntimeError> {
        // i.e. #_
        let mut cur_pos = start_pos;
        let mut state_index = 0;
        let mut match_begin = None;

        debug_assert!(!states.is_empty());

        if states[0].kind == ParseElement::WordBound {
            if !start_pos.at_word_start() {
                return Ok(None)
            }
            if states.len() == 1 {
                return Ok(Some(start_pos))
            }
            state_index = 1;
        }

        while phrase.in_bounds(cur_pos) {
            if self.context_match(states, &mut state_index, phrase, &mut cur_pos, true, false, false)? {
                if state_index >= states.len() - 1 {
                    return Ok(Some(cur_pos))
                }
                if match_begin.is_none() {
                    match_begin = Some(cur_pos);
                }
                state_index += 1;
            } else if let Some(mb) = match_begin{
                cur_pos = mb;
                cur_pos.increment(phrase);
                state_index = 0;
                match_begin = None;
                self.alphas.borrow_mut().clear();
                self.references.borrow_mut().clear();
            } else {
                cur_pos.increment(phrase);
                self.alphas.borrow_mut().clear();
                self.references.borrow_mut().clear();
                state_index = 0;
            }
        }

        if match_begin.is_none() {
            if let ParseElement::SyllBound = states.last().expect("states is not empty").kind {
                let sy = phrase[cur_pos.word_index].syllables.len() - 1;
                let sg = phrase[cur_pos.word_index].syllables[sy].segments.len();
                Ok(Some(SegPos::new(cur_pos.word_index, sy, sg)))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    fn insertion_before(&self, states: &[ParseItem], phrase: &Phrase, start_pos: SegPos) -> Result<Option<SegPos>, RuleRuntimeError> {
        // i.e. _#
        let mut cur_pos = start_pos;
        let mut state_index = 0;
        let mut match_begin = None;

        debug_assert!(!states.is_empty());

        while phrase.in_bounds(cur_pos) {
            let before_pos = cur_pos;
            if self.context_match(states, &mut state_index, phrase, &mut cur_pos, true, true, false)? {
                if match_begin.is_none() {
                    let mut sp = before_pos;
                    if let ParseElement::Syllable(..) | ParseElement::SyllBound = states[0].kind {
                        sp.decrement(phrase);
                        sp.seg_index +=1;
                    }
                    match_begin = Some(sp);
                }
                if state_index >= states.len() - 1 {
                    return Ok(match_begin)
                }
                state_index += 1;
            } else if let Some(mb) = match_begin{
                cur_pos = mb;
                cur_pos.increment(phrase);
                state_index = 0;
                match_begin = None;
                self.alphas.borrow_mut().clear();
                self.references.borrow_mut().clear();
            } else {
                cur_pos.increment(phrase);
                self.alphas.borrow_mut().clear();
                self.references.borrow_mut().clear();
                state_index = 0;
            }
        }

        if match_begin.is_none() {
            if let ParseElement::WordBound | ParseElement::SyllBound | ParseElement::Structure(..) = states[0].kind {
                let sy = phrase[cur_pos.word_index].syllables.len() - 1;
                let sg = phrase[cur_pos.word_index].syllables[sy].segments.len();
                Ok(Some(SegPos::new(cur_pos.word_index, sy, sg)))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }
}