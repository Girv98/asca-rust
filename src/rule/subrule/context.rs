use super :: {
    Modifiers, ParseElement, ParseItem, Position, SpecMod, UnderlineStruct, MatchElement, RefKind, SubRule, 
};

use crate :: {
    error :: RuleRuntimeError, 
    rule  :: { ItemSet, Reference, SetChoice }, 
    word  :: { Phrase, SegPos, Segment, Syllable, Tone }
};


impl SubRule {
    pub(super) fn get_contexts(&self) -> Vec<(&Vec<ParseItem>, &Option<UnderlineStruct>, &Vec<ParseItem>)> {
        match &self.context {
            Some(item) => item.envs.iter().map(|e| (&e.before, &e.center, &e.after)).collect(),
            None => vec![],
        }
    }

    pub(super) fn get_exceptions(&self) -> Vec<(&Vec<ParseItem>, &Option<UnderlineStruct>, &Vec<ParseItem>)> {
        match &self.except {
            Some(item) => item.envs.iter().map(|e| (&e.before, &e.center, &e.after)).collect(),
            None => vec![],
        }
    }

    pub(super) fn context_match(&self, states: &[ParseItem], state_index: &mut usize, phrase: &Phrase, pos: &mut SegPos, forwards: bool, ins_match_before: bool, within_struct: Option<usize>, negate: bool) -> Result<bool, RuleRuntimeError> {
        let state = &states[*state_index];
        match &state.kind {
            ParseElement::SyllBound | ParseElement::WordBound | ParseElement::ExtlBound if within_struct.is_some() => Err(RuleRuntimeError::BoundaryInsideStruct(state.position)),

            ParseElement::Structure(..) if within_struct.is_some() => Err(RuleRuntimeError::StructInsideStruct(state.position)),
            ParseElement::Syllable (..) if within_struct.is_some() => Err(RuleRuntimeError::SyllbleInsideStruct(state.position)),

            ParseElement::WordBound => Ok(phrase[pos.word_index].out_of_bounds(*pos)),
            ParseElement::SyllBound => if ins_match_before {
                Ok(!pos.at_word_start() && pos.at_syll_start())
            } else {
                Ok(pos.at_syll_start())
            },
            ParseElement::Ipa(s, m) => if self.context_match_ipa(s, m, phrase, *pos, state.position, negate)? {
                self.matrix_increment(phrase, pos);
                pos.increment(phrase); 
                Ok(true)
            } else { Ok(false) },
            ParseElement::Matrix(m, v) => self.context_match_matrix(m, v, phrase, pos, state.position, negate),
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
            ParseElement::Reference(vt, mods) => self.context_match_ref(vt, mods, phrase, pos, forwards, state.position, within_struct, negate),
            ParseElement::Set(s) => self.context_match_set(s, phrase, pos, forwards, within_struct),
            ParseElement::Optional(opt_states, min, max) => self.context_match_option(states, state_index, phrase, pos, forwards, opt_states, *min, *max, within_struct),
            ParseElement::Ellipsis => self.context_match_ellipsis(states, state_index, phrase, pos, forwards, true, within_struct),
            ParseElement::OptEllipsis => self.context_match_ellipsis(states, state_index, phrase, pos, forwards, false, within_struct),
            
            ParseElement::ExtlBound => if phrase[pos.word_index].out_of_bounds(*pos) && !pos.at_phrase_end(phrase) {
                pos.word_increment(phrase);
                Ok(true)
            } else { Ok(false) },

            ParseElement::Negation(item) => Ok(self.context_match(&[*item.clone()], &mut 0, phrase, pos, forwards, ins_match_before, within_struct, true)?),

            ParseElement::EmptySet | ParseElement::Metathesis | ParseElement::MetaOrdered => unreachable!(),
        }
    }

    pub(super) fn match_before_env(&self, states: &[ParseItem], phrase_rev: &Phrase, pos: &SegPos, ins_match_before: bool, inc: bool, is_context: bool) -> Result<bool, RuleRuntimeError> {
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
            if !self.context_match(states, &mut si, phrase_rev, &mut start_pos, false, ins_match_before, None, false)? {
                is_match = false;
                if is_context { break; }
            }
            si += 1;
        }
        Ok(is_match)
    }

    pub(super) fn match_after_env(&self, states: &[ParseItem], phrase: &Phrase, pos: &SegPos, ins_match_before: bool, inc: bool, is_context: bool) -> Result<bool, RuleRuntimeError> {
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
            if !self.context_match(states, &mut si, phrase, &mut start_pos, true, ins_match_before, None, false)? {
                is_match = false;
                if is_context { break; }
            }
            si += 1;
        }

        Ok(is_match)
    }

    pub(super) fn match_underline_struct_items(&self, phrase: &Phrase, items: &[ParseItem], pos: SegPos, forwards: bool, ins_info: &mut Option<(usize, &mut SegPos)>) -> Result<bool, RuleRuntimeError> {
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
                } else { return Ok(false) }
                ParseElement::Ipa(s, mods) => if self.context_match_ipa(s, mods, phrase, pos, item.position, false)? { // TODO
                    self.matrix_increment(phrase, &mut pos);
                    pos.increment(phrase);
                } else { return Ok(false) }
                ParseElement::Matrix(mods, refr) => if !self.context_match_matrix(mods, refr, phrase, &mut pos, item.position, false)? { // TODO
                    return Ok(false) 
                }
                // NOTE: since syllables are invalid, passing `forwards` won't matter
                ParseElement::Reference(num, mods) => if !self.context_match_ref(num, mods, phrase, &mut pos, forwards, item.position, Some(cur_syll_index), false)? { // TODO
                   return Ok(false) 
                }
                ParseElement::Set(set) => if !self.context_match_set(set, phrase, &mut pos, forwards, Some(cur_syll_index))? {
                    return Ok(false) 
                }
                ParseElement::Optional(states, min, max) => if self.context_match_option(items, &mut i, phrase, &mut pos, forwards, states, *min, *max, Some(cur_syll_index))? {
                    break;
                } else { return Ok(false) }
                ParseElement::Negation(item) => if self.context_match(&[*item.clone()], &mut 0, phrase, &mut pos, forwards, false, Some(cur_syll_index), true)? {
                } else { return Ok(false) }

                
                ParseElement::SyllBound | ParseElement::WordBound | ParseElement::ExtlBound => unreachable!(),

                ParseElement::MetaOrdered  | ParseElement::Metathesis | ParseElement::EmptySet |
                ParseElement::Syllable(..) | ParseElement::Structure(..) => unreachable!()
            }
            // TODO FIXME: This sucks, bef_len does not need to be mutable
            if let Some((bef_len, ins_pos)) = ins_info && *bef_len - 1 == i {
                **ins_pos = pos;
            }
        }
        Ok(true)
    }

    fn underline_struct_sanity_check(&self, matches: &[MatchElement]) -> Result<(), RuleRuntimeError> {
        for (i, m) in matches.iter().enumerate() {
            match m {
                &MatchElement::Syllable (.., err_pos) => {
                    return Err(RuleRuntimeError::BoundaryInsideStruct(err_pos))
                }
                
                &MatchElement::SyllBound(.., err_pos) | &MatchElement::WordBound(.., err_pos) => {
                    if i != 0 && matches.len() != i + 1  {
                        return Err(RuleRuntimeError::BoundaryInsideStruct(err_pos))
                    }
                }

                MatchElement::LongSegment(..) | MatchElement::Segment(..) => {}

                MatchElement::Set(els, _, _) => return self.underline_struct_sanity_check(els)
            }
        }

        Ok(())
    } 

    fn boundary_check(&self, phrase: &Phrase, center: &UnderlineStruct, start_pos: SegPos, end_pos: SegPos) -> bool {

        if center.before.is_empty() && (start_pos.seg_index != 0) {
            return false
        }

        if center.after.is_empty() 
        && !(end_pos.at_syll_end(phrase) && end_pos.syll_index == start_pos.syll_index) // end of same syllable as start_pos
        && !(end_pos.seg_index == 0 && self.last_is_boundary(&self.input)) {            // last input item is a boundary
            return false
        }

        true
    }

    fn match_underline_struct(&self, phrase_rev: &Phrase, phrase: &Phrase, matches: &[MatchElement], start_pos: SegPos, end_pos: SegPos, center: &UnderlineStruct) -> Result<bool, RuleRuntimeError> {
        
        { // Sanity Check Input
            // Check first and last input are in the same syllable
            // if start_pos.word_index != end_pos.word_index 
            // || !(
            //     start_pos.syll_index == end_pos.syll_index 
            //     || ( // check if end is beginning of next syllable (i.e. for boundaries)
            //         start_pos.syll_index + 1 == end_pos.syll_index 
            //         && end_pos.seg_index == 0
            //     ) 
            // ) { return Ok(false) }
            self.underline_struct_sanity_check(matches)?;
        }

        // Check Suprasegmentals
        let syll = &phrase[start_pos.word_index].syllables[start_pos.syll_index];
        if let Some(tone) = center.tone && !self.match_tone(&tone, syll) { return Ok(false) }
        if !self.match_stress(&center.stress, syll, center.position)? { return Ok(false) }

        // Check that boundaries of structure make sense
        if !self.boundary_check(phrase, center, start_pos, end_pos) { return Ok(false) }

        // Increment start_pos and end_pos before matching
        let cur_syll_index = start_pos.syll_index;
        let mut start_pos_rev = start_pos.reversed(phrase);
        let cur_syll_index_rev = start_pos_rev.syll_index;
        let mut end_pos = end_pos;
        // patch for matching syllable initial TODO: why
        if !center.before.is_empty() {
            start_pos_rev.increment(phrase_rev);
        }
        end_pos.increment(phrase);

        // Check we are not outside syllable (for an invalid reason)
        // If we are outside the syllable, but on that side there is only an 0-optional, we can 
        // safely skip matching that side
        let mut skip_pre = false;
        if start_pos_rev.syll_index != cur_syll_index_rev && let Some(x) = center.before.first() {
            if let ParseElement::OptEllipsis | ParseElement::Optional(_,0,_) = x.kind {
                if center.before.len() == 1 {skip_pre = true}
            } else {
                return Ok(false)
            }
        }
        let mut skip_pst = false;
        if end_pos.syll_index != cur_syll_index && let Some(x) = center.after.first() {
            if let ParseElement::OptEllipsis | ParseElement::Optional(_,0,_) = x.kind {
                if center.after.len() == 1 {skip_pst = true}
            } else {
                return Ok(false)
            }
        }

        if phrase[start_pos.word_index].syllables.len() > 1 && center.before.is_empty() && start_pos_rev.syll_index == cur_syll_index {
            return Ok(false)
        }

        if phrase[start_pos.word_index].syllables.len() > 1 && center.after.is_empty() && end_pos.syll_index == cur_syll_index {
            return Ok(false)
        }

        Ok(
            (skip_pre || self.match_underline_struct_items(phrase_rev, &center.before, start_pos_rev, false, &mut None)?)
         && (skip_pst || self.match_underline_struct_items(phrase, &center.after, end_pos, true, &mut None)?)
        )
    }

    pub(super) fn match_contexts_and_exceptions(&self, phrase: &Phrase, matches: &[MatchElement], start_pos: SegPos, end_pos: SegPos, inc_start:bool, inc_end: bool) -> Result<bool, RuleRuntimeError> {
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
                    if start_pos.syll_index == 0 && (!bef_cont_states.is_empty() && !self.is_boundary(bef_cont_states)) { break }
                    if end_pos.syll_index == phrase[end_pos.word_index].syllables.len() - 1 && (!aft_cont_states.is_empty() && !self.is_boundary(aft_cont_states)) { break }
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
                    if start_pos.syll_index == 0 && (!bef_expt_states.is_empty() && !self.is_boundary(bef_expt_states)) { break }
                    if end_pos.syll_index == phrase[end_pos.word_index].syllables.len() - 1 && (!aft_expt_states.is_empty() && !self.is_boundary(aft_expt_states)) { break }
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

    fn is_boundary(&self, items: &[ParseItem]) -> bool {
        if items.len() > 1 { return false }

        match &items[0].kind {
            ParseElement::WordBound | ParseElement::SyllBound | ParseElement::ExtlBound => true,

            ParseElement::Set(choices) if choices.contains_only(&ParseElement::WordBound).is_some() => true,
            ParseElement::Set(choices) if choices.contains_only(&ParseElement::SyllBound).is_some() => true,
            ParseElement::Set(choices) if choices.contains_only(&ParseElement::ExtlBound).is_some() => true,

            _ => false
        }
    }

    fn last_is_boundary(&self, items: &[ParseItem]) -> bool {
        match items.last() {
            Some(x) => match &x.kind {
                ParseElement::WordBound | ParseElement::SyllBound | ParseElement::ExtlBound => true,

                ParseElement::Set(choices) if choices.contains_only(&ParseElement::WordBound).is_some() => true,
                ParseElement::Set(choices) if choices.contains_only(&ParseElement::SyllBound).is_some() => true,
                ParseElement::Set(choices) if choices.contains_only(&ParseElement::ExtlBound).is_some() => true,

                _ => false
            },
            None => false,
        }
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
                } else { return Ok(false) }
                ParseElement::OptEllipsis => if i == items.len() - 1 {
                    // if last item, jump to end of syll and break loop
                    pos.syll_index += 1;
                    pos.seg_index = 0;
                    break;
                } else if self.context_match_ellipsis_struct(&items, &mut i, phrase, pos, cur_syll_index, forwards,false, &mut None)? {
                    break;
                } else { return Ok(false) }
                ParseElement::Ipa(s, mods) => if self.context_match_ipa(s, mods, phrase, *pos, item.position, false)? { // TODO
                    self.matrix_increment(phrase, pos);
                    pos.increment(phrase);
                } else { return Ok(false) }
                ParseElement::Matrix(mods, refr) => if !self.context_match_matrix(mods, refr, phrase, pos, item.position, false)? { // TODO
                    return Ok(false) 
                }
                // NOTE: since syllables are invalid, passing `forwards` won't matter
                ParseElement::Reference(num, mods) => if !self.context_match_ref(num, mods, phrase, pos, forwards, item.position, Some(cur_syll_index), false)? { // TODO
                   return Ok(false) 
                }
                ParseElement::Set(set) => if !self.context_match_set(set, phrase, pos, forwards, Some(cur_syll_index))? {
                    return Ok(false) 
                }
                ParseElement::Optional(states, min, max) => if self.context_match_option(&items, &mut i, phrase, pos, forwards, states, *min, *max, Some(cur_syll_index))? {
                    break;
                } else { return Ok(false) },
                ParseElement::Negation(item) => if self.context_match(&[*item.clone()], &mut 0, phrase, pos, forwards, false, Some(cur_syll_index), true)? {
                } else { return Ok(false) },

                ParseElement::SyllBound | ParseElement::WordBound | ParseElement::ExtlBound => unreachable!(),

                ParseElement::MetaOrdered  | ParseElement::Metathesis | ParseElement::EmptySet |
                ParseElement::Syllable(..) | ParseElement::Structure(..) => unreachable!()
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
    pub(super) fn context_match_ellipsis_struct(&self, items: &[ParseItem], index: &mut usize, phrase: &Phrase, pos: &mut SegPos, syll_index: usize, forwards: bool, inc: bool, ins_info: &mut Option<(usize, &mut SegPos)>) -> Result<bool, RuleRuntimeError> {
        
        if !phrase.in_bounds(*pos) { return Ok(false) }

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
                    ParseElement::Ipa(s, mods) => if self.context_match_ipa(s, mods, phrase, *pos, items[*index].position, false)? { // TODO
                        self.matrix_increment(phrase, pos);
                        pos.increment(phrase);
                    } else { m = false; break; },
                    ParseElement::Matrix(mods, refr) => if !self.context_match_matrix(mods, refr, phrase, pos, items[*index].position, false)? { // TODO
                        m = false; break;
                    },
                    // since syllables are invalid, passing `forwards` won't matter
                    ParseElement::Reference(num, mods) => if !self.context_match_ref(num, mods, phrase, pos, forwards, items[*index].position, Some(syll_index), false)? { // TODO
                        m = false; break;
                    },
                    ParseElement::Set(set) => if !self.context_match_set(set, phrase, pos, forwards, Some(syll_index))? {
                        m = false; break;
                    },
                    ParseElement::Optional(states, min, max) => if self.context_match_option(items, index, phrase, pos, forwards, states, *min, *max,Some(syll_index))? {
                        return Ok(true)
                    } else {m = false; break; },
                    
                    ParseElement::Negation(item) => if !self.context_match(&[*item.clone()], &mut 0, phrase, pos, forwards, false, Some(syll_index), true)? {
                        m = false; break;
                    },

                    ParseElement::SyllBound | ParseElement::WordBound | ParseElement::ExtlBound => unreachable!(),

                    ParseElement::MetaOrdered  | ParseElement::Metathesis | ParseElement::EmptySet |
                    ParseElement::Syllable(..) | ParseElement::Structure(..) => unreachable!()
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

    fn context_match_ellipsis(&self, states: &[ParseItem], state_index: &mut usize, phrase: &Phrase, pos: &mut SegPos, forwards: bool, inc: bool, within_struct: Option<usize>) -> Result<bool, RuleRuntimeError> {
        
        *state_index += 1;
        if inc { pos.increment(phrase) }
        
        if *state_index >= states.len() {
            if let Some(cur) = within_struct {
                pos.syll_index = cur + 1;
                pos.seg_index = 0;
                return Ok(true)
            } else {
                return Ok(!inc || phrase.in_bounds(*pos))
            }
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
                if !self.context_match(states, state_index, phrase, pos, forwards, false, None, false)? {
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

    fn match_opt_states(&self, opt_states: &[ParseItem], phrase: &Phrase, pos: &mut SegPos, forwards: bool, within_struct: Option<usize>) -> Result<bool, RuleRuntimeError> {
        let mut si = 0;
        while si < opt_states.len() {
            if !self.context_match(opt_states, &mut si, phrase, pos, forwards, false, within_struct, false)? {
                return Ok(false)
            }
            si += 1;
        }
        Ok(true)
    }

    // This is an absolute mess
    pub(super) fn context_match_option(&self, states: &[ParseItem], state_index: &mut usize, phrase: &Phrase, pos: &mut SegPos, forwards: bool, opt_states: &[ParseItem], match_min: usize, match_max: usize, within_struct: Option<usize>) -> Result<bool, RuleRuntimeError> {
        // should work like regex (...){min, max}? 
        let match_max = if match_max == 0 { None } else { Some(match_max) };
        let back_pos = *pos;
        let back_alphas = self.alphas.borrow().clone();
        let back_refs = self.references.borrow().clone();

        let start_syll = pos.syll_index;

        if let Some(cur) = within_struct && cur != start_syll {
            return Ok(false)
        }

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

        {
            let mut m = true;
            while *state_index < states.len() {
                if !self.context_match(states, state_index, phrase, pos, forwards, false, within_struct, false)? {
                    m = false;
                    break;
                }
                *state_index += 1;
            }
            if m {
                if let Some(cur) = within_struct {
                    if pos.syll_index == cur + 1 && pos.seg_index == 0 {
                        return Ok(true)
                    }
                } else {
                    return Ok(true)
                }
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
                    if !self.context_match(states, state_index, phrase, pos, forwards, false, within_struct, false)? {
                        m = false;
                        break;
                    }
                    *state_index += 1;
                }
                if within_struct.is_none() && m {                    
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

    fn context_match_set_choice_item(&self, s: &ParseItem, phrase: &Phrase, pos: &mut SegPos, forwards: bool, within_struct: Option<usize>) -> Result<bool, RuleRuntimeError> {
        match &s.kind {
                ParseElement::SyllBound if within_struct.is_some() => Err(RuleRuntimeError::BoundaryInsideStruct(s.position)),
                ParseElement::WordBound if within_struct.is_some() => Err(RuleRuntimeError::BoundaryInsideStruct(s.position)),
                // Since the lexer doesn't allow nesting, this will never happen. But, it's nice to have.
                ParseElement::Structure(..) if within_struct.is_some() => Err(RuleRuntimeError::StructInsideStruct(s.position)),
                ParseElement::Syllable (..) if within_struct.is_some() => Err(RuleRuntimeError::SyllbleInsideStruct(s.position)),

                ParseElement::Structure(items, stress, tone, refr) => self.context_match_structure(items, stress, tone, refr, phrase, pos, forwards, s.position),

                ParseElement::Reference(vt, mods) => self.context_match_ref(vt, mods, phrase, pos, forwards, s.position, within_struct, false), // TODO
                ParseElement::Ipa(seg, mods) => if self.context_match_ipa(seg, mods, phrase, *pos, s.position, false)? { // TODO
                    self.matrix_increment(phrase, pos);
                    pos.increment(phrase);
                    Ok(true)
                } else {Ok(false)},
                ParseElement::Matrix(mods, refr) => self.context_match_matrix(mods, refr, phrase, pos, s.position, false), // TODO
                ParseElement::Syllable(stress, tone, refr) => self.context_match_syll(stress, tone, refr, phrase, pos, forwards, s.position),
                ParseElement::WordBound => Ok(phrase[pos.word_index].out_of_bounds(*pos)),
                ParseElement::SyllBound => Ok(pos.at_syll_start()),
                _ => unimplemented!(),
        }
    }

    fn context_match_set_choice(&self, choice: &SetChoice, phrase: &Phrase, pos: &mut SegPos, forwards: bool, within_struct: Option<usize>) -> Result<bool, RuleRuntimeError> {
        let back_pos= *pos;
        let back_alphas = self.alphas.borrow().clone();
        let back_refs = self.references.borrow().clone();

        for item in &choice.items {
            if !self.context_match_set_choice_item(item, phrase, pos, forwards, within_struct)? {
                *pos = back_pos;
                *self.alphas.borrow_mut() = back_alphas.clone();
                *self.references.borrow_mut() = back_refs.clone();
                return Ok(false);
            }
        }

        Ok(true)
    }

    pub(super) fn context_match_set(&self, set: &ItemSet, phrase: &Phrase, pos: &mut SegPos, forwards: bool, within_struct: Option<usize>) -> Result<bool, RuleRuntimeError> {
        let back_pos= *pos;
        let back_alphas = self.alphas.borrow().clone();
        let back_refs = self.references.borrow().clone();


        for choice in &set.choices {
            if self.context_match_set_choice(choice, phrase, pos, forwards, within_struct)? {
                return Ok(true)
            }
            *pos = back_pos;
            *self.alphas.borrow_mut() = back_alphas.clone();
            *self.references.borrow_mut() = back_refs.clone();
        }

        Ok(false)

        
        // for s in &set.items {
        //     let res = match &s.kind {
        //         ParseElement::SyllBound if within_struct => Err(RuleRuntimeError::BoundaryInsideStruct(s.position)),
        //         ParseElement::WordBound if within_struct => Err(RuleRuntimeError::BoundaryInsideStruct(s.position)),
        //         // Since the lexer doesn't allow nesting, this will never happen. But, it's nice to have.
        //         ParseElement::Structure(..) if within_struct => Err(RuleRuntimeError::StructInsideStruct(s.position)),
        //         ParseElement::Syllable (..) if within_struct => Err(RuleRuntimeError::SyllbleInsideStruct(s.position)),

        //         ParseElement::Structure(items, stress, tone, refr) => self.context_match_structure(items, stress, tone, refr, phrase, pos, forwards, s.position),

        //         ParseElement::Reference(vt, mods) => self.context_match_ref(vt, mods, phrase, pos, forwards, s.position, within_struct),
        //         ParseElement::Ipa(seg, mods) => if self.context_match_ipa(seg, mods, phrase, *pos, s.position)? {
        //             self.matrix_increment(phrase, pos);
        //             pos.increment(phrase);
        //             Ok(true)
        //         } else {Ok(false)},
        //         ParseElement::Matrix(mods, refr) => self.context_match_matrix(mods, refr, phrase, pos, s.position),
        //         ParseElement::Syllable(stress, tone, refr) => self.context_match_syll(stress, tone, refr, phrase, pos, forwards, s.position),
        //         ParseElement::WordBound => Ok(phrase[pos.word_index].out_of_bounds(*pos)),
        //         ParseElement::SyllBound => Ok(pos.at_syll_start()),
        //         _ => unimplemented!(),
        //     };
        //     if res? {
        //         return Ok(true)
        //     }
        //     *pos = back_pos;
        //     // TODO: Deal with these clones
        //     *self.alphas.borrow_mut() = back_alphas.clone();
        //     *self.references.borrow_mut() = back_refs.clone();
        // }
        // Ok(false)
    }

    fn context_match_ref(&self, refr: &Reference, mods: &Option<Modifiers>, phrase: &Phrase, pos: &mut SegPos, forwards: bool, err_pos: Position, within_struct: Option<usize>, negate: bool) -> Result<bool, RuleRuntimeError> {
        if let Some(rk) = self.references.borrow().get(&refr.value) {
            match rk {
                RefKind::Segment(s) => if self.context_match_ipa(s, mods, phrase, *pos, err_pos, negate)? {
                    self.matrix_increment(phrase, pos);
                    pos.increment(phrase);
                    Ok(true)
                } else { Ok(false) },
                RefKind::Syllable(_) if within_struct.is_some() => Err(RuleRuntimeError::SyllRefInsideStruct(refr.position)),
                RefKind::Syllable(s) => self.context_match_syll_ref(s, mods, phrase, pos, forwards, err_pos, negate),
            }            
        } else {
            Err(RuleRuntimeError::UnknownReference(*refr))
        }
    }

    fn context_match_syll_ref(&self, syll_to_match: &Syllable, mods: &Option<Modifiers>, phrase: &Phrase, pos: &mut SegPos, forwards: bool, err_pos: Position, negate: bool) -> Result<bool, RuleRuntimeError> {
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
            if !self.match_stress(&suprs.stress, cur_syll, err_pos)? && !negate {
                return Ok(false)
            }
            if let Some(t) = suprs.tone.as_ref() && !self.match_tone(t, cur_syll) && !negate {
                return Ok(false)
            }
            if cur_syll.segments != syll_to_match.segments && !negate {
                return Ok(false)
            }
        } else if (cur_syll.segments != segs_to_match || cur_syll.stress != syll_to_match.stress || cur_syll.tone != syll_to_match.tone) && !negate {
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

    pub(super) fn context_match_ipa(&self, s: &Segment, mods: &Option<Modifiers>, phrase: &Phrase, pos: SegPos, err_pos: Position, negate: bool) -> Result<bool, RuleRuntimeError> {
        let Some(seg) = phrase.get_seg_at(pos) else { return Ok(false) };
        if let Some(m) = mods {
            let mod_match = self.match_ipa_with_modifiers(s, m, phrase, &pos, err_pos)?;
            Ok((!negate && mod_match) || (negate && !mod_match))
        } else {
            Ok((!negate && *s == seg) || (negate && *s != seg))
        }
    }

    pub(super) fn context_match_matrix(&self, mods: &Modifiers, refr: &Option<usize>, phrase: &Phrase, pos: &mut SegPos, err_pos: Position, negate: bool) -> Result<bool, RuleRuntimeError> {        
        if phrase[pos.word_index].out_of_bounds(*pos) { return Ok(false) }
        
        let mod_match = self.match_modifiers(mods, phrase, pos, err_pos)?;
        if (!negate && mod_match) || (negate && !mod_match) {
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
