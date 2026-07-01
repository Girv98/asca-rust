use super :: {
    Modifiers, ParseElement, ParseItem, Position, SpecMod, MatchElement, RefKind, SubRule
};

use crate :: {
    error :: RuleRuntimeError, 
    rule  :: { ItemSet, Reference, SetChoice },
    word  :: { Phrase, SegPos, Segment, Syllable, Tone }
};


impl SubRule { 

    fn input_calc_match_begin(&self, phrase: &Phrase, el: &MatchElement) -> SegPos {
        match el {
            MatchElement::Set(els, _, _) => self.input_calc_match_begin(phrase, &els[0]),
            MatchElement::Segment(sp, _) => *sp,
            MatchElement::LongSegment(sp, _) => SegPos { 
                word_index: sp.word_index, 
                syll_index: sp.syll_index, 
                seg_index: sp.seg_index + phrase.seg_length_at(*sp) - 1 
            },
            MatchElement::SyllBound(wp, sp, _) |
            MatchElement::Syllable (wp, sp, _) => SegPos { 
                word_index: *wp, 
                syll_index: *sp, 
                seg_index: 0 
            },
            MatchElement::WordBound(wp, _) => SegPos { 
                word_index: wp+1, 
                syll_index: 0, 
                seg_index: 0 
            },
        }
    }

    pub(super) fn input_match_at(&self, phrase: &Phrase, start_index: SegPos, state_index: usize) -> Result<(Vec<MatchElement>, Option<SegPos>), RuleRuntimeError> {
        let mut cur_index = start_index;
        let mut match_begin = None;
        let mut state_index = state_index;
        let mut captures: Vec<_> = Vec::new();

        while phrase.in_bounds(cur_index) {
            if self.input_match_item(&mut captures, &mut cur_index, &mut state_index, phrase, &self.input, false)? {
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
                    debug_assert!(!captures.is_empty(), "Captures array should not be empty if we have started matching");
                    match_begin = Some(self.input_calc_match_begin(phrase, captures.first().unwrap()));
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

        if self.input_check_end_of_word_match(phrase, &mut captures, match_begin, state_index)? {
            Ok((captures, None))
        } else {
            Ok((vec![], None))
        }
    }

    fn input_check_end_of_word_match(&self, phrase: &Phrase, captures: &mut Vec<MatchElement>, match_begin: Option<SegPos>, state_index: usize) -> Result<bool, RuleRuntimeError> {
        // if we've got to the end of the word and we haven't began matching
        if match_begin.is_none() {
            if self.input.len() == 1 && self.input[0].kind == ParseElement::SyllBound {
                captures.push(MatchElement::SyllBound(phrase.len()-1, phrase[phrase.len()-1].syllables.len(), self.input[0].position));
                return Ok(true)
            } else if self.input.len() == 1 && self.input_check_set_syll_bound(phrase, captures) {
                return Ok(true)
            } else {
                return Ok(false)
            }
        }
        
        if state_index < self.input.len() - 1 {
            return Ok(false)
        }

        // if we're matching and we've reached the end of the word and the last state is a syll boundary
        if let Some(last_item) = self.input.last() && last_item.kind == ParseElement::SyllBound {
            captures.push(MatchElement::SyllBound(phrase.len()-1, phrase[phrase.len()-1].syllables.len(), last_item.position));
            return Ok(true)
        }

        // if we're matching and we've reached the end of the word and the last state is a set containing a syll boundary
        if self.input_check_set_syll_bound(phrase, captures) {
            return Ok(true)
        }

        // No Match
        Ok(false)
    }

    fn input_check_set_syll_bound(&self, phrase: &Phrase, captures: &mut Vec<MatchElement>) -> bool {
        match &self.input.last() {
            Some(ParseItem { kind: ParseElement::Set(set), position}) => {
                match captures.last_mut() {
                    Some(MatchElement::Set(els, choice, _)) => {
                        if set.ends_in(ParseElement::SyllBound, *choice) {
                            els.push(MatchElement::SyllBound(phrase.len()-1, phrase[phrase.len()-1].syllables.len(), set.choices[*choice].items.last().unwrap().position));
                            true
                        } else {
                            false
                        }
                    }
                    _ => {
                        for (c, choice) in set.choices.iter().enumerate() {
                            if choice.items.len() == 1 && set.ends_in(ParseElement::SyllBound, c) {
                                captures.push(MatchElement::Set(
                                    vec![MatchElement::SyllBound(phrase.len()-1, phrase[phrase.len()-1].syllables.len(), choice.items.last().unwrap().position)],
                                    c,
                                    *position
                                ));
                                return true
                            }
                        }

                        false
                    }
                }
            },
            _ => false,
        }
    }

    fn input_match_item(&self, captures: &mut Vec<MatchElement>, seg_pos: &mut SegPos, state_index: &mut usize, phrase: &Phrase, states: &[ParseItem], negate: bool) -> Result<bool, RuleRuntimeError> {
        let err_pos = states[*state_index].position;
        match &states[*state_index].kind {
            ParseElement::Reference(refr, m) => if self.input_match_ref(captures, state_index, refr, m, phrase, seg_pos, err_pos, negate)? {
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseElement::Ipa(s, m) => if self.input_match_ipa(captures, s, m, phrase, seg_pos, err_pos, negate)? {
                seg_pos.increment(phrase);
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseElement::Matrix(m, v) => if self.input_match_matrix(captures, m, v, phrase, seg_pos, err_pos, negate)? {
                seg_pos.increment(phrase);
                *state_index += 1;
                Ok(true) 
            } else { Ok(false) },
            ParseElement::Set(s) => if self.input_match_set(captures, state_index, s, phrase, seg_pos, err_pos)? {
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseElement::SyllBound => if self.input_match_syll_bound(captures, *seg_pos, err_pos) {
                // NOTE(girv): Boundaries do not advance seg_index 
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseElement::ExtlBound => if seg_pos.at_word_end(phrase) && !seg_pos.at_phrase_end(phrase) {
                captures.push(MatchElement::WordBound(seg_pos.word_index, err_pos));
                seg_pos.word_increment(phrase);
                *state_index += 1;
                Ok(true)
            } else {
                Ok(false)
            },
            ParseElement::Syllable(s, t, v) => self.input_match_syll(captures, state_index, s, t, v, phrase, seg_pos, err_pos),
            ParseElement::Structure(segs, stress, tone, refr) => self.input_match_structure(captures, state_index, segs, stress, tone, refr, phrase, seg_pos, err_pos),
            ParseElement::Ellipsis    => self.input_match_ellipsis(captures, phrase, seg_pos, states, state_index, true),
            ParseElement::OptEllipsis => self.input_match_ellipsis(captures, phrase, seg_pos, states, state_index, false),

            ParseElement::Negation(item) => if self.input_match_item(captures, seg_pos, &mut 0, phrase, &[*item.clone()], true)? {
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },

            ParseElement::Optional(..) | ParseElement::EmptySet  | 
            ParseElement::Metathesis   | ParseElement::WordBound | 
            ParseElement::MetaOrdered  => unreachable!(),
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
                ParseElement::Ipa(s, mods) => if self.context_match_ipa(s, mods, phrase, *pos, item.position, false)? { // TODO
                    self.matrix_increment(phrase, pos);
                    pos.increment(phrase);
                } else { return Ok(false) },
                ParseElement::Matrix(mods, refr) => if !self.context_match_matrix(mods, refr, phrase, pos, item.position, false)? { // TODO
                    return Ok(false)
                },
                ParseElement::Reference(num, mods) => match self.references.borrow().get(&num.value) {
                    Some(refr) => match refr {
                        RefKind::Segment(s) => if self.context_match_ipa(s, mods, phrase, *pos, item.position, false)? { // TODO
                            self.matrix_increment(phrase, pos);
                            pos.increment(phrase);
                        } else { return Ok(false) },
                        RefKind::Syllable(_) => return Err(RuleRuntimeError::SyllRefInsideStruct(item.position)),
                    },
                    None => return Err(RuleRuntimeError::UnknownReference(*num)),
                }
                ParseElement::Optional(states, min, max) => if self.context_match_option(items, &mut i, phrase, pos, true, states, *min, *max, Some(cur_syll_index))? {               
                    break;
                } else { return Ok(false) },
                ParseElement::Set(set) => if !self.context_match_set(set, phrase, pos, true, Some(cur_syll_index))? {
                    return Ok(false) 
                },

                ParseElement::Negation(item) => if !self.context_match(&[*item.clone()], &mut 0, phrase, pos, true, false, Some(cur_syll_index), true)? {
                    return Ok(false)
                },

                ParseElement::WordBound  | ParseElement::SyllBound | ParseElement::EmptySet | 
                ParseElement::Metathesis | ParseElement::ExtlBound | ParseElement::MetaOrdered | 
                ParseElement::Syllable(..) | ParseElement::Structure(..) => unreachable!(),
            }
        }
        if pos.seg_index != 0 { return Ok(false) }

        if let Some(r) = refr {
            self.references.borrow_mut().insert(*r, RefKind::Syllable(phrase[cur_word_index].syllables[cur_syll_index].clone()));
        }
        captures.push(MatchElement::Syllable(cur_word_index, cur_syll_index, err_pos));
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
                if !self.input_match_item(captures, pos, state_index, phrase, states, false)? {
                    m = false;
                    break;
                }
            }
            if m { return Ok(true) }
            *self.alphas.borrow_mut() = back_alphas.clone();
            *self.references.borrow_mut() = back_refs.clone();
            captures.truncate(back_captures_len);
        }

        if states.len() <= *state_index + 1 {
            match &states.last() {
                Some(ParseItem { kind: ParseElement::SyllBound, position })
                    if pos.at_word_end(phrase) || !phrase.in_bounds(*pos) => {
                        captures.push(MatchElement::SyllBound(phrase.len()-1, phrase[phrase.len()-1].syllables.len(), *position));
                        *state_index += 1;
                        return Ok(true)
                    }
                Some(ParseItem { kind: ParseElement::ExtlBound, position }) 
                    if pos.at_word_end(phrase) || !phrase.in_bounds(*pos) => {
                        captures.push(MatchElement::WordBound(pos.word_index, *position));
                        pos.word_increment(phrase);
                        *state_index += 1;
                        return Ok(true)
                    }
                _ => (),
            }
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
            captures.push(MatchElement::Syllable(cur_word_index, cur_syll_index, err_pos));
            *state_index += 1;
            pos.syll_index += 1;
            pos.seg_index = 0;

            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn input_match_syll_bound(&self, captures: &mut Vec<MatchElement>, pos: SegPos, err_pos: Position) -> bool {
        if pos.seg_index == 0 {
            captures.push(MatchElement::SyllBound(pos.word_index, pos.syll_index, err_pos));
            true
        } else {
            false
        }
    }

    fn input_match_set_choice(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, choice: &SetChoice, phrase: &Phrase, pos: &mut SegPos, choice_index: usize, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        let back_pos = *pos;
        let back_alphas = self.alphas.borrow().clone();
        let back_refs = self.references.borrow().clone();

        let mut caps = Vec::new();

        for item in &choice.items {
            let res = match &item.kind {
                ParseElement::Reference(vt, mods) => self.input_match_ref(&mut caps, state_index, vt, mods, phrase, pos, item.position, false), // TODO
                ParseElement::Ipa(seg, mods) => if self.input_match_ipa(&mut caps, seg, mods, phrase, pos, item.position, false)? { // TODO
                    pos.increment(phrase);
                    Ok(true)
                } else { Ok(false) },
                ParseElement::Matrix(mods, refr) => if self.input_match_matrix(&mut caps, mods, refr, phrase, pos, item.position, false)? { // TODO
                    pos.increment(phrase);
                    Ok(true)
                } else { Ok(false) },
                ParseElement::Syllable(stress, tone, refr) => self.input_match_syll(&mut caps, state_index, stress, tone, refr, phrase, pos, item.position),
                ParseElement::SyllBound => if pos.at_syll_start() {
                    caps.push(MatchElement::SyllBound(pos.word_index, pos.syll_index, item.position));
                    Ok(true)
                } else { Ok(false) },
                ParseElement::WordBound => Err(RuleRuntimeError::WordBoundSetLocError(item.position)),
                ParseElement::Structure(items, stress, tone, refr) => self.input_match_structure(&mut caps, state_index, items, stress, tone, refr, phrase, pos, item.position),
                
                ParseElement::Negation(item) => self.input_match_item(&mut caps, pos, state_index, phrase, &[*item.clone()], true),

                ParseElement::Optional(..) | ParseElement::EmptySet | ParseElement::ExtlBound | ParseElement::OptEllipsis | 
                ParseElement::MetaOrdered  | ParseElement::Ellipsis | ParseElement::Set(..)   | ParseElement::Metathesis  => unreachable!(),
            }?;

            if !res {
                *pos = back_pos;
                *self.alphas.borrow_mut() = back_alphas.clone();
                *self.references.borrow_mut() = back_refs.clone();
                return Ok(false)
            } 
        }
        
        captures.push(MatchElement::Set(caps, choice_index, err_pos));
        Ok(true)
    }

    fn input_match_set(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, set: &ItemSet, phrase: &Phrase, pos: &mut SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        let back_pos = *pos;
        let back_alphas = self.alphas.borrow().clone();
        let back_refs = self.references.borrow().clone();

        for (i, choice) in set.choices.iter().enumerate() {
            if self.input_match_set_choice(captures, state_index, choice, phrase, pos, i, err_pos)? {
                return Ok(true)
            }
            *pos = back_pos;
            *self.alphas.borrow_mut() = back_alphas.clone();
            *self.references.borrow_mut() = back_refs.clone();
        }

        Ok(false)

        // for (i,s) in set.items.iter().enumerate() {
        //     let res = match &s.kind {
        //         ParseElement::Reference(vt, mods) => self.input_match_ref(captures, state_index, vt, mods, phrase, pos, s.position),
        //         ParseElement::Ipa(seg, mods) => if self.input_match_ipa(captures, seg, mods, phrase, pos, s.position)? {
        //             pos.increment(phrase);
        //             Ok(true)
        //         } else { Ok(false) },
        //         ParseElement::Matrix(mods, refr) => if self.input_match_matrix(captures, mods, refr, phrase, pos, s.position)? {
        //             pos.increment(phrase);
        //             Ok(true)
        //         } else { Ok(false) },
        //         ParseElement::Syllable(stress, tone, refr) => self.input_match_syll(captures, state_index, stress, tone, refr, phrase, pos, s.position),
        //         ParseElement::SyllBound => if pos.at_syll_start() {
        //             captures.push(MatchElement::SyllBound(pos.word_index, pos.syll_index, Some(i)));
        //             Ok(true)
        //         } else { Ok(false) },
        //         ParseElement::WordBound => Err(RuleRuntimeError::WordBoundSetLocError(s.position)),
        //         ParseElement::Structure(items, stress, tone, refr) => self.input_match_structure(captures, state_index, items, stress, tone, refr, phrase, pos, s.position),
        //         _ => unreachable!(),
        //     };
        //     if res? {
        //         debug_assert!(!captures.is_empty());
        //         // SAFETY: captures is not empty
        //         unsafe { captures.last_mut().unwrap_unchecked().set_ind(Some(i)) };
        //         return Ok(true)
        //     }
        //     *pos = back_pos;
        //     // TODO: Deal with these clones
        //     *self.alphas.borrow_mut() = back_alphas.clone();
        //     *self.references.borrow_mut() = back_refs.clone();
        // }
        // Ok(false)
    }

    fn input_match_ipa(&self, captures: &mut Vec<MatchElement>, s: &Segment, mods: &Option<Modifiers>, phrase: &Phrase, pos: &mut SegPos, err_pos: Position, negate: bool) -> Result<bool, RuleRuntimeError> {
        let Some(seg) = phrase.get_seg_at(*pos) else { return Ok(false) };

        if let Some(m) = mods {
            let mod_match = self.match_ipa_with_modifiers(s, m, phrase, pos, err_pos)?;
            if (!negate && mod_match) || (negate && !mod_match) {
                if m.suprs.length.is_some() {
                    captures.push(MatchElement::LongSegment(*pos, err_pos));
                } else {
                    captures.push(MatchElement::Segment(*pos, err_pos));
                }
                self.matrix_increment(phrase, pos);
                Ok(true)
            } else {
                self.matrix_increment(phrase, pos);
                Ok(false)
            }
        } else if (!negate && *s == seg) || (negate && *s != seg) {
            captures.push(MatchElement::Segment(*pos, err_pos));
            self.matrix_increment(phrase, pos);
            Ok(true)
        } else {
            self.matrix_increment(phrase, pos);
            Ok(false)
        }
    }

    fn input_match_syll_ref(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, syll_to_match: &Syllable, mods: &Option<Modifiers>, phrase: &Phrase, pos: &mut SegPos, err_pos: Position, negate: bool) -> Result<bool, RuleRuntimeError> {
        if pos.seg_index != 0 || phrase[pos.word_index].out_of_bounds(*pos) {
            return Ok(false)
        }
        let cwi = pos.word_index;
        let csi = pos.syll_index;
        let cur_syll = &phrase[cwi].syllables[csi];

        if let Some(m) = mods {
            if !self.match_stress(&m.suprs.stress, cur_syll, err_pos)? && !negate {
                return Ok(false) 
            } 
            if let Some(t) = &m.suprs.tone.as_ref() && !self.match_tone(t, cur_syll) && !negate {
                return Ok(false) 
            }
            if cur_syll.segments != syll_to_match.segments && !negate {
                return Ok(false)
            }
            
        } else if (!negate && *cur_syll != *syll_to_match) || (negate && *cur_syll == *syll_to_match) {
            return Ok(false)
        }
        captures.push(MatchElement::Syllable(cwi, csi, err_pos));

        *state_index += 1;
        pos.syll_index += 1;
        pos.seg_index = 0;
        
        Ok(true)
        
    }

    fn input_match_ref(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, refr: &Reference, mods: &Option<Modifiers>, phrase: &Phrase, pos: &mut SegPos, err_pos: Position, negate: bool) -> Result<bool, RuleRuntimeError> {
        match self.references.borrow_mut().get(&refr.value) {
            Some(rk) => match rk {
                RefKind::Segment(s)  => if self.input_match_ipa(captures, s, mods, phrase, pos, err_pos, negate)? {
                    pos.increment(phrase);
                    Ok(true)
                } else { Ok(false) },
                RefKind::Syllable(s) => self.input_match_syll_ref(captures, state_index , s, mods, phrase, pos, err_pos, negate),
            },
            None => Err(RuleRuntimeError::UnknownReference(*refr)),
        }
    }

    fn input_match_matrix(&self, captures: &mut Vec<MatchElement>, mods: &Modifiers, refr: &Option<usize>, phrase: &Phrase, pos: &mut SegPos, err_pos: Position, negate: bool) -> Result<bool, RuleRuntimeError> { 
        if phrase[pos.word_index].out_of_bounds(*pos) { return Ok(false) }
        
        let mod_match = self.match_modifiers(mods, phrase, pos, err_pos)?;
        if (!negate && mod_match) || (negate && !mod_match) {
            if let Some(r) = refr {
                let Some(seg) = phrase.get_seg_at(*pos) else { return Ok(false) };
                self.references.borrow_mut().insert(*r, RefKind::Segment(seg));
            }
            captures.push(MatchElement::LongSegment(*pos, err_pos));
            self.matrix_increment(phrase, pos);
            Ok(true)
        } else {
            self.matrix_increment(phrase, pos);
            Ok(false)
        }
    }
}
