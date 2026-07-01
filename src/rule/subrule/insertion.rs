use super :: {
    RefKind, SubRule
};

use crate::{
    error :: RuleRuntimeError, 
    rule  :: { ParseElement, ParseItem, SupraSegs, UnderlineStruct }, 
    word  :: { Phrase, SegPos, Syllable }
};

impl SubRule {
    pub(super) fn insertion(&self, phrase: &Phrase) -> Result<Phrase, RuleRuntimeError> {
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
            // self.insertion_by_segment(phrase, before_cont, after_cont)
        }
    }
    
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
                        res_phrase[pos.word_index].syllables.back_mut().unwrap().segments.push_back(*seg);
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
                    // pos.ins_inc(&res_phrase);
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
                                    res_phrase[pos.word_index].syllables.back_mut().unwrap().segments.push_back(*seg);
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
                                // pos.ins_inc(&res_phrase);
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

                ParseElement::Negation(..) => return Err(RuleRuntimeError::BadNegationOutput(state.position)),
                ParseElement::Matrix(..) => return Err(RuleRuntimeError::InsertionMatrix(state.position)),
                ParseElement::Set(..) => return Err(RuleRuntimeError::LonelySet(state.position)),

                ParseElement::WordBound  | ParseElement::EmptySet  | ParseElement::Optional(..) |
                ParseElement::Metathesis | ParseElement::MetaOrdered | ParseElement::ExtlBound => unreachable!(),
            }
        }

        if is_context_after {
            pos.increment(&res_phrase);
        }

        // if is_context_after {
        //     pos.ins_inc(&res_phrase);
        // }
        // pos.ins_inc(&res_phrase);
        
        Ok((res_phrase, Some(pos)))
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

        // For infinite loop checking
        let phrase_len_max = Self::phrase_len_max(phrase);

        while res_phrase.in_bounds(pos) {
            if res_phrase.seg_count() > phrase_len_max {
                return Err(RuleRuntimeError::InfiniteLoop(self.input.first().unwrap().position, phrase.clone(), res_phrase.clone()))
            }

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

                        if self.env_is_hangable() {
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

    pub(super) fn insertion_match_exceptions(&self, phrase: &Phrase, ins_pos: SegPos) -> Result<bool, RuleRuntimeError> {
        let empty = Vec::new();
        let exceptions = self.get_exceptions();

        let (before_expt, center_expt, after_expt) = match exceptions.len().cmp(&1) {
            std::cmp::Ordering::Less => (&empty, &None, &empty),
            std::cmp::Ordering::Equal => exceptions[0],
            std::cmp::Ordering::Greater => return Err(RuleRuntimeError::InsertionGroupedEnv(self.except.clone().unwrap().position)),
        };

        if center_expt.is_some() {
            unreachable!("We should only enter this function when matching by segment")
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

    pub(super) fn insertion_match(&self, phrase: &Phrase, start_pos: SegPos) -> Result<Option<SegPos>, RuleRuntimeError> {
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

        if center_cont.is_some() {
            unreachable!("We should only enter this function when matching by segment")
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
                    while state_index < aft_states.len() {
                        if !self.context_match(aft_states, &mut state_index, phrase, &mut pos, true, false, None, false)? {
                            match bef_states.last().unwrap().kind {
                                ParseElement::WordBound => return Ok(None),
                                ParseElement::SyllBound => start_pos.increment(phrase),
                                _ => {}
                            }
                            start_pos.increment(phrase);
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
            if self.context_match(states, &mut state_index, phrase, &mut cur_pos, true, false, None, false)? {
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
            if self.context_match(states, &mut state_index, phrase, &mut cur_pos, true, true, None, false)? {
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
