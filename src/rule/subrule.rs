#![allow(clippy::comparison_chain, clippy::too_many_arguments)]

// NOTE(girv): lots of duplication here atm (and starting to look like spaghetti), focusing on getting things done before optimising

use std ::{
    cell::RefCell, 
    collections::{HashMap, VecDeque}
};

use crate  :: {
    error  :: RuleRuntimeError, 
    rule   :: { Alpha, AlphaMod, BinMod, ModKind, Modifiers, ParseElement, ParseItem, PlaceMod, Position, RuleType, SupraSegs, Token }, 
    word   :: { FeatKind, NodeKind, SegPos, Segment, StressKind, Syllable, Tone, Word },
};

type SylPos = usize;            // the index of the syllable in the word.syllables array
type BndPos = usize;            // the index of the syllable that has the boundary as it's start
type SetInd = Option<usize>;    // if matched in a set, the index within that set that was matched

#[derive(Debug, Clone, Copy)]
pub(crate) enum MatchElement {
    Segment  (SegPos, SetInd),
    Syllable (SylPos, SetInd),
    SyllBound(BndPos, SetInd)
}

impl MatchElement {
    pub(crate) fn set_ind(&mut self, si: SetInd) {
        *self = match self {
            MatchElement::Segment(sp, _) => MatchElement::Segment(*sp, si),
            MatchElement::Syllable(sp, _) => MatchElement::Syllable(*sp, si),
            MatchElement::SyllBound(bp, _) => MatchElement::SyllBound(*bp, si),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum VarKind {
    Segment(Segment),
    Syllable(Syllable)
}

#[derive(Debug)]
pub(crate) struct SubRule {
    pub(crate) input    : Vec<ParseItem>,
    pub(crate) output   : Vec<ParseItem>,
    pub(crate) context  : Option<ParseItem>,
    pub(crate) except   : Option<ParseItem>,
    pub(crate) rule_type: RuleType,
    pub(crate) variables: RefCell<HashMap<usize, VarKind>>,
    pub(crate) alphas   : RefCell<HashMap<char, Alpha>>
}

impl SubRule {

    fn get_contexts(&self) -> Vec<(&Vec<ParseItem>, &Vec<ParseItem>)> {
        match &self.context {
            Some(x) => match &x.kind {
                ParseElement::Environment(env) => env.iter().map(|e| (&e.before, &e.after)).collect(),
                _ => unreachable!()
            },
            None => vec![],
        }
    }

    fn get_exceptions(&self) -> Vec<(&Vec<ParseItem>, &Vec<ParseItem>)> {
        // static EMPTY_ENV: Vec<Item> = Vec::new();
        match &self.except {
            Some(x) => match &x.kind {
                ParseElement::Environment(env) => env.iter().map(|e| (&e.before, &e.after)).collect(),
                _ => unreachable!()
            },
            None => vec![],
        }
    }

    pub(crate) fn apply(&self, word: Word) -> Result<Word, RuleRuntimeError> {
        // RuleType::Substitution  => {/* input>env>output */},
        // RuleType::Metathesis    => {/* skip calc output */},
        // RuleType::Deletion      => {/* skip calc output */},
        // RuleType::Insertion     => {/* skip match input */},

        if self.rule_type == RuleType::Insertion {
            return self.transform(&word, vec![], &mut None)
        } 
        
        let mut word = word;
        let mut cur_index = SegPos::new(0, 0);
        // TODO(girv): `$ > *` or any broad deletion rule without context/exception should  give a warning to the user
        loop {
            self.alphas.borrow_mut().clear();
            self.variables.borrow_mut().clear();
            let (res, mut next_index) = self.input_match_at(&word, cur_index)?;
            if !res.is_empty() {
                let start = match res[0] {
                    MatchElement::Segment(sp, _)  => sp,
                    MatchElement::Syllable(s, _)  |
                    MatchElement::SyllBound(s, _) => SegPos::new(s, 0),
                };
                let end = match *res.last().unwrap() {
                    MatchElement::Segment(mut sp, _)  => {
                        // So that long vowels work
                        let mut seg_len = word.seg_length_at(sp);
                        while seg_len > 1 {
                            sp.increment(&word);
                            seg_len -= 1;
                        }
                        sp
                    },
                    MatchElement::SyllBound(s, _) => {
                        let mut sp = SegPos::new(s, 0);
                        if s != 0 {
                            sp.decrement(&word);
                        } // else {
                            // FIXME(girv): this may lead to bugs 
                        // }
                        sp
                    },
                    MatchElement::Syllable(s, _)  => SegPos::new(s, word.syllables[s].segments.len()-1),
                };
                if !self.match_contexts_and_exceptions(&word, start, end, true)? {
                    if let Some(ci) = next_index { 
                        cur_index = ci;
                        continue;
                    }
                    // end of word
                    break;
                }

                word = self.transform(&word, res, &mut next_index)?;
                
                if let Some(ci) = next_index { 
                    cur_index = ci;
                } else {
                    // End of Word
                    break;
                }
            } else {
                // No match
                break
            }
        }
        Ok(word)
    }

    fn match_before_env(&self, states: &[ParseItem], word_rev: &Word, pos: &SegPos, ins_match_before: bool, is_context: bool) -> Result<bool, RuleRuntimeError> {
        // NOTE: assumes parent has done reversals
        let mut start_pos = *pos;
        start_pos.increment(word_rev);
        let mut is_match = if is_context {
            true
        } else {
            !states.is_empty()
        };
        let mut si = 0;
        while si < states.len() {
            if !self.context_match(states, &mut si, word_rev, &mut start_pos, false, ins_match_before)? {
                is_match = false;
                if is_context { break; }
            }
            si += 1;
        }

        Ok(is_match)
    }

    fn match_after_env(&self, states: &[ParseItem], word: &Word, pos: &SegPos, ins_match_before: bool, inc: bool, is_context: bool) -> Result<bool, RuleRuntimeError> {
        let mut start_pos = *pos;
        if inc {
            start_pos.increment(word);
        }
        let mut is_match = if is_context {
            true
        } else {
            !states.is_empty()
        };
        let mut si = 0;
        while si < states.len() {
            if !self.context_match(states, &mut si, word, &mut start_pos, true, ins_match_before)? {
                is_match = false;
                if is_context { break; }
            }
            si += 1;
        }

        Ok(is_match)
    }

    fn match_contexts_and_exceptions(&self, word: &Word, start_pos: SegPos, end_pos: SegPos, inc: bool) -> Result<bool, RuleRuntimeError> {
        let contexts = self.get_contexts();
        let exceptions = self.get_exceptions();
        if contexts.is_empty() && exceptions.is_empty() {
            return Ok(true)
        }

        let word_rev = word.reverse();
        let mut is_cont_match = contexts.is_empty();
        let mut is_expt_match = false;

        for (bef_cont_states, aft_cont_states) in contexts {
            let mut bef_cont_states = bef_cont_states.clone();
            bef_cont_states.reverse();
            if (bef_cont_states.is_empty() || self.match_before_env(&bef_cont_states, &word_rev, &start_pos.reversed(word), false, true)?) 
            && (aft_cont_states.is_empty() || self.match_after_env(aft_cont_states, word, &end_pos, false, inc, true)?) {
                is_cont_match = true;
                break;
            }
        }
        for (bef_expt_states, aft_expt_states) in exceptions {
            let mut bef_expt_states = bef_expt_states.clone();
            bef_expt_states.reverse();
            if (bef_expt_states.is_empty() || self.match_before_env(&bef_expt_states, &word_rev, &start_pos.reversed(word), false, false)?) 
            && (aft_expt_states.is_empty() || self.match_after_env(aft_expt_states, word, &end_pos, false, inc, false)?) {
                is_expt_match = true;
                break;
            }
        }
        Ok(!is_expt_match && is_cont_match)
    }

    fn context_match(&self, states: &[ParseItem], state_index: &mut usize, word: &Word, pos: &mut SegPos, forwards: bool, ins_match_before: bool) -> Result<bool, RuleRuntimeError> {
        let state = &states[*state_index];
        match &state.kind {
            ParseElement::WordBound => Ok(word.out_of_bounds(*pos)),
            ParseElement::SyllBound => if ins_match_before {
                Ok(!pos.at_word_start() && pos.at_syll_start())
            } else {
                Ok(pos.at_syll_start())
            },
            ParseElement::Ipa(s, m) => if self.context_match_ipa(s, m, word, *pos, state.position)? {
                pos.increment(word); 
                Ok(true)
            } else { Ok(false) },
            ParseElement::Matrix(m, v) => self.context_match_matrix(m, v, word, pos, state.position),
            ParseElement::Syllable(s, t, v) => if ins_match_before {
                Ok(!pos.at_word_start() && self.context_match_syll(s, t, v, word, pos, forwards)?)
            } else {
                self.context_match_syll(s, t, v, word, pos, forwards)
            },
            ParseElement::Structure(segs, stress, tone, var) => if ins_match_before {
                Ok(!pos.at_word_start() && self.context_match_structure(segs, stress, tone, var, word, pos, forwards)?)
            } else {
                self.context_match_structure(segs, stress, tone, var, word, pos, forwards)
            },
            ParseElement::Variable(vt, mods) => self.context_match_var(vt, mods, word, pos, forwards, state.position),
            ParseElement::Set(s) => self.context_match_set(s, word, pos, forwards),
            ParseElement::Optional(opt_states, min, max) => self.context_match_option(states, state_index, word, pos, forwards, opt_states, *min, *max),
            ParseElement::Ellipsis => self.context_match_ellipsis(states, state_index, word, pos, forwards),
            
            ParseElement::EmptySet | ParseElement::Metathesis |
            ParseElement::Environment(_) => unreachable!(),
        }
    }

    fn context_match_structure(&self, items: &[ParseItem], stress: &[Option<ModKind>; 2], tone: &Option<Tone>, var: &Option<usize>, word: &Word, pos: &mut SegPos, forwards: bool) -> Result<bool, RuleRuntimeError> {
        if items.is_empty() {
            return self.context_match_syll(stress, tone, var, word, pos, forwards)
        }
        if !pos.at_syll_start() {
            return Ok(false)
        }
        let cur_syll = if word.in_bounds(*pos){ 
            &word.syllables[pos.syll_index] 
        } else { return Ok(false) };
        if !self.match_stress(stress, cur_syll)? {
            return Ok(false)
        }
        if let Some(t) = tone.as_ref() {
            if !self.match_tone(t, cur_syll) {
                return Ok(false)
            }
        }

        let mut items = items.to_vec().clone();
        if !forwards {
            items.reverse();
        }

        let cur_syll_index = pos.syll_index;

        for (mut i, item) in items.iter().enumerate() {
            if pos.syll_index != cur_syll_index {
                return Ok(false)
            }
            match &item.kind {
                ParseElement::Ellipsis => if i == items.len() - 1 {
                    // if last item, jump to end of syll and break loop
                    pos.syll_index += 1;
                    pos.seg_index = 0;
                    break;
                } else if self.context_match_ellipis_struct(&items, &mut i, word, pos, cur_syll_index)? {
                    break;
                } else { return Ok(false) }, 
                ParseElement::Ipa(s, mods) => if self.context_match_ipa(s, mods, word, *pos, item.position)? {
                    pos.increment(word);
                } else { return Ok(false) },
                ParseElement::Matrix(mods, var) => if !self.context_match_matrix(mods, var, word, pos, item.position)? {
                    return Ok(false) 
                },
                ParseElement::Variable(..) => unimplemented!(),
                _ => unreachable!()
            }
        }

        if pos.seg_index != 0 { return Ok(false) }

        if let Some(v) = var {
            if forwards {
                self.variables.borrow_mut().insert(*v, VarKind::Syllable(word.syllables[cur_syll_index].clone()));
            } else {
                let mut syll = word.syllables[cur_syll_index].clone();
                syll.segments.make_contiguous().reverse();
                self.variables.borrow_mut().insert(*v, VarKind::Syllable(syll));
            }
        }

        Ok(true)
    }

    fn context_match_ellipis_struct(&self, items: &[ParseItem], index: &mut usize, word: &Word, pos: &mut SegPos, syll_index: usize) -> Result<bool, RuleRuntimeError> {
        if *index >= items.len() {
            return Ok(true)
        }
        
        *index += 1;
        pos.increment(word);

        while pos.syll_index == syll_index {
            let back_pos = *pos;
            let back_index = *index;
            let back_alphas = self.alphas.borrow().clone();
            let back_varlbs = self.variables.borrow().clone();
            
            let mut m = true;
            while *index < items.len() {
                if pos.syll_index != syll_index {
                    m = false;
                    break;
                }
                match &items[*index].kind {
                    ParseElement::Ellipsis => if *index == items.len() - 1 {
                        // if last item, jump to end of syll and break loop
                        pos.syll_index = syll_index + 1;
                        pos.seg_index = 0;
                        return Ok(true)
                    } else if self.context_match_ellipis_struct(items, index, word, pos, syll_index)? {
                        // Don't know about this
                        pos.syll_index = syll_index + 1;
                        pos.seg_index = 0;
                        return Ok(true)
                    } else { m = false; break; }, 
                    ParseElement::Ipa(s, mods) => if self.context_match_ipa(s, mods, word, *pos, items[*index].position)? {
                        pos.increment(word);
                    } else { m = false; break; },
                    ParseElement::Matrix(mods, var) => if !self.context_match_matrix(mods, var, word, pos, items[*index].position)? {
                        m = false; break;
                    },
                    ParseElement::Variable(..) => unimplemented!(),
                    _ => unreachable!()
                }
                *index += 1;
            }
            if m {
                return Ok(true)
            }
            *index = back_index;
            *pos = back_pos;
            *self.alphas.borrow_mut() = back_alphas;
            *self.variables.borrow_mut() = back_varlbs;
            pos.increment(word);
        }

        Ok(false)
    }

    fn context_match_ellipsis(&self, states: &[ParseItem], state_index: &mut usize, word: &Word, pos: &mut SegPos, forwards: bool) -> Result<bool, RuleRuntimeError> {
        if *state_index >= states.len() {
            return Ok(true)
        }

        *state_index += 1;
        pos.increment(word);

        while word.in_bounds(*pos) {
            let back_pos = *pos;
            let back_state = *state_index;
            let back_alphas = self.alphas.borrow().clone();
            let back_varlbs = self.variables.borrow().clone();

            let mut m = true;
            while *state_index < states.len() {
                if !self.context_match(states, state_index, word, pos, forwards, false)? {
                    m = false;
                    break;
                }
                *state_index += 1;
            }
            if m {
                return Ok(true)
            }
            *state_index = back_state;
            *pos = back_pos;
            *self.alphas.borrow_mut() = back_alphas;
            *self.variables.borrow_mut() = back_varlbs;
            pos.increment(word);
        }
        
        Ok(false)
    }

    fn match_opt_states(&self, opt_states: &[ParseItem], word: &Word, pos: &mut SegPos, forwards: bool) -> Result<bool, RuleRuntimeError> {
        let mut si = 0;
        while si < opt_states.len() {
            if !self.context_match(opt_states, &mut si, word, pos, forwards, false)? {
                return Ok(false)
            }
            si += 1;
        }
        Ok(true)
    }

    fn context_match_option(&self, states: &[ParseItem], state_index: &mut usize, word: &Word, pos: &mut SegPos, forwards: bool, opt_states: &[ParseItem], match_min: usize, match_max: usize) -> Result<bool, RuleRuntimeError> {
        // should work like regex (...){min, max}? 
        let match_max = if match_max == 0 { None } else { Some(match_max) };
        let back_pos = *pos;
        let back_alphas = self.alphas.borrow().clone();
        let back_varlbs = self.variables.borrow().clone();
        
        let mut index = 0;
        while index < match_min {
            if !self.match_opt_states(opt_states, word, pos, forwards)? {
                *pos = back_pos;
                *self.alphas.borrow_mut() = back_alphas;
                *self.variables.borrow_mut() = back_varlbs;
                return Ok(false)
            }
            index += 1;
        }

        *state_index +=1;
        let back_state = *state_index;
        let back_pos = *pos;
        let back_alphas = self.alphas.borrow().clone();
        let back_varlbs = self.variables.borrow().clone();

        let mut m = true;
        while *state_index < states.len() {
            if !self.context_match(states, state_index, word, pos, forwards, false)? {
                m = false;
                break;
            }
            *state_index += 1;
        }
        if m {
            return Ok(true)
        }

        *pos = back_pos;
        *state_index = back_state;
        *self.alphas.borrow_mut() = back_alphas.clone();
        *self.variables.borrow_mut() = back_varlbs.clone();
        
        let max = match_max.unwrap_or(usize::MAX);
        while index < max {
            *state_index = back_state;
            if self.match_opt_states(opt_states, word, pos, forwards)? {
                let mut m = true;
                while *state_index < states.len() {
                    if !self.context_match(states, state_index, word, pos, forwards, false)? {
                        m = false;
                        break;
                    }
                    *state_index += 1;
                }
                if m {
                    return Ok(true)
                } else {
                    index += 1;
                    *self.alphas.borrow_mut() = back_alphas.clone();
                    *self.variables.borrow_mut() = back_varlbs.clone();
                    continue;
                }
            } else {
                return Ok(false)
            }
        }
        Ok(false)
    }

    fn context_match_set(&self, set: &[ParseItem], word: &Word, pos: &mut SegPos, forwards: bool) -> Result<bool, RuleRuntimeError> {
        let back_pos= *pos;
        let back_alphas = self.alphas.borrow().clone();
        let back_varlbs = self.variables.borrow().clone();
        
        for s in set {
            let res = match &s.kind {
                ParseElement::Variable(vt, mods) => self.context_match_var(vt, mods, word, pos, forwards, s.position),
                ParseElement::Ipa(seg, mods) => if self.context_match_ipa(seg, mods, word, *pos, s.position)? {
                    pos.increment(word);
                    Ok(true)
                } else {Ok(false)},
                ParseElement::Matrix(mods, var) => self.context_match_matrix(mods, var, word, pos, s.position),
                ParseElement::Syllable(stress, tone, var) => self.context_match_syll(stress, tone, var, word, pos, forwards),
                ParseElement::WordBound => Ok(word.out_of_bounds(*pos)),
                ParseElement::SyllBound => Ok(pos.at_syll_start()),
                _ => unimplemented!(),
            };
            if res? {
                return Ok(true)
            }
            *pos = back_pos;
            // TODO: Deal with these clones
            *self.alphas.borrow_mut() = back_alphas.clone();
            *self.variables.borrow_mut() = back_varlbs.clone();
        }
        Ok(false)
    }

    fn context_match_var(&self, vt: &Token, mods: &Option<Modifiers>, word: &Word, pos: &mut SegPos, forwards: bool, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        if let Some(var) = self.variables.borrow_mut().get(&vt.value.parse::<usize>().unwrap()) {
            match var {
                VarKind::Segment(s) => if self.context_match_ipa(s, mods, word, *pos, err_pos)? {
                    pos.increment(word);
                    Ok(true)
                } else { Ok(false) },
                VarKind::Syllable(s) => self.context_match_syll_var(s, mods, word, pos, forwards),
            }            
        } else {
            Err(RuleRuntimeError::UnknownVariable(vt.clone()))
        }
    }

    fn context_match_syll_var(&self, syll_to_match: &Syllable, mods: &Option<Modifiers>, word: &Word, pos: &mut SegPos, forwards: bool) -> Result<bool, RuleRuntimeError> {
        if !pos.at_syll_start()  {
            return Ok(false)
        }
        let cur_syll = if word.in_bounds(*pos) {
             &word.syllables[pos.syll_index]
        } else { return Ok(false) };

        let segs_to_match = if forwards {
            syll_to_match.segments.clone() // i hate this
        } else {
            let mut segs = syll_to_match.segments.clone();
            segs.make_contiguous().reverse();
            segs
        };
        
        if let Some(Modifiers { nodes: _, feats: _, suprs }) = mods {
            if !self.match_stress(&suprs.stress, cur_syll)? {
                return Ok(false)
            }
            if let Some(t) = suprs.tone.as_ref()  {
                if !self.match_tone(t, cur_syll) {
                    return Ok(false)
                }
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

    fn context_match_syll(&self, stress: &[Option<ModKind>; 2], tone: &Option<Tone>, var: &Option<usize>, word: &Word, pos: &mut SegPos, forwards: bool) -> Result<bool, RuleRuntimeError> {        
        if !pos.at_syll_start() {
            return Ok(false)
        }
        let cur_syll = if word.in_bounds(*pos){ 
            &word.syllables[pos.syll_index] 
        } else { return Ok(false) };

        if !self.match_stress(stress, cur_syll)? {
            return Ok(false)
        }
        if let Some(t) = tone.as_ref() {
            if !self.match_tone(t, cur_syll) {
                return Ok(false)
            }
        }
        if let Some(v) = var {
            if forwards {
                self.variables.borrow_mut().insert(*v, VarKind::Syllable(word.syllables[pos.syll_index].clone()));
            } else {
                let mut syll = word.syllables[pos.syll_index].clone();
                syll.segments.make_contiguous().reverse();
                self.variables.borrow_mut().insert(*v, VarKind::Syllable(syll));
            }
        }
        pos.syll_index += 1;
        pos.seg_index = 0;
        Ok(true)
    }

    fn context_match_matrix(&self, mods: &Modifiers, var: &Option<usize>, word: &Word, pos: &mut SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> {        
        if word.out_of_bounds(*pos) {
            return Ok(false)
        }
        if self.match_modifiers(mods, word, pos, err_pos)? {
            if let Some(v) = var {
                self.variables.borrow_mut().insert(*v, VarKind::Segment(word.get_seg_at(*pos).unwrap()));
            }
            let mut seg_length = word.seg_length_at(*pos);            
            while seg_length >= 1 {
                pos.increment(word);
                seg_length -= 1;
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn transform(&self, word: &Word, input: Vec<MatchElement>, next_pos: &mut Option<SegPos>) -> Result<Word, RuleRuntimeError> {
        match self.rule_type {
            RuleType::Metathesis => {
                let mut res_word = word.clone();
                for z in 0..(input.len() / 2) {
                    match (input[z], input[input.len()-1-z]) {
                        (MatchElement::Segment(li, _), MatchElement::Segment(ri, _)) => {
                            // FIXME: If we swap syllables or boundaries then do this, these SegPos may not be correct
                            let sl = res_word.get_seg_at(li).unwrap();
                            let sr = res_word.get_seg_at(ri).unwrap();
                            let tmp = sl;
                            res_word.syllables[li.syll_index].segments[li.seg_index] = sr;
                            res_word.syllables[ri.syll_index].segments[ri.seg_index] = tmp;
                        },
                        (MatchElement::Syllable(i, _), MatchElement::Syllable(j, _)) => {
                            res_word.swap_sylls(i, j);
                        },
                        (MatchElement::SyllBound(..), MatchElement::SyllBound(..)) => {/* Do nothing */},
                        (MatchElement::Segment(si, _), MatchElement::SyllBound(bi, _)) => {
                            // FIXME(girv): this won't work for rules with `...`, it may be necessary to disallow `$` in `...` rules                            
                            let seg = res_word.syllables[si.syll_index].segments[si.seg_index];
                            if bi < res_word.syllables.len() {
                                res_word.syllables[bi].segments.push_front(seg);
                                res_word.syllables[si.syll_index].segments.remove(si.seg_index); // pop_back()                                
                            } else {
                                res_word.syllables.push(Syllable { segments: VecDeque::new(), stress: StressKind::Unstressed, tone: 0 });
                                res_word.syllables.last_mut().unwrap().segments.push_front(seg);
                                res_word.syllables[si.syll_index].segments.remove(si.seg_index); // pop_back()
                            }                 

                            if res_word.syllables[si.syll_index].segments.is_empty() {
                                res_word.syllables.remove(si.syll_index);
                            }
                        },
                        (MatchElement::SyllBound(bi, _), MatchElement::Segment(si, _)) => {
                            // FIXME(girv): this won't work for rules with `...`, it may be necessary to disallow `$` in `...` rules
                            let seg = res_word.syllables[si.syll_index].segments[si.seg_index];
                            if bi > 0 {
                                res_word.syllables[bi-1].segments.push_back(seg);
                                res_word.syllables[si.syll_index].segments.remove(si.seg_index); // pop_front()                                
                            } else {
                                res_word.syllables[si.syll_index].segments.remove(si.seg_index); // pop_front()
                                res_word.syllables.insert(0, Syllable { segments: VecDeque::new(), stress: StressKind::Unstressed, tone: 0 });
                                res_word.syllables.first_mut().unwrap().segments.push_front(seg);
                            }
                            if res_word.syllables[si.syll_index].segments.is_empty() {
                                res_word.syllables.remove(si.syll_index);
                            }
                        },
                        // I think we're just gonna disallow these, I can't think of a valid rule where these make sense
                        (MatchElement::Segment(..), MatchElement::Syllable(..)) |
                        (MatchElement::Syllable(..), MatchElement::Segment(..)) => {
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
                Ok(res_word)
            },
            RuleType::Deletion => {
                let mut pos = SegPos::new(0, 0);
                let mut res_word = word.clone();
                for z in input.into_iter().rev() {
                    match z {
                        MatchElement::Segment(i, _) => {
                            pos = i;
                            // remove segment                             
                            if res_word.syllables.len() <= 1 && word.syllables[i.syll_index].segments.len() <= 1 {
                                return Err(RuleRuntimeError::DeletionOnlySeg)
                            }
                            res_word.syllables[i.syll_index].segments.remove(i.seg_index);
                            // if that was the only segment in that syllable, remove the syllable
                            if res_word.syllables[i.syll_index].segments.is_empty() {
                                res_word.syllables.remove(i.syll_index);
                            }
                        },
                        MatchElement::Syllable(i, _) => {
                            // remove syllable
                            if res_word.syllables.len() <= 1 {
                                return Err(RuleRuntimeError::DeletionOnlySyll)
                            }
                            pos.syll_index = i;
                            pos.seg_index = 0;
                            pos.decrement(&res_word);
                            res_word.remove_syll(i);
                        },
                        MatchElement::SyllBound(i, _) => {
                            // join the two neighbouring syllables
                            // if one has stress and/or tone, joined syll gets them
                            // if they both have stress, highest wins
                            // if they both have tone, join them i.e. ma5a1 > ma:51
                            if res_word.syllables.len() <= 1 {
                                return Err(RuleRuntimeError::DeletionOnlySyll)
                            }
                            
                            if i == 0 || i >= res_word.syllables.len() {
                                // can't delete a word boundary
                                continue;
                            }
                            pos.syll_index = i;
                            pos.seg_index = 0;
                            pos.decrement(&res_word);

                            let mut syll_segs = res_word.syllables[i].segments.clone();
                            res_word.syllables[i-1].segments.append(&mut syll_segs);

                            res_word.syllables[i-1].stress = match (res_word.syllables[i-1].stress, res_word.syllables[i].stress) {
                                (StressKind::Primary, _) | (_, StressKind::Primary) => StressKind::Primary,
                                (StressKind::Secondary, StressKind::Secondary)  | 
                                (StressKind::Secondary, StressKind::Unstressed) | 
                                (StressKind::Unstressed, StressKind::Secondary)  => StressKind::Secondary,
                                (StressKind::Unstressed, StressKind::Unstressed) => StressKind::Unstressed,
                            };

                            res_word.syllables[i-1].tone = Self::concat_tone(res_word.syllables[i-1].tone, res_word.syllables[i].tone);
                            res_word.syllables.remove(i);
                        },
                    }
                }
                if let Some(next) = next_pos {
                    pos.increment(&res_word);
                    *next = pos;
                }
                Ok(res_word)
            },
            RuleType::Insertion => {
                // find insertion position using context
                // "Parse" and insert output
                let empty = Vec::new();
                let context = self.get_contexts();
                let exceptions = self.get_exceptions();

                let ((before_cont, after_cont), (before_expt, after_expt)) = match (context.len().cmp(&1), exceptions.len().cmp(&1)) {
                    (std::cmp::Ordering::Equal,  std::cmp::Ordering::Less) => (context[0],(&empty, &empty)),
                    (std::cmp::Ordering::Equal, std::cmp::Ordering::Equal) => (context[0], exceptions[0]),
                    (std::cmp::Ordering::Less,  std::cmp::Ordering::Equal) => ((&empty, &empty), exceptions[0]),
                    (std::cmp::Ordering::Less,   std::cmp::Ordering::Less) => return Err(RuleRuntimeError::InsertionNoEnv(self.output.last().unwrap().position)),
                    (std::cmp::Ordering::Greater, _) => return Err(RuleRuntimeError::InsertionGroupedEnv(self.context.clone().unwrap().position)),
                    (_, std::cmp::Ordering::Greater) => return Err(RuleRuntimeError::InsertionGroupedEnv(self.except.clone().unwrap().position)),
                };

                if before_cont.is_empty() && after_cont.is_empty() && before_expt.is_empty() && after_expt.is_empty() {
                    return Err(RuleRuntimeError::InsertionNoEnv(self.output.last().unwrap().position))
                }

                let mut res_word = word.clone();

                let is_context_after = before_cont.is_empty() && !after_cont.is_empty();

                let mut pos = SegPos::new(0, 0);
                while res_word.in_bounds(pos) {
                    self.alphas.borrow_mut().clear();
                    self.variables.borrow_mut().clear();
                    match self.insertion_match(&res_word, pos)? {
                        Some(ins) => {                                    
                            if self.insertion_match_exceptions(word, ins)? {
                                pos.increment(word);
                                continue;
                            }
                            let (res, next_pos) = self.insert(&res_word, ins, is_context_after)?;
                            res_word = res;
                
                            if let Some(np) = next_pos {
                                pos = np;
                                if !is_context_after && pos.at_syll_end(&res_word) {
                                    pos.increment(&res_word);
                                }
                            } else {
                                // End of Word
                                break;
                            }
                        },
                        None => break
                    }
                }

                Ok(res_word)
            },
            RuleType::Substitution => self.substitution(word, input, next_pos),
        }
    }

    fn insertion_match_exceptions(&self, word: &Word, ins_pos: SegPos) -> Result<bool, RuleRuntimeError> {
        let empty = Vec::new();
        let exceptions = self.get_exceptions();

        let (before_expt, after_expt) = match exceptions.len().cmp(&1) {
            std::cmp::Ordering::Less => (&empty, &empty),
            std::cmp::Ordering::Equal => exceptions[0],
            std::cmp::Ordering::Greater => return Err(RuleRuntimeError::InsertionGroupedEnv(self.except.clone().unwrap().position)),
        };
        let mut before_expt = before_expt.clone();
        before_expt.reverse();

        match (before_expt.is_empty(), after_expt.is_empty()) {
            // _
            (true, true) => Ok(false),
            (false, true) => {
                // #_
                let word_rev = &word.reverse();
                let pos_rev = ins_pos.reversed(word);
                let match_bef = self.match_before_env(&before_expt, word_rev, &pos_rev, false, false)?;
                Ok(match_bef)
            },
            (true, false) => {
                // _#
                // edge case for when insertion position is out of bounds but not at the word end
                if after_expt.len() == 1 && after_expt[0].kind == ParseElement::WordBound && !ins_pos.at_word_end(word) {
                    return Ok(false)
                }
                let match_aft = self.match_after_env(after_expt, word, &ins_pos, true, false, false)?;
                Ok(match_aft)
            },
            // #_#
            (false, false) => {
                let word_rev = &word.reverse();
                let pos_rev = ins_pos.reversed(word);
                let match_bef = self.match_before_env(&before_expt, word_rev, &pos_rev, false, false)?;
                let match_aft = self.match_after_env(after_expt, word, &ins_pos, false, false, false)?;

                Ok(match_bef && match_aft)
            },
        }
    }

    fn insertion_match(&self, word: &Word, start_pos: SegPos) -> Result<Option<SegPos>, RuleRuntimeError> {
        let empty = Vec::new();
        let context = self.get_contexts();
        let exceptions = self.get_exceptions();

        let ((before_cont, after_cont), (before_expt, after_expt)) = match (context.len().cmp(&1), exceptions.len().cmp(&1)) {
            (std::cmp::Ordering::Equal,  std::cmp::Ordering::Less) => (context[0],(&empty, &empty)),
            (std::cmp::Ordering::Equal, std::cmp::Ordering::Equal) => (context[0], exceptions[0]),
            (std::cmp::Ordering::Less,  std::cmp::Ordering::Equal) => ((&empty, &empty), exceptions[0]),
            (std::cmp::Ordering::Less,   std::cmp::Ordering::Less) => return Err(RuleRuntimeError::InsertionNoEnv(self.output.last().unwrap().position)),
            (std::cmp::Ordering::Greater, _) => return Err(RuleRuntimeError::InsertionGroupedEnv(self.context.clone().unwrap().position)),
            (_, std::cmp::Ordering::Greater) => return Err(RuleRuntimeError::InsertionGroupedEnv(self.except.clone().unwrap().position)),
        };

        if before_cont.is_empty() && after_cont.is_empty() && before_expt.is_empty() && after_expt.is_empty() {
            return Err(RuleRuntimeError::InsertionNoEnv(self.output.last().unwrap().position))
        }

        let maybe_ins = match (before_cont.is_empty(), after_cont.is_empty()) {
            (true, true) => Some(start_pos), // _
            (false, true) => self.insertion_after(before_cont, word, start_pos)?, // #_
            (true, false) => self.insertion_before(after_cont, word, start_pos)?, // _#
            (false, false) => self.insertion_between(before_cont, after_cont, word, start_pos)?, // #_#
        };

        Ok(maybe_ins)
    }

    fn insertion_between(&self, bef_states: &[ParseItem], aft_states: &[ParseItem], word: &Word, start_pos: SegPos) -> Result<Option<SegPos>, RuleRuntimeError> {
        let mut start_pos = start_pos;
        
        // FIXME: This is scuffed
        'outer: while word.in_bounds(start_pos) {
            match self.insertion_after(bef_states, word, start_pos)? {
                Some(mut ins_pos) => {
                    let mut pos = ins_pos;
                    let mut state_index = 0;
                    start_pos = ins_pos;
                    while state_index < aft_states.len() {
                        if !self.context_match(aft_states, &mut state_index, word, &mut pos, true, false)? {
                            match bef_states.last().unwrap().kind {
                                ParseElement::WordBound => return Ok(None),
                                ParseElement::SyllBound => start_pos.increment(word),
                                _ => {}
                            }
                            continue 'outer;
                        }
                        state_index +=1;
                    }
                    // Fix for insertion before a mid-word syllable boundary
                    if let ParseElement::SyllBound | ParseElement::Structure(..) = aft_states[0].kind {
                        if ins_pos.at_syll_start() {
                            ins_pos.decrement(word);
                            ins_pos.seg_index += 1;
                        }
                    }
                    return Ok(Some(ins_pos)) 
                },
                None => return Ok(None),
            }
        }

        Ok(None)
    }

    fn insertion_after(&self, states: &[ParseItem], word: &Word, start_pos: SegPos) -> Result<Option<SegPos>, RuleRuntimeError> {
        // i.e. #_
        let mut cur_pos = start_pos;
        let mut state_index = 0;
        let mut match_begin = None;

        if states[0].kind == ParseElement::WordBound {
            if !start_pos.at_word_start() {
                return Ok(None)
            }
            if states.len() == 1 {
                return Ok(Some(start_pos))
            }
            state_index = 1;
        }

        while word.in_bounds(cur_pos) {
            if self.context_match(states, &mut state_index, word, &mut cur_pos, true, false)? {
                if state_index >= states.len() - 1 {
                    return Ok(Some(cur_pos))
                }
                if match_begin.is_none() {
                    match_begin = Some(cur_pos);
                }
                state_index += 1;
            } else if let Some(mb) = match_begin{
                cur_pos = mb;
                cur_pos.increment(word);
                state_index = 0;
                match_begin = None;
                self.alphas.borrow_mut().clear();
                self.variables.borrow_mut().clear();
            } else {
                cur_pos.increment(word);
                self.alphas.borrow_mut().clear();
                self.variables.borrow_mut().clear();
                state_index = 0;
            }
        }

        if match_begin.is_none() {
            if let ParseElement::SyllBound = states.last().unwrap().kind {
                let sy = word.syllables.len() - 1;
                let sg = word.syllables[sy].segments.len();
                Ok(Some(SegPos::new(sy, sg)))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    fn insertion_before(&self, states: &[ParseItem], word: &Word, start_pos: SegPos) -> Result<Option<SegPos>, RuleRuntimeError> {
        // i.e. _#
        let mut cur_pos = start_pos;
        let mut state_index = 0;
        let mut match_begin = None;

        while word.in_bounds(cur_pos) {
            let before_pos = cur_pos;
            if self.context_match(states, &mut state_index, word, &mut cur_pos, true, true)? {
                if match_begin.is_none() {
                    let mut sp = before_pos;
                    if let ParseElement::Syllable(..) | ParseElement::SyllBound = states.first().unwrap().kind {
                        sp.decrement(word);
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
                cur_pos.increment(word);
                state_index = 0;
                match_begin = None;
                self.alphas.borrow_mut().clear();
                self.variables.borrow_mut().clear();
            } else {
                cur_pos.increment(word);
                self.alphas.borrow_mut().clear();
                self.variables.borrow_mut().clear();
                state_index = 0;
            }
        }

        if match_begin.is_none() {
            if let ParseElement::WordBound | ParseElement::SyllBound | ParseElement::Structure(..) = states.first().unwrap().kind {
                let sy = word.syllables.len() - 1;
                let sg = word.syllables[sy].segments.len();
                Ok(Some(SegPos::new(sy, sg)))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    fn insert(&self, word: &Word, pos: SegPos, is_context_after: bool) -> Result<(Word, Option<SegPos>), RuleRuntimeError> {
        let mut res_word = word.clone();
        let mut pos = pos;
        for state in &self.output {
            match &state.kind {
                ParseElement::Ipa(seg, mods) => {
                    if let Some(syll) = res_word.syllables.get_mut(pos.syll_index) { 
                        let lc = syll.insert_segment(pos.seg_index, seg, mods, &self.alphas, state.position)?;
                        if lc > 0 {
                            pos.seg_index += lc.unsigned_abs() as usize;
                        }
                    } else {
                        res_word.syllables.last_mut().unwrap().segments.push_back(*seg);
                        if let Some(m) = mods {
                            let lc = res_word.apply_seg_mods(&self.alphas, m, pos, state.position)?;
                            if lc > 0 {
                                pos.seg_index += lc.unsigned_abs() as usize;
                            }
                        } 
                    };
                    pos.increment(&res_word);
                },
                ParseElement::SyllBound => {
                    // FIXME(girv): Issue
                    // if we are part sequence of rules that are inserting a new syllable to the start of the word
                    // then we want the original syllable (which in this case will become the second syllable) to have the stress/tone
                    // however, this edge case is not possible to discern from

                    if pos.at_syll_start() {
                        // NOTE: Possible unintended behaviour
                        // i.e "* > $ka / _#" on /de.su/ would return /de.suka/
                        continue;
                    }
                    // split current syll into two at pos
                    let mut second_syll = Syllable::new();
                    let first_syll = res_word.syllables.get_mut(pos.syll_index).unwrap();

                    while first_syll.segments.len() > pos.seg_index {
                        second_syll.segments.push_front(first_syll.segments.pop_back().unwrap());
                    }

                    res_word.syllables.insert(pos.syll_index+1, second_syll);

                    pos.syll_index += 1;
                    pos.seg_index = 0;
                },
                ParseElement::Syllable(stress, tone, var) => {
                    if pos.at_syll_start() {
                        // apply mods to current syllable, possibly not a good idea
                        if let Some(syll) = res_word.syllables.get_mut(pos.syll_index) {
                            syll.apply_syll_mods(&self.alphas, &SupraSegs { stress: *stress, length: [None, None], tone: *tone }, state.position)?;
                            if let Some(v) = var {
                                self.variables.borrow_mut().insert(*v, VarKind::Syllable(res_word.syllables[pos.syll_index].clone()));
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
                    new_syll.apply_syll_mods(&self.alphas, /*&self.variables,*/ &SupraSegs { stress: *stress, length: [None, None], tone: *tone }, state.position)?;

                    let syll = res_word.syllables.get_mut(pos.syll_index).expect("pos should not be out of bounds");

                    while syll.segments.len() > pos.seg_index {
                        new_syll.segments.push_front(syll.segments.pop_back().unwrap());
                    }
                    res_word.syllables.insert(pos.syll_index+1, new_syll);

                    pos.syll_index += 2;
                    pos.seg_index = 0;

                    if let Some(v) = var {
                        self.variables.borrow_mut().insert(*v, VarKind::Syllable(res_word.syllables[pos.syll_index -1].clone()));
                    }
                },
                ParseElement::Structure(items, stress, tone, var) => {
                    let insert_syll = self.gen_syll_from_struct(items, stress, tone, var, state.position, true)?;

                    if pos.at_syll_start() {
                        res_word.syllables.insert(pos.syll_index, insert_syll);
                        pos.syll_index += 1;
                        pos.seg_index = 0;
                        continue;
                    }

                    let old_syll = res_word.syllables.get_mut(pos.syll_index).expect("pos should not be out of bounds");
                    
                    let mut new_syll = Syllable::new();
                    new_syll.stress = old_syll.stress;
                    new_syll.tone = old_syll.tone;

                    while old_syll.segments.len() > pos.seg_index {
                        new_syll.segments.push_front(old_syll.segments.pop_back().unwrap());
                    }

                    let mut adjustment = 0;
                    if old_syll.segments.is_empty() {
                        *old_syll = insert_syll;
                    } else {
                        res_word.syllables.insert(pos.syll_index+1, insert_syll);
                        adjustment = 1;
                    }
                    
                    if !new_syll.segments.is_empty() {
                        res_word.syllables.insert(pos.syll_index+1+adjustment, new_syll);
                        pos.syll_index += 2 + adjustment;
                    } else {
                        pos.syll_index += 1 + adjustment;
                    }
                    
                    pos.seg_index = 0;
                },
                ParseElement::Variable(num, mods) => {
                    if let Some(var) = self.variables.borrow().get(&num.value.parse().unwrap()) {
                        match var {
                            VarKind::Segment(seg) => {
                                if let Some(syll) = res_word.syllables.get_mut(pos.syll_index) { 
                                    let lc = syll.insert_segment(pos.seg_index, seg, mods, &self.alphas, state.position)?;
                                    if lc > 0 {
                                        pos.seg_index += lc.unsigned_abs() as usize;
                                    }
                                } else {
                                    res_word.syllables.last_mut().unwrap().segments.push_back(*seg);
                                    if let Some(m) = mods {
                                        let lc = res_word.apply_seg_mods(&self.alphas, m, pos, state.position)?;
                                        if lc > 0 {
                                            pos.seg_index += lc.unsigned_abs() as usize;
                                        }
                                    } 
                                };
                                pos.increment(&res_word);
                            },
                            VarKind::Syllable(syll) => {
                                let mut new_syll = syll.clone();
                                if let Some(m) = mods {
                                    new_syll.apply_syll_mods(&self.alphas, &m.suprs, num.position)?;
                                }
                                if pos.at_syll_start() {
                                    res_word.syllables.insert(pos.syll_index, new_syll.clone());
                                    pos.syll_index += 1;
                                } else {
                                    // split current syllable in two, insert var_syll in between them
                                    let before_syll = res_word.syllables.get_mut(pos.syll_index).unwrap();
                                    let mut after_syll = Syllable::new();
                                    while before_syll.segments.len() > pos.seg_index {
                                        after_syll.segments.push_front(before_syll.segments.pop_back().unwrap());
                                    }
                                    res_word.syllables.insert(pos.syll_index+1, after_syll);
                                    res_word.syllables.insert(pos.syll_index+1, new_syll);

                                    pos.syll_index += 3;
                                    pos.seg_index = 0;
                                }
                            },
                        }
                    } else {
                        return Err(RuleRuntimeError::UnknownVariable(num.clone()))
                    }
                },

                ParseElement::Matrix(..) => return Err(RuleRuntimeError::InsertionMatrix(state.position)),
                ParseElement::Set(_) => return Err(RuleRuntimeError::LonelySet(state.position)),
                ParseElement::EmptySet  | ParseElement::Metathesis | 
                ParseElement::Ellipsis  | ParseElement::Optional(..) | 
                ParseElement::WordBound | ParseElement::Environment(..) => unreachable!(),
            }
        }

        if is_context_after {
            pos.increment(&res_word);
        }
        
        Ok((res_word, Some(pos)))
    }

    fn context_match_ipa(&self, s: &Segment, mods: &Option<Modifiers>, word: &Word, pos: SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        if word.out_of_bounds(pos) {
            return Ok(false)
        }
        let seg = word.get_seg_at(pos).unwrap();
        if let Some(m) = mods {
            Ok(self.match_ipa_with_modifiers(s, m, word, &pos, err_pos)?)
        } else {
            Ok(*s == seg)
        }
    }

    fn apply_syll_mods(&self, word: &mut Word, syll_index: usize, mods: &SupraSegs, var: &Option<usize>, err_pos: Position) -> Result<(), RuleRuntimeError> {
        word.syllables.get_mut(syll_index).unwrap().apply_syll_mods(&self.alphas, mods, err_pos)?;
        
        if let Some(v) = var {
            self.variables.borrow_mut().insert(*v, VarKind::Syllable(word.syllables[syll_index].clone()));
        }
        Ok(())
    }
    
    fn apply_seg_mods(&self, word: &mut Word, pos: SegPos, mods: &Modifiers, var: &Option<usize>, err_pos: Position) -> Result<i8, RuleRuntimeError>{
        let lc = word.apply_seg_mods(&self.alphas, mods, pos, err_pos)?;

        if let Some(v) = var {
            self.variables.borrow_mut().insert(*v, VarKind::Segment(word.syllables[pos.syll_index].segments[pos.seg_index]));
        }

        Ok(lc)
    }

    fn gen_syll_from_struct(&self, items: &[ParseItem], stress: &[Option<ModKind>; 2], tone: &Option<Tone>, var: &Option<usize>, err_pos: Position, is_inserting: bool) -> Result<Syllable, RuleRuntimeError> {
        let mut syll = Syllable::new();
        let mods = SupraSegs { stress: *stress, length: [None, None], tone: *tone };

        syll.apply_syll_mods(&self.alphas, &mods, err_pos)?;

        for item in items {
            match &item.kind {
                ParseElement::Ellipsis   => return Err(if is_inserting {RuleRuntimeError::InsertionEllipsis(item.position)} else {RuleRuntimeError::SubstitutionEllipsis(item.position)}),
                ParseElement::Matrix(..) => return Err(if is_inserting {RuleRuntimeError::InsertionMatrix(item.position)}   else {RuleRuntimeError::SubstitutionMatrix(item.position)}),
                &ParseElement::Ipa(mut segment, ref modifiers) => {
                    let mut len = 1;
                    if let Some(mods) = modifiers {
                        segment.apply_seg_mods(&self.alphas, mods.nodes, mods.feats, item.position, false)?;
                        len = match mods.suprs.length {
                            [None, None] => 1,
                            [None, Some(v)] => if v.as_bool(&self.alphas, item.position)? { 3 } else { 1 },
                            [Some(l), None] => if l.as_bool(&self.alphas, item.position)? { 2 } else { 1 },
                            [Some(l), Some(v)] => match (l.as_bool(&self.alphas, item.position)?, v.as_bool(&self.alphas, item.position)?) {
                                (true, true)   => 3,
                                (true, false)  => 2,
                                (false, false) => 1,
                                (false, true)  => Err(RuleRuntimeError::OverlongPosLongNeg(item.position))?,
                            },
                        };
                        // TODO: Ignore syll suprs?
                    }
                    for _ in 0..len {
                        syll.segments.push_back(segment);
                    }
                
                },
                ParseElement::Variable(num, modifiers) => {
                    if let Some(var) = self.variables.borrow_mut().get(&num.value.parse().unwrap()) {
                        match var {
                            VarKind::Syllable(_) => return Err(RuleRuntimeError::SyllVarInsideStruct(item.position)),
                            &VarKind::Segment(mut segment) => {
                                let mut len = 1;
                                if let Some(mods) = modifiers {
                                    segment.apply_seg_mods(&self.alphas, mods.nodes, mods.feats, item.position, false)?;
                                    len = match mods.suprs.length {
                                        [None, None] => 1,
                                        [None, Some(v)] => if v.as_bool(&self.alphas, item.position)? { 3 } else { 1 },
                                        [Some(l), None] => if l.as_bool(&self.alphas, item.position)? { 2 } else { 1 },
                                        [Some(l), Some(v)] => match (l.as_bool(&self.alphas, item.position)?, v.as_bool(&self.alphas, item.position)?) {
                                            (true, true)   => 3,
                                            (true, false)  => 2,
                                            (false, false) => 1,
                                            (false, true)  => Err(RuleRuntimeError::OverlongPosLongNeg(item.position))?,
                                        },
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
                // ParseElement::Optional(vec, _, _) => unimplemented!(),
                _ => unreachable!()
            }
        }

        if let Some(v) = var {
            self.variables.borrow_mut().insert(*v, VarKind::Syllable(syll.clone()));
        }

        Ok(syll)
    }
    
    // TODO: break this up
    fn substitution(&self, word: &Word, input: Vec<MatchElement>, next_pos: &mut Option<SegPos>) -> Result<Word, RuleRuntimeError> {
        // the SegPositions captured in input will not be correct if we change the length of a segment
        // therefore we must keep track of a change in a syllable's length and update the SegPositions accordingly
        let mut total_len_change: Vec<i8> = vec![0; word.syllables.len()];
        let mut last_pos = SegPos::new(0, 0);
        
        let mut res_word = word.clone();
        for (state_index, (in_state, out_state)) in self.input.iter().zip(&self.output).enumerate() {
            match &out_state.kind {
                ParseElement::Syllable(..) => return Err(RuleRuntimeError::SubstitutionSyll(out_state.position)),
                ParseElement::Structure(items, stress, tone, var) => {
                    match input[state_index] {
                        MatchElement::Syllable(sp, _) => {
                            // Replace syllable segments with that which is in the structure
                            // Apply modifiers
                            let syll = res_word.syllables.get_mut(sp).expect("SyllPos is validated index");

                            syll.segments = self.gen_syll_from_struct(items, stress, tone, var, out_state.position, false)?.segments;

                            let suprs = SupraSegs { stress: *stress, length: [None, None], tone: *tone };
                            // Takes care of var aswell
                            self.apply_syll_mods(&mut res_word, sp, &suprs, var, out_state.position)?; 

                            last_pos.syll_index = sp + 1;
                            last_pos.seg_index = 0;
                            if state_index >= self.output.len()-1 {
                                last_pos.decrement(&res_word);
                            }

                        },
                        MatchElement::Segment(sp, _) => {
                            // Remove segment and then insert syllable in its place

                            if items.is_empty() {
                                return Err(RuleRuntimeError::SubstitutionSyll(out_state.position))
                            }

                            let insert_syll = self.gen_syll_from_struct(items, stress, tone, var, out_state.position, false)?;

                            let old_syll = res_word.syllables.get_mut(sp.syll_index).unwrap();
                            
                            // If the matched segment is the only segment in the syllable
                            // We can just replace the segment with the new one
                            if old_syll.segments.len() <= 1 {
                                *old_syll = insert_syll;
                                last_pos.syll_index = sp.syll_index + 1;
                                last_pos.seg_index = 0;
                                if state_index >= self.output.len()-1 {
                                    last_pos.decrement(&res_word);
                                }
                                continue;
                            }
                            // If not, we need to remove the segment and split the syllable in two
                            // with the created syllable being inserted in between
                            old_syll.segments.remove(sp.seg_index);

                            let mut new_syll = Syllable::new();
                            new_syll.stress = old_syll.stress;
                            new_syll.tone = old_syll.tone;

                            while old_syll.segments.len() > sp.seg_index {
                                new_syll.segments.push_front(old_syll.segments.pop_back().unwrap());
                            }

                            let mut adjustment = 0;
                            if old_syll.segments.is_empty() {
                                *old_syll = insert_syll;
                            } else {
                                res_word.syllables.insert(sp.syll_index+1, insert_syll);
                                adjustment = 1;
                            }
                            if !new_syll.segments.is_empty() {
                                res_word.syllables.insert(sp.syll_index+1+adjustment, new_syll);
                                last_pos.syll_index = sp.syll_index + 2 + adjustment;
                            } else {
                                last_pos.syll_index = sp.syll_index + 1 + adjustment;
                            }
                            last_pos.seg_index = 0;
                            if state_index >= self.output.len()-1 {
                                last_pos.decrement(&res_word);
                            }
                            
                        },
                        MatchElement::SyllBound(_, _) => return Err(RuleRuntimeError::SubstitutionSyllBound(in_state.position, out_state.position))
                    }
                },
                ParseElement::Matrix(m, v) => {
                    // get match at index and check it's a segment/or syllable and not a boundary and apply changes
                    // if a syllable, make sure to only do Syllable Suprs
                    match input[state_index] {
                        MatchElement::Segment(mut sp, _)   => {
                            // inserting a syll_bound may complicate this
                            match total_len_change[sp.syll_index].cmp(&0) {
                                std::cmp::Ordering::Greater => sp.seg_index += total_len_change[sp.syll_index].unsigned_abs() as usize,
                                std::cmp::Ordering::Less    => sp.seg_index -= total_len_change[sp.syll_index].unsigned_abs() as usize, // Should not underflow
                                _ => {},
                            }
                            last_pos = sp;
                            debug_assert!(res_word.in_bounds(sp));
                            let lc = self.apply_seg_mods(&mut res_word, sp, m, v, out_state.position)?;
                            total_len_change[sp.syll_index] += lc;
                            if lc > 0 {
                                last_pos.seg_index += lc.unsigned_abs() as usize;
                            }
                            if self.input.len() == self.output.len() {
                                if state_index < self.input.len() -1 {
                                    last_pos.seg_index +=1;
                                }
                            } else {
                                last_pos.seg_index +=1;
                            }
                        },
                        MatchElement::Syllable(sp, _)  => {
                            last_pos.syll_index = sp;
                            last_pos.seg_index = 0;
                            self.apply_syll_mods(&mut res_word, sp, &m.suprs, v, out_state.position)?;                            
                        },
                        MatchElement::SyllBound(..)  => return Err(RuleRuntimeError::SubstitutionBoundMod(in_state.position, out_state.position)),
                    }
                },
                ParseElement::Ipa(seg, mods) => match input[state_index] {
                    MatchElement::Segment(mut sp, _) => {
                        match total_len_change[sp.syll_index].cmp(&0) {
                            std::cmp::Ordering::Greater => sp.seg_index += total_len_change[sp.syll_index].unsigned_abs() as usize,
                            std::cmp::Ordering::Less    => sp.seg_index -= total_len_change[sp.syll_index].unsigned_abs() as usize,
                            _ => {},
                        }
                        last_pos = sp;
                        debug_assert!(res_word.in_bounds(sp));
                        // "Replace with output IPA.
                        let lc = res_word.syllables[sp.syll_index].replace_segment(sp.seg_index, seg, mods, &self.alphas, out_state.position)?;
                        total_len_change[sp.syll_index] += lc;
                        if lc > 0 {
                            last_pos.seg_index += lc.unsigned_abs() as usize;
                        }
                        if self.input.len() == self.output.len() {
                            if state_index < self.input.len() -1 {
                                last_pos.seg_index +=1;
                            }
                        } else {
                            last_pos.seg_index +=1;
                        }
                    },    
                    MatchElement::Syllable(..) | MatchElement::SyllBound(..) => return Err(RuleRuntimeError::SubstitutionSylltoMatrix(in_state.position, out_state.position)),
                },
                ParseElement::Variable(num, mods) => {
                    if let Some(var) = self.variables.borrow_mut().get(&num.value.parse().unwrap()) {
                        match (input[state_index], var) {
                            (MatchElement::Segment(mut sp, _), VarKind::Segment(seg)) => {
                                match total_len_change[sp.syll_index].cmp(&0) {
                                    std::cmp::Ordering::Greater => sp.seg_index += total_len_change[sp.syll_index].unsigned_abs() as usize,
                                    std::cmp::Ordering::Less    => sp.seg_index -= total_len_change[sp.syll_index].unsigned_abs() as usize,
                                    _ => {},
                                }
                                last_pos = sp;
                                debug_assert!(res_word.in_bounds(sp));
                                res_word.syllables[sp.syll_index].segments[sp.seg_index] = *seg;
                                if let Some(m) = mods {
                                    let lc = res_word.apply_seg_mods(&self.alphas, m, sp, num.position)?;
                                    total_len_change[sp.syll_index] += lc;
                                    if lc > 0 {
                                        last_pos.seg_index += lc.unsigned_abs() as usize;
                                    }
                                }
                                if self.input.len() == self.output.len() {
                                    if state_index < self.input.len() -1 {
                                        last_pos.seg_index +=1;
                                    }
                                } else {
                                    last_pos.seg_index +=1;
                                }
                            },
                            (MatchElement::Syllable(sp, _), VarKind::Syllable(syll)) => {
                                res_word.syllables[sp] = syll.clone();
                                last_pos.syll_index = sp+1;
                                last_pos.seg_index = 0;
                                if let Some(m) = mods {
                                    res_word.syllables[sp].apply_syll_mods(&self.alphas, &m.suprs, num.position)?;
                                }
                            },
                            (MatchElement::Segment(sp, _), VarKind::Syllable(insert_syll)) => {
                                // Remove segment and then insert syllable in its place
                                let old_syll = res_word.syllables.get_mut(sp.syll_index).unwrap();
                                // If the matched segment is the only segment in the syllable
                                // We can just replace the segment with the new one
                                if old_syll.segments.len() <= 1 {
                                    *old_syll = insert_syll.clone();
                                    last_pos.syll_index = sp.syll_index + 1;
                                    last_pos.seg_index = 0;
                                    if state_index >= self.output.len()-1 {
                                        last_pos.decrement(&res_word);
                                    }
                                    continue;
                                }
                                // If not, we need to remove the segment and split the syllable in two
                                // with the created syllable being inserted in between
                                old_syll.segments.remove(sp.seg_index);

                                let mut new_syll = Syllable::new();
                                new_syll.stress = old_syll.stress;
                                new_syll.tone = old_syll.tone;

                                while old_syll.segments.len() > sp.seg_index {
                                    new_syll.segments.push_front(old_syll.segments.pop_back().unwrap());
                                }

                                let mut adjustment = 0;
                                if old_syll.segments.is_empty() {
                                    *old_syll = insert_syll.clone();
                                } else {
                                    res_word.syllables.insert(sp.syll_index+1, insert_syll.clone());
                                    adjustment = 1;
                                }
                                if !new_syll.segments.is_empty() {
                                    res_word.syllables.insert(sp.syll_index+1+adjustment, new_syll);
                                    last_pos.syll_index = sp.syll_index + 2 + adjustment;
                                } else {
                                    last_pos.syll_index = sp.syll_index + 1 + adjustment;
                                }
                                last_pos.seg_index = 0;
                                if state_index >= self.output.len()-1 {
                                    last_pos.decrement(&res_word);
                                }
                            },
                            (MatchElement::SyllBound(..), VarKind::Segment(..)) |
                            (MatchElement::Syllable(..),  VarKind::Segment(..)) => return Err(RuleRuntimeError::SubstitutionSylltoSeg(in_state.position, out_state.position)),
                            (MatchElement::SyllBound(..), VarKind::Syllable(_), ) => return Err(RuleRuntimeError::SubstitutionSyllBound(in_state.position, out_state.position)),
                        }
                    } else {
                        return Err(RuleRuntimeError::UnknownVariable(num.clone()))
                    }
                },
                ParseElement::Set(set_output) => {
                    // Check that self.input[si] is a set, if not throw RuleRuntimeError::LonelySet(state.position)
                    // Check both sets have the same number of elements 
                    // See which one of the input set matched and use the corresponding in output to substitute
                    match &in_state.kind {
                        ParseElement::Set(set_input) => if set_input.len() == set_output.len() {
                            match input[state_index] {
                                MatchElement::Segment(mut sp, set_index) => {
                                    let Some(i) = set_index else { return Err(RuleRuntimeError::LonelySet(in_state.position)) };
                                    match total_len_change[sp.syll_index].cmp(&0) {
                                        std::cmp::Ordering::Greater => sp.seg_index += total_len_change[sp.syll_index].unsigned_abs() as usize,
                                        std::cmp::Ordering::Less    => sp.seg_index -= total_len_change[sp.syll_index].unsigned_abs() as usize,
                                        _ => {},
                                    }
                                    last_pos = sp;
                                    match &set_output[i].kind {
                                        ParseElement::Ipa(seg, mods) => {
                                            res_word.syllables[sp.syll_index].segments[sp.seg_index] = *seg;
                                            if let Some(m) = mods {
                                                let lc = res_word.apply_seg_mods(&self.alphas, m, sp, set_output[i].position)?;
                                                total_len_change[sp.syll_index] += lc;
                                                if lc > 0 {
                                                    last_pos.seg_index += lc.unsigned_abs() as usize;
                                                }
                                            }
                                            if self.input.len() == self.output.len() {
                                                if state_index < self.input.len() -1 {
                                                    last_pos.seg_index +=1;
                                                }
                                            } else {
                                                last_pos.seg_index +=1;
                                            }
                                        }
                                        ParseElement::Matrix(mods, var) => {
                                            let lc = self.apply_seg_mods(&mut res_word, sp, mods, var, set_output[i].position)?;
                                            total_len_change[sp.syll_index] += lc;
                                            if lc > 0 {
                                                last_pos.seg_index += lc.unsigned_abs() as usize;
                                            }
                                            if self.input.len() == self.output.len() {
                                                if state_index < self.input.len() -1 {
                                                    last_pos.seg_index +=1;
                                                }
                                            } else {
                                                last_pos.seg_index +=1;
                                            }
                                        },
                                        ParseElement::Variable(num, mods) => { 
                                            if let Some(var) = self.variables.borrow_mut().get(&num.value.parse().unwrap()) {
                                                match var {
                                                    VarKind::Segment(seg) => {
                                                        res_word.syllables[sp.syll_index].segments[sp.seg_index] = *seg;
                                                        if let Some(m) = mods {
                                                            let lc = res_word.apply_seg_mods(&self.alphas, m, sp, num.position)?;
                                                            total_len_change[sp.syll_index] += lc;
                                                            if lc > 0 {
                                                                last_pos.seg_index += lc.unsigned_abs() as usize;
                                                            }
                                                        }
                                                        if self.input.len() == self.output.len() {
                                                            if state_index < self.input.len() -1 {
                                                                last_pos.seg_index +=1;
                                                            }
                                                        } else {
                                                            last_pos.seg_index +=1;
                                                        }
                                                    },
                                                    VarKind::Syllable(insert_syll) => {
                                                        let old_syll = res_word.syllables.get_mut(sp.syll_index).unwrap();
                                                        // If the matched segment is the only segment in the syllable
                                                        // We can just replace the segment with the new one
                                                        if old_syll.segments.len() <= 1 {
                                                            *old_syll = insert_syll.clone();
                                                            last_pos.syll_index = sp.syll_index + 1;
                                                            last_pos.seg_index = 0;
                                                            if state_index >= self.output.len()-1 {
                                                                last_pos.decrement(&res_word);
                                                            }
                                                            continue;
                                                        }
                                                        // If not, we need to remove the segment and split the syllable in two
                                                        // with the created syllable being inserted in between
                                                        old_syll.segments.remove(sp.seg_index);

                                                        let mut new_syll = Syllable::new();
                                                        new_syll.stress = old_syll.stress;
                                                        new_syll.tone = old_syll.tone;

                                                        while old_syll.segments.len() > sp.seg_index {
                                                            new_syll.segments.push_front(old_syll.segments.pop_back().unwrap());
                                                        }
                                                        
                                                        let mut adjustment = 0;
                                                        if old_syll.segments.is_empty() {
                                                            *old_syll = insert_syll.clone();
                                                        } else {
                                                            res_word.syllables.insert(sp.syll_index+1, insert_syll.clone());
                                                            adjustment = 1;
                                                        }
                                                        if !new_syll.segments.is_empty() {
                                                            res_word.syllables.insert(sp.syll_index+1+adjustment, new_syll);
                                                            last_pos.syll_index = sp.syll_index + 2 + adjustment;
                                                        } else {
                                                            last_pos.syll_index = sp.syll_index + 1 + adjustment;
                                                        }
                                                        last_pos.seg_index = 0;
                                                        if state_index >= self.output.len()-1 {
                                                            last_pos.decrement(&res_word);
                                                        }
                                                    }
                                                }
                                            } else {
                                                return Err(RuleRuntimeError::UnknownVariable(num.clone()))
                                            }
                                        },
                                        ParseElement::SyllBound |
                                        ParseElement::Syllable(..) => return Err(RuleRuntimeError::SubstitutionSylltoSeg(in_state.position, set_output[i].position)),
                                        ParseElement::WordBound => return Err(RuleRuntimeError::WordBoundSetLocError(set_output[i].position)),
                                        _ => unreachable!(),
                                    }
                                },
                                MatchElement::Syllable(sp, set_index) => {
                                    let Some(i) = set_index else { return Err(RuleRuntimeError::LonelySet(in_state.position)) };
                                    last_pos.syll_index = sp;
                                    last_pos.seg_index = 0;

                                    match &set_output[i].kind {
                                        ParseElement::Matrix(mods, var) => {
                                            self.apply_syll_mods(&mut res_word, sp, &mods.suprs, var, set_output[i].position)?;
                                        },
                                        ParseElement::Syllable(stress, tone, var) => {
                                            let sups = SupraSegs::from(*stress, [None, None], *tone);
                                            self.apply_syll_mods(&mut res_word, sp, &sups, var, set_output[i].position)?;
                                        },
                                        ParseElement::Variable(num, mods) => {
                                            if let Some(var) = self.variables.borrow_mut().get(&num.value.parse().unwrap()) {
                                                match var {
                                                    VarKind::Syllable(syll) => {
                                                        res_word.syllables[sp] = syll.clone();
                                                        if let Some(m) = mods {
                                                            res_word.syllables[sp].apply_syll_mods(&self.alphas, &m.suprs, num.position)?;
                                                        }
                                                    },
                                                    VarKind::Segment(_) => return Err(RuleRuntimeError::SubstitutionSylltoMatrix(in_state.position, num.position)),
                                                }
                                            } else {
                                                return Err(RuleRuntimeError::UnknownVariable(num.clone()))
                                            }
                                        },

                                        ParseElement::Ipa(..) => return Err(RuleRuntimeError::SubstitutionSylltoMatrix(in_state.position, set_output[i].position)),
                                        ParseElement::SyllBound => return Err(RuleRuntimeError::SubstitutionSylltoBound(in_state.position, set_output[i].position)),
                                        ParseElement::WordBound => return Err(RuleRuntimeError::WordBoundSetLocError(set_output[i].position)),
                                        _ => unreachable!(),
                                    }
                                },
                                MatchElement::SyllBound(sp, set_index) => {
                                    let Some(i) = set_index else { return Err(RuleRuntimeError::LonelySet(in_state.position)) };
                                    last_pos.syll_index = sp;
                                    last_pos.seg_index = 0;

                                    match &set_output[i].kind {
                                        ParseElement::SyllBound => continue,
                                        _ => return Err(RuleRuntimeError::SubstitutionSyllBound(in_state.position, out_state.position))
                                    }
                                },
                            }
                        } else { return Err(RuleRuntimeError::UnevenSet(in_state.position, out_state.position)) },
                        _ => return Err(RuleRuntimeError::LonelySet(out_state.position))
                    }
                },
                ParseElement::SyllBound => {
                    // if !pos.at_syll_start() && !pos.at_syll_end(word) {
                    //     // split current syll into two at insert_pos
                    // }
                    if let MatchElement::SyllBound(..) = input[state_index] {
                        continue
                    } else {
                        return Err(RuleRuntimeError::SubstitutionSyllBound(in_state.position, out_state.position))
                    }
                },
                ParseElement::EmptySet   | ParseElement::Metathesis    | 
                ParseElement::Ellipsis   | ParseElement::Optional(..)  | 
                ParseElement::WordBound  | ParseElement::Environment(..) => unreachable!(),
            }
        }

        let mut pos = last_pos;
        if self.output.len() > self.input.len() {
            for z in self.output.iter().skip(self.input.len()) {
                match &z.kind {
                    ParseElement::Ipa(seg, mods) => {
                        if let Some(syll) = res_word.syllables.get_mut(pos.syll_index) { 
                            let lc = syll.insert_segment(pos.seg_index, seg, mods, &self.alphas, z.position)?;
                            if lc > 0 {
                                pos.seg_index += lc.unsigned_abs() as usize;
                            }
                        } else {
                            res_word.syllables.last_mut().unwrap().segments.push_back(*seg);
                            if let Some(m) = mods {
                                let lc = res_word.apply_seg_mods(&self.alphas, m, pos, z.position)?;
                                if lc > 0 {
                                    pos.seg_index += lc.unsigned_abs() as usize;
                                }
                            } 
                        };
                        pos.increment(&res_word);
                    },
                    ParseElement::SyllBound => {
                        if pos.at_syll_start() {
                            // if res_word.out_of_bounds(*pos) {
                            // } else {
                            //     continue;
                            // }
                            // Possibly err if out of bounds, as it leads to unexpected behaviour (see rule::test_sub_insert_syll())
                            continue;
                        } 
                        // split current syll into two at pos
                        // initialise stress & tone as default
                        let mut new_syll = Syllable::new();
                        let syll = res_word.syllables.get_mut(pos.syll_index).unwrap();
    
                        while syll.segments.len() > pos.seg_index {
                            new_syll.segments.push_front(syll.segments.pop_back().unwrap());
                        }
                        res_word.syllables.insert(pos.syll_index+1, new_syll);
                        total_len_change.insert(pos.syll_index+1, 0);
                        pos.syll_index += 1;
                        pos.seg_index = 0;

                    },
                    ParseElement::Syllable(stress, tone, var) => {
                        if pos.at_syll_start() {
                            // apply mods to current syllable
                            if let Some(syll) = res_word.syllables.get_mut(pos.syll_index) {
                                syll.apply_syll_mods(&self.alphas, &SupraSegs { stress: *stress, length: [None, None], tone: *tone }, z.position)?;
                                if let Some(v) = var {
                                    self.variables.borrow_mut().insert(*v, VarKind::Syllable(res_word.syllables[pos.syll_index].clone()));
                                }
                                continue;
                            } else {
                                // Possibly err as out of bounds leads to unexpected behaviour (see rule::test_sub_insert_syll())
                                continue;
                            }
                        } 
                        // split current syll into two at insert_pos
                        // Apply mods to second syll
                        let mut new_syll = Syllable::new();
                        new_syll.apply_syll_mods(&self.alphas, /*&self.variables,*/ &SupraSegs { stress: *stress, length: [None, None], tone: *tone }, z.position)?;

                        let syll = res_word.syllables.get_mut(pos.syll_index).expect("pos should not be out of bounds");

                        while syll.segments.len() > pos.seg_index {
                            new_syll.segments.push_front(syll.segments.pop_back().unwrap());
                        }
                        res_word.syllables.insert(pos.syll_index+1, new_syll);

                        pos.syll_index += 2;
                        pos.seg_index = 0;

                        if let Some(v) = var {
                            self.variables.borrow_mut().insert(*v, VarKind::Syllable(res_word.syllables[pos.syll_index -1].clone()));
                        }
                    },
                    ParseElement::Structure(items, stress, tone, var) => {
                        let insert_syll = self.gen_syll_from_struct(items, stress, tone, var, z.position, true)?;
                        
                        if let Some(v) = var {
                            self.variables.borrow_mut().insert(*v, VarKind::Syllable(insert_syll.clone()));
                        }

                        if pos.at_syll_start() {
                            res_word.syllables.insert(pos.syll_index, insert_syll);
                            pos.syll_index += 1;
                            continue;
                        }

                        let old_syll = res_word.syllables.get_mut(pos.syll_index).expect("pos should not be out of bounds");
                    
                        let mut new_syll = Syllable::new();
                        new_syll.stress = old_syll.stress;
                        new_syll.tone = old_syll.tone;
    
                        while old_syll.segments.len() > pos.seg_index {
                            new_syll.segments.push_front(old_syll.segments.pop_back().unwrap());
                        }

                        let mut adjustment = 0;
                        if old_syll.segments.is_empty() {
                            *old_syll = insert_syll.clone();
                        } else {
                            res_word.syllables.insert(pos.syll_index+1, insert_syll.clone());
                            adjustment = 1;
                        }
                        if !new_syll.segments.is_empty() {
                            res_word.syllables.insert(pos.syll_index+1+adjustment, new_syll);
                            pos.syll_index += 2 + adjustment;
                        } else {
                            pos.syll_index += 1 + adjustment;
                        }
                        
                        pos.seg_index = 0;
                    },
                    ParseElement::Variable(num, mods) => {
                        if let Some(var) = self.variables.borrow().get(&num.value.parse().unwrap()) {
                            match var {
                                VarKind::Segment(seg) => {
                                    if res_word.in_bounds(pos) {
                                        res_word.syllables[pos.syll_index].segments.insert(pos.seg_index, *seg);
                                    } else if let Some(syll) = res_word.syllables.get_mut(pos.syll_index) { 
                                        if pos.seg_index >= syll.segments.len() {
                                            syll.segments.push_back(*seg);
                                        } else {
                                            syll.segments.insert(pos.seg_index, *seg);
                                        }
                                    } else {
                                        res_word.syllables.last_mut().unwrap().segments.push_back(*seg);
                                    }
                                    if let Some(m) = mods {
                                        let lc = res_word.apply_seg_mods(&self.alphas, m, pos, num.position)?;
                
                                        match lc.cmp(&0) {
                                            std::cmp::Ordering::Greater => pos.seg_index += lc.unsigned_abs() as usize,
                                            std::cmp::Ordering::Less    => pos.seg_index -= lc.unsigned_abs() as usize,
                                            _ => {}
                                        }
                                    }          
                                    
                                    if res_word.in_bounds(pos) {
                                        pos.increment(&res_word);
                                    }
                                },
                                VarKind::Syllable(syll) => {
                                    let mut new_syll = syll.clone();
                                    if let Some(m) = mods {
                                        new_syll.apply_syll_mods(&self.alphas, &m.suprs, num.position)?;
                                    }
                                    if pos.at_syll_start() {
                                        res_word.syllables.insert(pos.syll_index, new_syll.clone());
                                        pos.syll_index += 1;

                                    } else {
                                        // split current syllable in two, insert var_syll in between them
                                        let before_syll = res_word.syllables.get_mut(pos.syll_index).unwrap();
                                        let mut after_syll = Syllable::new();
                                        while before_syll.segments.len() > pos.seg_index {
                                            after_syll.segments.push_front(before_syll.segments.pop_back().unwrap());
                                        }
                                        res_word.syllables.insert(pos.syll_index+1, after_syll);
                                        res_word.syllables.insert(pos.syll_index+1, new_syll);

                                        pos.syll_index += 3;
                                        pos.seg_index = 0;
                                    }
                                },
                            }
                        } else {
                            return Err(RuleRuntimeError::UnknownVariable(num.clone()))
                        }
                    },


                    ParseElement::Set(_) => return Err(RuleRuntimeError::LonelySet(z.position)),
                    ParseElement::Matrix(..) => return Err(RuleRuntimeError::InsertionMatrix(z.position)),
                    
                    ParseElement::EmptySet   | ParseElement::Metathesis    | 
                    ParseElement::Ellipsis   | ParseElement::Optional(..)  | 
                    ParseElement::WordBound  | ParseElement::Environment(..) => unreachable!(),
                }
            }
        } else if self.input.len() > self.output.len() {
            // TODO(girv): factor this out
            let start_index = self.input.len() - self.output.len();
            for &z in input.iter().skip(start_index).rev() {
                match z {
                    MatchElement::Segment(mut sp, _) => {
                        match total_len_change[sp.syll_index].cmp(&0) {
                            std::cmp::Ordering::Greater => sp.seg_index += total_len_change[sp.syll_index].unsigned_abs() as usize,
                            std::cmp::Ordering::Less    => sp.seg_index -= total_len_change[sp.syll_index].unsigned_abs() as usize,
                            _ => {},
                        }
                        pos = sp;
                        debug_assert!(res_word.in_bounds(sp));
                        // remove segment                             
                        if res_word.syllables.len() <= 1 && word.syllables[sp.syll_index].segments.len() <= 1 {
                            return Err(RuleRuntimeError::DeletionOnlySeg)
                        }
                        res_word.syllables[sp.syll_index].segments.remove(sp.seg_index);
                        // if that was the only segment in that syllable, remove the syllable
                        if res_word.syllables[sp.syll_index].segments.is_empty() {
                            res_word.syllables.remove(sp.syll_index);
                        }
                        if pos.seg_index > 0 {
                            pos.seg_index -= 1; 
                        }
                    },
                    MatchElement::Syllable(i, _) => {
                        // remove syllable
                        if res_word.syllables.len() <= 1 {
                            return Err(RuleRuntimeError::DeletionOnlySyll)
                        }
                        pos.syll_index = i;
                        pos.seg_index = 0;
                        pos.decrement(&res_word);
                        res_word.remove_syll(i);
                    },
                    MatchElement::SyllBound(i, _) => {
                        // join the two neighbouring syllables
                        // if one has stress and/or tone, joined syll gets them
                        // if they both have stress, highest wins
                        // if they both have tone, join them i.e. ma5a1 > ma:51
                        if res_word.syllables.len() <= 1 {
                            return Err(RuleRuntimeError::DeletionOnlySyll)
                        }
                        if i == 0 || i >= res_word.syllables.len() {
                            // can't delete a word boundary
                            continue;
                        }
                        pos.syll_index = i;
                        pos.seg_index = 0;
                        pos.decrement(&res_word);

                        let mut syll_segs = res_word.syllables[i].segments.clone();
                        res_word.syllables[i-1].segments.append(&mut syll_segs);

                        res_word.syllables[i-1].stress = match (res_word.syllables[i-1].stress, res_word.syllables[i].stress) {
                            (StressKind::Primary, _) | (_, StressKind::Primary) => StressKind::Primary,
                            (StressKind::Secondary,  StressKind::Unstressed) | 
                            (StressKind::Secondary,  StressKind::Secondary)  | 
                            (StressKind::Unstressed, StressKind::Secondary)  => StressKind::Secondary,
                            (StressKind::Unstressed, StressKind::Unstressed) => StressKind::Unstressed,
                        };

                        res_word.syllables[i-1].tone = Self::concat_tone(res_word.syllables[i-1].tone, res_word.syllables[i].tone);
                        res_word.syllables.remove(i);
                    },
                }
            }
        };
        if let Some(next) = next_pos {
            pos.increment(&res_word);
            *next = pos;
        }

        if res_word.syllables.last_mut().unwrap().segments.is_empty() {
            res_word.syllables.pop();
        }
        Ok(res_word)
    }

    fn input_match_at(&self, word: &Word, start_index: SegPos) -> Result<(Vec<MatchElement>, Option<SegPos>), RuleRuntimeError> {
        let mut cur_index = start_index;
        let mut match_begin = None;
        let mut state_index = 0;
        let mut captures: Vec<_> = Vec::new();

        while word.in_bounds(cur_index) {
            if self.input_match_item(&mut captures, &mut cur_index, &mut state_index, word, &self.input)? {
                // if we have a full match
                if state_index > self.input.len() - 1 { 
                    // As matching a syllbound doesn't increment, this is to avoid an infinite loop
                    if self.input.last().unwrap().kind == ParseElement::SyllBound {
                        cur_index.increment(word);
                    }
                    return Ok((captures, Some(cur_index)));
                }
                if match_begin.is_none() { 
                    // if we haven't started matching, we have now
                    match captures.last().expect("") {
                        MatchElement::Segment(sp, _) => match_begin = Some(*sp),
                        MatchElement::Syllable(sp, _) |
                        MatchElement::SyllBound(sp, _) => match_begin = Some(SegPos { syll_index: *sp, seg_index: 0 }),
                    }
                }
                // else continue 
            } else if let Some (x) = match_begin { 
                // if we were in the middle of matching but now don't match, go back to when we started matching +1 and start again
                cur_index = x;
                cur_index.increment(word);
                state_index = 0;
                captures = vec![];
                match_begin = None;
                self.alphas.borrow_mut().clear();
                self.variables.borrow_mut().clear();
            } else {
                // if we weren't in the middle of matching, move on
                cur_index.increment(word);
                self.alphas.borrow_mut().clear();
                self.variables.borrow_mut().clear();
                // NOTE(girv): Should be unnecessary, but safety first!
                state_index = 0;
                captures = vec![];
            }
        }

        if match_begin.is_none() { // if we've got to the end of the word and we haven't began matching
            Ok((vec![], None))
        } else if self.input.last().unwrap().kind == ParseElement::SyllBound {
            // if we've reached the end of the word and the last state is a word boundary
            captures.push(MatchElement::SyllBound(word.syllables.len(), None));
            Ok((captures, None))
        } else { // No Match
            Ok((vec![], None))
        }
    }

    fn input_match_item(
        &self, 
        captures: &mut Vec<MatchElement>, 
        seg_pos: &mut SegPos, 
        state_index: &mut usize,
        word: &Word, 
        states: &[ParseItem], 
    ) -> Result<bool, RuleRuntimeError> {
        let err_pos = states[*state_index].position;
        match &states[*state_index].kind {
            ParseElement::Variable(vt, m) => if self.input_match_var(captures, state_index, vt, m, word, seg_pos, err_pos)? {
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseElement::Ipa(s, m) => if self.input_match_ipa(captures, s, m, word, seg_pos, err_pos)? {
                seg_pos.increment(word);
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseElement::Matrix(m, v) => if self.input_match_matrix(captures, m, v, word, seg_pos, err_pos)? {
                seg_pos.increment(word);
                *state_index += 1;
                Ok(true) 
            } else { Ok(false) },
            ParseElement::Set(s) => if self.input_match_set(captures, state_index, s, word, seg_pos)? {
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseElement::SyllBound => if self.input_match_syll_bound(captures, *seg_pos) {
                // NOTE(girv): Boundaries do not advance seg_index 
                *state_index += 1;
                Ok(true)
            } else { Ok(false) },
            ParseElement::Syllable(s, t, v) => self.input_match_syll(captures, state_index, s, t, v, word, seg_pos),
            ParseElement::Structure(segs, stress, tone, var) => self.input_match_structure(captures, state_index, segs, stress, tone, var, word, seg_pos),
            ParseElement::Ellipsis => self.input_match_ellipsis(captures, word, seg_pos, states, state_index),

            ParseElement::Optional(..) | ParseElement::Environment(_) |
            ParseElement::EmptySet | ParseElement::WordBound | ParseElement::Metathesis  => unreachable!(),
        }
    }

    fn input_match_structure(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, items: &[ParseItem], stress: &[Option<ModKind>; 2], tone: &Option<Tone>, var: &Option<usize>, word: &Word, pos: &mut SegPos) -> Result<bool, RuleRuntimeError> {
        if items.is_empty() {
            return self.input_match_syll(captures, state_index, stress, tone, var, word, pos)
        }
        if !(word.in_bounds(*pos) && pos.seg_index == 0) {
            return Ok(false)
        }

        let cur_syll_index = pos.syll_index;
        let cur_syll = &word.syllables[cur_syll_index];

        if !self.match_stress(stress, cur_syll)? {
            return Ok(false)
        }
        if let Some(t) = tone.as_ref() {
            if !self.match_tone(t, cur_syll) {
                return Ok(false)
            }
        }

        for (mut i, item) in items.iter().enumerate() {
            if pos.syll_index != cur_syll_index {
                return Ok(false)
            }
            match &item.kind {
                ParseElement::Ellipsis => if i == items.len() - 1 {
                    pos.syll_index += 1;
                    pos.seg_index = 0;
                    break;
                } else if self.context_match_ellipis_struct(items, &mut i, word, pos, cur_syll_index)? {
                    break;
                } else { return Ok(false) },
                ParseElement::Ipa(s, mods) => if self.context_match_ipa(s, mods, word, *pos, item.position)? {
                    pos.increment(word);
                } else { return Ok(false) },
                ParseElement::Matrix(mods, var) => if !self.context_match_matrix(mods, var, word, pos, item.position)? {
                    return Ok(false)
                },
                ParseElement::Variable(num, mods) => match self.variables.borrow_mut().get(&num.value.parse::<usize>().unwrap()) {
                    Some(var) => match var {
                        VarKind::Segment(s) => if self.context_match_ipa(s, mods, word, *pos, item.position)? {
                            pos.increment(word);
                        } else { return Ok(false) },
                        VarKind::Syllable(_) => return Err(RuleRuntimeError::SyllVarInsideStruct(item.position)),
                    },
                    None => return Err(RuleRuntimeError::UnknownVariable(num.clone())),
                }
                ParseElement::Optional(..) => unimplemented!(),
                _ => unreachable!()
            }

        }

        if pos.seg_index != 0 { return Ok(false) }

        if let Some(v) = var {
            self.variables.borrow_mut().insert(*v, VarKind::Syllable(word.syllables[pos.syll_index].clone()));
        }
        captures.push(MatchElement::Syllable(cur_syll_index, None));
        *state_index += 1;
        Ok(true)
    }
    
    fn input_match_ellipsis(&self, captures: &mut Vec<MatchElement>, word: &Word, pos: &mut SegPos, states: &[ParseItem], state_index: &mut usize) -> Result<bool, RuleRuntimeError> {
        // should work akin to '.+?' in Regex, that is, a lazy-match of one-or-more elements
        // increment seg_pos
        // save position
        // try to match rest of states
        // if match, return true
        // else return to saved position
        // increment seg_pos
        // repeat until end of word
        
        if *state_index >= states.len() {
            return Ok(true)
        }

        *state_index += 1;
        pos.increment(word);

        while word.in_bounds(*pos) {
            let back_pos = *pos;
            let back_state = *state_index;
            let back_alphas = self.alphas.borrow().clone();
            let back_varlbs = self.variables.borrow().clone();

            let mut m = true;
            while *state_index < states.len() {
                if !self.input_match_item(captures, pos, state_index, word, states)? {
                    m = false;
                    break;
                }
                *state_index += 1;
            }
            if m {
                return Ok(true)
            }
            *state_index = back_state;
            *pos = back_pos;
            *self.alphas.borrow_mut() = back_alphas;
            *self.variables.borrow_mut() = back_varlbs;
            pos.increment(word);
        }
        
        Ok(false)
    }

    fn input_match_syll(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, stress: &[Option<ModKind>;2], tone: &Option<Tone>, var: &Option<usize>, word: &Word, pos: &mut SegPos) -> Result<bool, RuleRuntimeError> {
        // checks current segment is at start of syll
        // matches stress and tone
        // jumps to end of syllable if match
        if word.in_bounds(*pos) && pos.seg_index == 0 {
        // if word.seg_is_syll_initial(*seg_index) {
            let cur_syll_index = pos.syll_index;
            let cur_syll = &word.syllables[cur_syll_index];

            if !self.match_stress(stress, cur_syll)? {
                return Ok(false)
            }
            if let Some(t) = tone.as_ref() {
                if !self.match_tone(t, cur_syll) {
                    return Ok(false)
                }
            }
            if let Some(v) = var {
                self.variables.borrow_mut().insert(*v, VarKind::Syllable(word.syllables[pos.syll_index].clone()));
            }
            captures.push(MatchElement::Syllable(cur_syll_index, None));
            *state_index += 1;
            pos.syll_index += 1;
            pos.seg_index = 0;

            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn concat_tone(prev: Tone, aft: Tone) -> Tone {
        if prev == aft {
            return prev
        }
        if prev == 0 || aft == 0 {
            return prev | aft
        }

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

    fn match_stress(&self, stress: &[Option<ModKind>; 2], syll: &Syllable) -> Result<bool, RuleRuntimeError> {
        // ±stress (+ matches prim and sec, - matches unstressed)
        if let Some(str) = stress[0] {
            match str {
                ModKind::Binary(bm) => match bm {
                    BinMod::Negative => if syll.stress != StressKind::Unstressed { return Ok(false) },
                    BinMod::Positive => if syll.stress == StressKind::Unstressed { return Ok(false) },
                },
                ModKind::Alpha(am) => match am {
                    AlphaMod::Alpha(ch) => {
                        if let Some(alph) = self.alphas.borrow().get(&ch) {
                            let pos = alph.as_binary();
                            match syll.stress {
                                StressKind::Primary |
                                StressKind::Secondary  => if !pos { return Ok(false) },
                                StressKind::Unstressed => if  pos { return Ok(false) },
                            }
                        } else {
                            let stress = syll.stress != StressKind::Unstressed;
                            self.alphas.borrow_mut().insert(ch, Alpha::Supra(stress));
                        }
                    },
                    AlphaMod::InvAlpha(ch) => {
                        if let Some(alph) = self.alphas.borrow().get(&ch) {
                            let pos = alph.as_binary();
                            match syll.stress {
                                StressKind::Primary |
                                StressKind::Secondary  => if  pos { return Ok(false) },
                                StressKind::Unstressed => if !pos { return Ok(false) },
                            }
                        } else {
                            let stress = syll.stress == StressKind::Unstressed;
                            self.alphas.borrow_mut().insert(ch, Alpha::Supra(stress));
                        }
                        
                    },
                },
            }
        }
        // ±secstress (+ matches sec, - matches prim and unstressed)
        if let Some(str) = stress[1] {
            match str {
                ModKind::Binary(bm) => match bm {
                    BinMod::Negative => if syll.stress == StressKind::Secondary { return Ok(false) },
                    BinMod::Positive => if syll.stress != StressKind::Secondary { return Ok(false) },
                },
                ModKind::Alpha(am) => match am {
                    AlphaMod::Alpha(ch) => {
                        if let Some(alph) = self.alphas.borrow().get(&ch) {
                            let pos = alph.as_binary();
                            match syll.stress {
                                StressKind::Secondary  => if !pos { return Ok(false) },
                                StressKind::Primary |
                                StressKind::Unstressed => if  pos { return Ok(false) },
                            }
                        } else {
                            let stress = syll.stress == StressKind::Secondary;
                            self.alphas.borrow_mut().insert(ch, Alpha::Supra(stress));
                        }
                    },
                    AlphaMod::InvAlpha(ch) => {
                        if let Some(alph) = self.alphas.borrow().get(&ch) {
                            let pos = alph.as_binary();
                            match syll.stress {
                                StressKind::Secondary  => if  pos { return Ok(false) },
                                StressKind::Primary |
                                StressKind::Unstressed => if !pos { return Ok(false) },
                            }
                        } else {
                            let stress = syll.stress != StressKind::Secondary;
                            self.alphas.borrow_mut().insert(ch, Alpha::Supra(stress));
                        }
                    },
                },
            }
        }

        Ok(true)
    }

    fn match_tone(&self, tone: &Tone, syll: &Syllable) -> bool {        
        *tone == syll.tone
    }

    fn input_match_syll_bound(&self, captures: &mut Vec<MatchElement>, pos: SegPos) -> bool {
        if pos.seg_index == 0 {
            captures.push(MatchElement::SyllBound(pos.syll_index, None));
            true
        } else {
            false
        }
    }

    fn input_match_set(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, set: &[ParseItem], word: &Word, pos: &mut SegPos) -> Result<bool, RuleRuntimeError> {
        let back_pos = *pos;
        let back_alphas = self.alphas.borrow().clone();
        let back_varlbs = self.variables.borrow().clone();

        for (i,s) in set.iter().enumerate() {
            let res = match &s.kind {
                ParseElement::Variable(vt, mods) => self.input_match_var(captures, state_index, vt, mods, word, pos, s.position),
                ParseElement::Ipa(seg, mods) => if self.input_match_ipa(captures, seg, mods, word, pos, s.position)? {
                    pos.increment(word);
                    Ok(true)
                } else { Ok(false) },
                ParseElement::Matrix(mods, var) => if self.input_match_matrix(captures, mods, var, word, pos, s.position)? {
                    pos.increment(word);
                    Ok(true)
                } else { Ok(false) },
                ParseElement::Syllable(stress, tone, var) => self.input_match_syll(captures, state_index, stress, tone, var, word, pos),
                ParseElement::SyllBound => if pos.at_syll_start() {
                    captures.push(MatchElement::SyllBound(pos.syll_index, Some(i))); // FIXME: `i` is being unnecessarily reassigned
                    Ok(true)
                } else { Ok(false) },
                ParseElement::WordBound => Err(RuleRuntimeError::WordBoundSetLocError(s.position)),
                _ => unreachable!(),
            };
            if res? {
                captures.last_mut().unwrap().set_ind(Some(i));
                return Ok(true)
            }
            *pos = back_pos;
            // TODO: Deal with these clones
            *self.alphas.borrow_mut() = back_alphas.clone();
            *self.variables.borrow_mut() = back_varlbs.clone();
        }
        Ok(false)
    }

    fn input_match_ipa(&self, captures: &mut Vec<MatchElement>, s: &Segment, mods: &Option<Modifiers>, word: &Word, pos: &mut SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        let seg = word.get_seg_at(*pos).unwrap();

        if let Some(m) = mods {
            if self.match_ipa_with_modifiers(s, m, word, pos, err_pos)? {
                captures.push(MatchElement::Segment(*pos, None));
                // the way we implement `long` vowels means we need to do this
                let mut seg_length = word.seg_length_at(*pos);            
                while seg_length > 1 {
                    pos.increment(word);
                    seg_length -= 1;
                }
                Ok(true)
            } else {
                let mut seg_length = word.seg_length_at(*pos);            
                while seg_length > 1 {
                    pos.increment(word);
                    seg_length -= 1;
                }
                Ok(false)
            }
        } else if *s == seg {
            captures.push(MatchElement::Segment(*pos, None));
            // the way we implement `long` vowels means we need to do this
            let mut seg_length = word.seg_length_at(*pos);            
            while seg_length > 1 {
                pos.increment(word);
                seg_length -= 1;
            }
            Ok(true)
        } else {
            // the way we implement `long` vowels means we need to do this
            let mut seg_length = word.seg_length_at(*pos);            
            while seg_length > 1 {
                pos.increment(word);
                seg_length -= 1;
            }
            Ok(false)
        }
    }

    fn input_match_syll_var(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, syll_to_match: &Syllable, mods: &Option<Modifiers>, word: &Word, pos: &mut SegPos) -> Result<bool, RuleRuntimeError> {
        if pos.seg_index != 0 || word.out_of_bounds(*pos){
            return Ok(false)
        }
        let csi = pos.syll_index;
        let cur_syll = &word.syllables[csi];

        if let Some(m) = mods {
            if !self.match_stress(&m.suprs.stress, cur_syll)? {
                return Ok(false)
            } 
            if let Some(t) = &m.suprs.tone.as_ref() {
                if !self.match_tone(t, cur_syll) {
                    return Ok(false)
                }
            }
            if cur_syll.segments != syll_to_match.segments {
                return Ok(false)
            }

        } else if *cur_syll != *syll_to_match {
            return Ok(false)
        }
        captures.push(MatchElement::Syllable(csi, None));

        *state_index += 1;
        pos.syll_index += 1;
        pos.seg_index = 0;

        // Because we are incrementing in the parent function in the case of a match
        // We must jump to the next syllable but not skip the segment at index 0
        // if pos.syll_index < word.syllables.len() {
        //     pos.decrement(word);
        // }
        
        Ok(true)
        
    }

    fn input_match_var(&self, captures: &mut Vec<MatchElement>, state_index: &mut usize, vt: &Token, mods: &Option<Modifiers>, word: &Word, pos: &mut SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        match self.variables.borrow_mut().get(&vt.value.parse::<usize>().unwrap()) {
            Some(var) => match var {
                VarKind::Segment(s)  => if self.input_match_ipa(captures, s, mods, word, pos, err_pos)? {
                    pos.increment(word);
                    Ok(true)
                } else { Ok(false) },
                VarKind::Syllable(s) => self.input_match_syll_var(captures, state_index , s, mods, word, pos),
            },
            None => Err(RuleRuntimeError::UnknownVariable(vt.clone())),
        }
    }

    fn input_match_matrix(&self, captures: &mut Vec<MatchElement>, mods: &Modifiers, var: &Option<usize>, word: &Word, pos: &mut SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> { 
        if self.match_modifiers(mods, word, pos, err_pos)? {
            if let Some(v) = var {
                self.variables.borrow_mut().insert(*v, VarKind::Segment(word.get_seg_at(*pos).unwrap()));
            }
            captures.push(MatchElement::Segment(*pos, None));
            // the way we implement `long` vowels means we need to do this
            let mut seg_length = word.seg_length_at(*pos);            
            while seg_length > 1 {
                pos.increment(word);
                seg_length -= 1;
            }
            Ok(true)
        } else {
            // the way we implement `long` vowels means we need to do this
            let mut seg_length = word.seg_length_at(*pos);            
            while seg_length > 1 {
                pos.increment(word);
                seg_length -= 1;
            }
            Ok(false)
        }
    }

    fn match_ipa_with_modifiers(&self, seg: &Segment, mods: &Modifiers, word: &Word, pos: &SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> {   
        let mut joined_mods = seg.as_modifiers();
        
        for (i, n) in mods.nodes.iter().enumerate() {
            if n.is_some() { joined_mods.nodes[i] = *n }
        }
        for (i, f) in mods.feats.iter().enumerate() {
            if f.is_some() { joined_mods.feats[i] = *f }
        }
        joined_mods.suprs = mods.suprs;

        self.match_modifiers(&joined_mods, word, pos, err_pos)
    }

    fn match_modifiers(&self, mods: &Modifiers, word: &Word, pos: &SegPos, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        let seg = word.get_seg_at(*pos).expect("Segment Position should be within bounds");
        
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
        self.match_supr_mod_seg(word, &mods.suprs, pos)
    }

    fn match_supr_mod_seg(&self, word: &Word, mods: &SupraSegs, pos: &SegPos) -> Result<bool, RuleRuntimeError> {

        let syll = &word.syllables[pos.syll_index];

        if !self.match_stress(&mods.stress, syll)? { return Ok(false) }
        if !self.match_seg_length(word, &mods.length, pos)? { return Ok(false) }

        if let Some(t) = mods.tone.as_ref() {
            return Ok(self.match_tone(t, syll))
        }

        Ok(true)
    }

    fn match_seg_length(&self, word: &Word, length: &[Option<ModKind>; 2], pos: &SegPos) -> Result<bool, RuleRuntimeError> {
        let seg_length = word.seg_length_at(*pos);
        // +/- long
        if let Some(len) = length[0] {
            match len {
                ModKind::Binary(bm) => match bm {
                    BinMod::Positive => if seg_length < 2 { return Ok(false) },
                    BinMod::Negative => if seg_length > 1 { return Ok(false) },
                },
                ModKind::Alpha(am) => match am {
                    AlphaMod::Alpha(ch) => {
                        if let Some(alph) = self.alphas.borrow().get(&ch) {
                            match alph.as_binary() {
                                true  => if seg_length < 2 { return Ok(false) },
                                false => if seg_length > 1 { return Ok(false) },
                            }
                        } else {
                            self.alphas.borrow_mut().insert(ch, Alpha::Supra(seg_length > 1));
                        }
                    },
                    AlphaMod::InvAlpha(ch) => {
                        if let Some(alph) = self.alphas.borrow().get(&ch) {
                            match !alph.as_binary() {
                                true  => if seg_length < 2 { return Ok(false) },
                                false => if seg_length > 1 { return Ok(false) }
                            }
                        } else {
                            self.alphas.borrow_mut().insert(ch, Alpha::Supra(seg_length <= 1));
                        }
                    },
                },
            }
        }
        // +/- overlong
        if let Some(len) = length[1] {
            match len {
                ModKind::Binary(bm) => match bm {
                    BinMod::Positive => if seg_length < 3 { return Ok(false) },
                    BinMod::Negative => if seg_length > 2 { return Ok(false) },
                },
                ModKind::Alpha(am) => match am {
                    AlphaMod::Alpha(ch) => {
                        if let Some(alph) = self.alphas.borrow().get(&ch) {
                            match alph.as_binary() {
                                true  => if seg_length < 3 { return Ok(false) },
                                false => if seg_length > 2 { return Ok(false) },
                            }
                        } else {
                            self.alphas.borrow_mut().insert(ch, Alpha::Supra(seg_length > 2));
                        }

                    },
                    AlphaMod::InvAlpha(ch) => {
                        if let Some(alph) = self.alphas.borrow().get(&ch) {
                            match !alph.as_binary() {
                                true  => if seg_length < 3 { return Ok(false) },
                                false => if seg_length > 2 { return Ok(false) },
                            }
                        } else {
                            self.alphas.borrow_mut().insert(ch, Alpha::Supra(seg_length <= 2));
                        }
                    },
                },
            }
        }
        Ok(true)
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
}

// struct RevIf<I> {
//     iter: I,
//     rev: bool,
// }

// impl<I: DoubleEndedIterator> Iterator for RevIf<I> {
//     type Item = I::Item;
//     fn next(&mut self) -> Option<Self::Item> {
//         if self.rev {
//             self.iter.next_back()
//         } else {
//             self.iter.next()
//         }
//     }
// }

// impl<I: DoubleEndedIterator> DoubleEndedIterator for RevIf<I> {
//     fn next_back(&mut self) -> Option<Self::Item> {
//         if self.rev {
//             self.iter.next()
//         } else {
//             self.iter.next_back()
//         }
//     }
// }

// trait IterExt: Sized {
//     fn rev_if(self, rev: bool) -> RevIf<Self>;
// }
// impl<I: Iterator> IterExt for I {
//     fn rev_if(self, rev: bool) -> RevIf<Self> {
//         RevIf { iter: self, rev }
//     }
// }