use crate::{error::RuleRuntimeError, rule::subrule::{MatchElement, SubRule}, word::{Phrase, SegPos, StressKind}};




impl SubRule {
    fn delete_elements(&self, res_phrase: &mut Phrase, phrase: &Phrase, els: &[MatchElement], pos: &mut SegPos)-> Result<(), RuleRuntimeError> {
        for z in els.iter().rev() {
            match z {
                &MatchElement::Segment(i, _) => {
                    *pos = i;
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
                &MatchElement::LongSegment(i, _) => {
                    *pos = i;
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
                &MatchElement::Syllable(wp, i, _) => {
                    // remove syllable
                    if ((!self.inp_x_bound && !self.env_x_bound) || res_phrase.len() == 1) && res_phrase[wp].syllables.len() <= 1 {
                        return Err(RuleRuntimeError::DeletionOnlySyll)
                    }
                    pos.syll_index = i;
                    pos.seg_index = 0;
                    pos.decrement(res_phrase);
                    res_phrase[wp].remove_syll(i);

                    if res_phrase[wp].syllables.is_empty() && !self.inp_x_bound {
                        res_phrase.remove(wp);
                    }
                },
                &MatchElement::WordBound(wp, _) => {
                    // wp ## wp+1 -> wp.wp+1
                    
                    pos.word_index = wp+1;
                    pos.word_decrement(phrase);

                    let w1 = res_phrase[wp+1].syllables.clone();
                    res_phrase[wp].syllables.extend(w1);
                    res_phrase.remove(wp+1);
                },
                &MatchElement::SyllBound(wp, i, _) => {
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
                    pos.decrement(res_phrase);

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
                MatchElement::Set(els, _, _) => self.delete_elements(res_phrase, phrase, els, pos)?,
            }
        }

        Ok(())
    }

    // TODO: update deletion to the Action System
    pub(super) fn deletion(&self, phrase: &Phrase, input: &[MatchElement], next_pos: &mut Option<SegPos>) -> Result<Phrase, RuleRuntimeError> {
        let word_pos = if let Some(np) = next_pos { np.word_index } else { 0 };
        let mut pos = SegPos::new(word_pos, 0, 0);
        let mut res_phrase = phrase.clone();

        self.delete_elements(&mut res_phrase, phrase, input, &mut pos)?;

        if let Some(next) = next_pos {
            pos.increment(&res_phrase);
            *next = pos;
        }
        Ok(res_phrase)
    }
}