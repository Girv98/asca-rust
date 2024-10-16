use std::{
    collections::{HashMap, VecDeque}, 
    cell::RefCell, 
    fmt, 
};

use crate :: {
    seg   ::Segment, 
    rule  ::Alpha, 
    syll  ::{StressKind, Syllable}, 
    error ::{RuleRuntimeError, WordSyntaxError}, 
    parser::{Modifiers, SupraSegs}, 
    CARDINALS_MAP, CARDINALS_TRIE, DIACRITS 
};

// #[derive(Debug, Clone, Copy, PartialEq, Eq)]
// pub enum LengthKind {
//     Short,
//     Long,
//     Overlong
// }
//
// impl Default for LengthKind {
//     fn default() -> Self {
//         Self::Short
//     }
// }

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct SegPos {
    pub syll_index: usize,
    pub seg_index: usize
}

impl fmt::Debug for SegPos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.syll_index, self.seg_index)
    }
}

impl SegPos {
    pub fn new(syll_index: usize, seg_index: usize) -> Self {
        Self { syll_index, seg_index }
    }

    pub fn increment(&mut self, word: &Word) {
        // NOTE: Does not guarantee that the resulting position is within the bounds of the word
        debug_assert!(self.syll_index < word.syllables.len(), "error incrementing");

        self.seg_index += 1;
        if self.seg_index > word.syllables[self.syll_index].segments.len() - 1 {
            self.seg_index = 0;
            self.syll_index += 1;
        }
    }

    pub fn decrement(&mut self, word: &Word) {
        // debug_assert!(self.syll_index < word.syllables.len(), "error decrementing");

        if self.syll_index > word.syllables.len() {
            self.syll_index = word.syllables.len() - 1;
            self.seg_index = word.syllables[self.syll_index].segments.len() - 1;
        } else if self.seg_index > 0 {
            self.seg_index -= 1;
        } else if self.syll_index > 0 {
            self.syll_index -= 1;
            self.seg_index = word.syllables[self.syll_index].segments.len() - 1;
        }
        // if 0:0, do nothing
    }

    pub fn at_word_start(&self) -> bool {
        self.syll_index == 0 && self.seg_index == 0
    }

    pub fn at_word_end(&self, word: &Word) -> bool {
        self.syll_index == word.syllables.len() - 1 && self.seg_index >= word.syllables[self.syll_index].segments.len() - 1
    }

    pub fn at_syll_start(&self) -> bool {
        // NOTE: does not account for out_of_bounds
        self.seg_index == 0
    }

    pub fn at_syll_end(&self, word: &Word) -> bool {
        // NOTE: returns false if out_of_bounds
        self.syll_index < word.syllables.len() && self.seg_index >= word.syllables[self.syll_index].segments.len() - 1

    }
}

#[derive(Clone)]
pub struct Word {
    // pub segments: Vec<Segment>,
    pub syllables: Vec<Syllable>,
}

impl fmt::Debug for Word {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, syll) in self.syllables.iter().enumerate() {
            for (j, seg) in syll.segments.iter().enumerate() {
                writeln!(f, "{i}:{j} | {seg:?}")?;
            }
        }
        Ok(())
    }
}

impl Word {
    pub fn new(text: String) -> Result<Self, WordSyntaxError>  {
        let mut w = Self { syllables: Vec::new() };
        let t = text.replace('\'', "ˈ")
                    .replace(',',  "ˌ")
                    .replace(':',  "ː")
                    .replace(';',  "ː.")
                    .replace('g',  "ɡ")
                    .replace('?',  "ʔ")
                    .replace('ǝ',  "ə");
        w.setup(t)?;

        Ok(w)
    }

    pub fn render(&self) -> Result<String, (String, usize)> {
        let mut buffer = String::new();
        for (i, syll) in self.syllables.iter().enumerate() {
            match syll.stress {
                StressKind::Primary => buffer.push('ˈ'), 
                StressKind::Secondary => buffer.push('ˌ'),
                StressKind::Unstressed =>  if i > 0 { buffer.push('.') },
            }
            for (j, seg) in syll.segments.iter().enumerate() {
                if j != 0 && *seg == syll.segments[j-1] {
                    buffer.push('ː');
                } else {
                    let Some(x) = &seg.get_as_grapheme() else { return Err((buffer, i)) };
                    buffer.push_str(x);
                }
            }
            buffer.push_str(&syll.tone);
        }
        Ok(buffer)
    }

    pub fn get_segs_in_syll(&self, syll_index: usize) -> &VecDeque<Segment> {
        debug_assert!(syll_index < self.syllables.len());
        &self.syllables[syll_index].segments
    } 

    pub fn get_segs_in_syll_mut(&mut self, syll_index: usize) -> &mut VecDeque<Segment> {
        debug_assert!(syll_index < self.syllables.len());
        &mut self.syllables[syll_index].segments
    } 

    pub fn remove_syll(&mut self, syll_index: usize) {
        debug_assert!(self.syllables.len() > 1);
        debug_assert!(syll_index < self.syllables.len());
        self.syllables.remove(syll_index);
    }

    pub fn swap_syll(&mut self, a_index: usize, b_index: usize) {
        debug_assert!(a_index < self.syllables.len());
        debug_assert!(b_index < self.syllables.len());
        self.syllables.swap(a_index, b_index)
    }

    /// Finds number of consecutive identical segments ***within a syllable*** starting from the given index.
    /// 
    /// Does not take into account if said index is in the middle of the repetition
    /// # Examples
    /// ``` 
    /// let word = Word::new("aa.a").unwrap();
    /// assert_eq!(word.seg_length_at(0), 2);
    /// assert_eq!(word.seg_length_at(1), 1);
    /// ```
    pub fn seg_length_in_syll(&self, seg_index: SegPos) -> usize {

        let syll = &self.syllables[seg_index.syll_index];

        if seg_index.seg_index > syll.segments.len() {
            return 1
        }

        let mut s_i = seg_index.seg_index + 1;
        let mut len = 1;

        while s_i < syll.segments.len() && syll.segments[seg_index.seg_index] == syll.segments[s_i] {
            len +=1;
            s_i += 1;
        }

        len
    }

    pub fn in_bounds(&self, seg_pos: SegPos) -> bool {
        seg_pos.syll_index < self.syllables.len() && seg_pos.seg_index < self.syllables[seg_pos.syll_index].segments.len()
    }

    pub fn out_of_bounds(&self, seg_pos: SegPos) -> bool {
        seg_pos.syll_index >= self.syllables.len() || seg_pos.seg_index >= self.syllables[seg_pos.syll_index].segments.len()
    }

    pub fn get_seg_at(&self, seg_pos: SegPos) -> Option<Segment> {
        if self.in_bounds(seg_pos) {
            Some(self.syllables[seg_pos.syll_index].segments[seg_pos.seg_index])
        } else {
            None
        }
    }

    // pub fn first_diff(before: &Word, after: &Word) -> Option<(usize, isize)> {
    //     let mut first_diff = None;
    //     for (i,( bfr, aft)) in before.syllables.iter().zip(after.syllables.iter()).enumerate() {
    //         print!("{}, ", bfr.segments.len());
    //         println!("{}", aft.segments.len());
    //         let diff = aft.segments.len() as isize - bfr.segments.len() as isize;
    //         if diff != 0 {
    //             first_diff = Some((i, diff));
    //             break;
    //         } 
    //     }
    //     match first_diff {
    //         Some(_) => first_diff,
    //         None => {
    //             if before.syllables.len() > after.syllables.len() {
    //                 let i = after.syllables.len() - 1;
    //                 let diff = -(before.syllables[i].segments.len() as isize);
    //                 Some((i, diff))
    //             } else if before.syllables.len() < after.syllables.len() {
    //                 let i = before.syllables.len() - 1;
    //                 let diff = after.syllables[i].segments.len() as isize;
    //                 Some((i, diff))
    //             } else {
    //                 None
    //             } 
    //         },
    //     }
    // }

    // pub fn match_mod_at(&self, md: &SegMKind, seg_index: usize) -> bool {
    //     todo!()
    // }

    // pub fn match_supra_at(&self, supr: &Supr, seg_index: usize) -> bool {
    //     todo!()
    // }

    fn setup(&mut self, input_txt: String) -> Result<(), WordSyntaxError> {
        let mut i = 0;
        let txt: Vec<char> = input_txt.chars().collect();

        let mut sy = Syllable::new();

        'outer: while i < txt.len() {
            if txt[i] == 'ˌ' || txt[i] == 'ˈ' {
                if !sy.segments.is_empty() {
                    self.syllables.push(sy);
                }

                // Reset for next syllable
                sy = Syllable::new();
                
                // set stress for next syllable
                match txt[i] {
                    'ˌ' => sy.stress = StressKind::Secondary,
                    'ˈ' => sy.stress = StressKind::Primary,
                    _ => unreachable!()
                }

                i+=1;
                continue;
            }

            if txt[i] == '.' || txt[i].is_ascii_digit() {
                if sy.segments.is_empty() {
                    i+=1;           // NOTE: We could (or maybe should) error here, but we can just skip
                    continue;
                }

                if txt[i].is_ascii_digit() {
                    let mut tone_buffer = String::new();
                    while i < txt.len() && txt[i].is_ascii_digit() {
                        tone_buffer.push(txt[i]);
                        i+=1;
                    }
                    i-=1;
                    sy.tone = tone_buffer;                    
                }
                self.syllables.push(sy.clone());
                // Reset syllable for next pass
                sy = Syllable::new();
                i+=1;
                continue;
            }
            if txt[i] == 'ː' {
                if sy.segments.is_empty() {
                    return Err(WordSyntaxError::NoSegmentBeforeColon(input_txt, i))
                }
                sy.segments.push_back(*sy.segments.back().unwrap());
                i += 1;
                continue;
            }

            let mut buffer = txt[i].to_string();
            
            if CARDINALS_TRIE.contains_prefix(buffer.as_str()) {
                i += 1;
                while i < txt.len() {
                    let mut tmp = buffer.clone(); tmp.push(txt[i]);
                    if CARDINALS_TRIE.contains_prefix(tmp.as_str()) {
                        buffer.push(txt[i]);
                        i += 1;
                        continue;
                    }
                    if txt[i] == '^' {
                        tmp.pop();
                        tmp.push('\u{0361}');
                        if CARDINALS_TRIE.contains_prefix(tmp.as_str()) {
                            buffer.push('\u{0361}');
                            i += 1;
                            continue;
                        }
                        tmp.pop();
                        tmp.push('\u{035C}');
                        if CARDINALS_TRIE.contains_prefix(tmp.as_str()) {
                            buffer.push('\u{035C}');
                            i += 1;
                            continue;
                        }
                    }
                    break;
                }
                let maybe_seg = CARDINALS_MAP.get(&buffer);
                if let Some(seg_stuff) = maybe_seg {
                    sy.segments.push_back(*seg_stuff);
                } else {
                    // Is this needed?
                    i -= 1;

                    let last_char = buffer.pop();
                    let last_buffer = buffer;

                    let maybe_seg = if last_buffer.is_empty() {
                        CARDINALS_MAP.get(&last_char.expect("buffer should not be empty").to_string())
                    } else {
                        CARDINALS_MAP.get(&last_buffer)
                    };

                    if let Some(seg) = maybe_seg {
                        sy.segments.push_back(*seg) 
                    } else {
                        return Err(WordSyntaxError::UnknownChar(input_txt, i));
                    }
                }
            } else {
                let c = buffer.chars().next().unwrap();
                for d in DIACRITS.iter() {
                    if c == d.diacrit {
                        match sy.segments.back_mut() {
                            Some(s) => if s.check_and_apply_diacritic(d).is_some() {
                                i += 1;
                                continue 'outer;
                            } else {
                                return Err(WordSyntaxError::DiacriticDoesNotMeetPreReqs(input_txt, i))
                            },
                            None => return Err(WordSyntaxError::DiacriticBeforeSegment(input_txt, i))
                        }
                    }
                }
                return Err(WordSyntaxError::UnknownChar(input_txt, i));
            }
        }
        if sy.segments.is_empty() {
            if !sy.tone.is_empty() || sy.stress != StressKind::Unstressed {
                return Err(WordSyntaxError::CouldNotParse(input_txt));
            } 
        } else {
            self.syllables.push(sy);
        }
        Ok(())

    }

    pub fn apply_supras(&mut self, alphas: &RefCell<HashMap<char, Alpha>>, mods: &SupraSegs, pos: SegPos) -> Result<(), RuleRuntimeError> {
        let seg = self.get_seg_at(pos).expect("pos is in bounds");
        let mut seg_len = self.seg_length_in_syll(pos);
        match mods.length {
            // [long, Overlong]
            [None, None] => {},
            [None, Some(v)] => match v {
                crate::ModKind::Binary(b) => match b {
                    crate::BinMod::Negative => while seg_len > 2 {
                        self.syllables[pos.syll_index].segments.remove(pos.seg_index);
                        seg_len -=1;
                    },
                    crate::BinMod::Positive => while seg_len < 3 {
                        self.syllables[pos.syll_index].segments.insert(pos.seg_index, seg);
                        seg_len +=1;
                    },
                },
                crate::ModKind::Alpha(_) => todo!(),
            },
            [Some(v), None] => match v {
                crate::ModKind::Binary(b) => match b {
                    crate::BinMod::Negative => while seg_len > 1 {
                        self.syllables[pos.syll_index].segments.remove(pos.seg_index);
                        seg_len -= 1;
                    },
                    crate::BinMod::Positive => while seg_len < 2 {
                        self.syllables[pos.syll_index].segments.insert(pos.seg_index, seg);
                        seg_len += 1;
                    },
                },
                crate::ModKind::Alpha(_) => todo!(),
            },
            [Some(_), Some(_)] => todo!(),
        }


        self.syllables[pos.syll_index].apply_mods(alphas, mods)
    }

    pub fn apply_mods(&mut self, alphas: &RefCell<HashMap<char, Alpha>>, mods: &Modifiers, start_pos: SegPos) -> Result<(), RuleRuntimeError> {
        // check seg length, if long then we must apply mods to all occurences (we assume that we are at the start)
        let mut pos = start_pos;

        debug_assert!(self.in_bounds(pos));
        
        let mut seg_len = self.seg_length_in_syll(pos);
        while seg_len > 0 {
            // todo
            let seg = self.syllables[pos.syll_index].segments.get_mut(pos.seg_index).expect("position is in bounds");

            seg.apply_seg_mods(alphas, mods.nodes, mods.feats)?;

            seg_len -= 1;
            pos.increment(self);
        }

        // Really, this should be first so that we don't have to needlessly apply mods if we apply -long
        self.apply_supras(alphas, &mods.suprs, start_pos)
    }
}

#[cfg(test)]
mod word_tests {

    use super::*;

    #[test]
    fn test_get_grapheme() {
        let s = Word::new("n".to_owned()).unwrap().syllables[0].segments[0];
        assert_eq!(s.get_as_grapheme().unwrap(), "n")
    }

    #[test]
    fn test_render_word() {
        let w = Word::new("ˌna.kiˈsa".to_owned()).unwrap();
        assert_eq!(w.render().unwrap(), "ˌna.kiˈsa");

        let w = Word::new(",na.ki'sa".to_owned()).unwrap();
        assert_eq!(w.render().unwrap(), "ˌna.kiˈsa");

        let w = Word::new("ˈna.ki.sa123".to_owned()).unwrap();
        assert_eq!(w.render().unwrap(), "ˈna.ki.sa123");

        let w = Word::new("aɫ.ɫa:h".to_owned()).unwrap();
        assert_eq!(w.render().unwrap(), "aɫ.ɫaːh");

        let w = Word::new("aɫ.ɫa;hu".to_owned()).unwrap();
        assert_eq!(w.render().unwrap(), "aɫ.ɫaː.hu");

        let w = Word::new("ˈɫɫaa".to_owned()).unwrap();
        assert_eq!(w.render().unwrap(), "ˈɫːaː");

        let w = Word::new("ˈt͡saa".to_owned()).unwrap();
        assert_eq!(w.render().unwrap(), "ˈt͡saː");

        let w = Word::new("ˈt^saa".to_owned()).unwrap();
        assert_eq!(w.render().unwrap(), "ˈt͡saː");
    }

    #[test]
    fn test_render_diacritics() {
        // TODO: Test other diacritic combinations
        let w = Word::new("ˈmu.ðr̩".to_owned()).unwrap(); 
        assert_eq!(w.render().unwrap(), "ˈmu.ðr̩");

        let w = Word::new("ˈpʰiːkʲ".to_owned()).unwrap(); 
        assert_eq!(w.render().unwrap(), "ˈpʰiːkʲ");

        let w = Word::new("ˈpʰiikʲ".to_owned()).unwrap(); 
        assert_eq!(w.render().unwrap(), "ˈpʰiːkʲ");
    }

}