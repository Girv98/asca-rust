mod diacritic;
mod phrase;
mod place;
mod seg;
mod syll;
mod feature;

pub use feature::*;
pub(crate) use diacritic::*;
pub use phrase::*;
pub use place::*;
pub use seg::*;
pub use syll::*;

use std :: {
    cell::RefCell, 
    collections::{HashMap, VecDeque}, 
    fmt
};

use crate :: {
    error :: { ASCAError, AliasRuntimeError, RuleRuntimeError, WordSyntaxError }, 
    rule  :: { Alpha, BinMod, ModKind, Modifiers, Position, SupraSegs }, 
    CARDINALS_MAP, CARDINALS_TRIE, DIACRITS
};

use crate  :: alias :: {
    parser :: { AliasParseElement, SegType }, 
    AliasPosition, Transformation
};

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

    pub(crate) fn reversed(&self, word: &Word) -> Self {
        debug_assert!(word.in_bounds(*self));
        SegPos::new(
            word.syllables.len() - 1 - self.syll_index, 
            word.syllables[self.syll_index].segments.len() - 1 - self.seg_index
        )
    }

    pub(crate) fn increment(&mut self, word: &Word) {
        // NOTE: Does not guarantee that the resulting position is within the bounds of the word
        // debug_assert!(self.syll_index < word.syllables.len(), "error incrementing");

        if self.syll_index >= word.syllables.len() {
            return
        }

        self.seg_index += 1;
        if self.seg_index >= word.syllables[self.syll_index].segments.len() {
            self.seg_index = 0;
            self.syll_index += 1;
        }
    }

    pub(crate) fn decrement(&mut self, word: &Word) {
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

    pub(crate) fn at_word_start(&self) -> bool {
        self.syll_index == 0 && self.seg_index == 0
    }

    pub(crate) fn at_word_end(&self, word: &Word) -> bool {
        self.syll_index == word.syllables.len() - 1 && self.seg_index >= word.syllables[self.syll_index].segments.len() - 1
    }

    pub(crate) fn at_syll_start(&self) -> bool {
        // NOTE: does not account for out_of_bounds
        self.seg_index == 0
    }
    #[allow(unused)]
    pub(crate) fn at_syll_end(&self, word: &Word) -> bool {
        // NOTE: returns false if out_of_bounds
        self.syll_index < word.syllables.len() && self.seg_index >= word.syllables[self.syll_index].segments.len() - 1
    }
}

#[derive(Clone)]
pub struct Word {
    pub syllables: Vec<Syllable>,
    americanist: bool
}

impl PartialEq for Word {
    fn eq(&self, other: &Self) -> bool {
        self.syllables == other.syllables
    }
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
    pub fn new(text: String, aliases: &[Transformation]) -> Result<Self, ASCAError>  {
        let mut w = Self { syllables: Vec::new(), americanist: false };
        let t_norm = text.replace('\'', "ˈ")
                    .replace(',', "ˌ")
                    .replace(':', "ː")
                    .replace(';', "ː.");

        let t_amer = t_norm
                    .replace('¢', "t͡s")
                    .replace('ƛ', "t͡ɬ")
                    .replace('λ', "d͡ɮ")
                    .replace('ł', "ɬ")
                    .replace('ñ', "ɲ");

        if t_amer != t_norm {
            w.americanist = true
        }
        w.setup(t_amer, aliases)?;

        Ok(w)
    }

    fn to_ipa(&self, ch: char) -> char {
        match ch {
            'S' => 'ʃ',
            'Z' => 'ʒ',
            'C' => 'ɕ',
            'G' => 'ɢ',
            'N' => 'ɴ',
            'B' => 'ʙ',
            'R' => 'ʀ',
            'X' => 'χ',
            'H' => 'ʜ',
            'A' => 'ɐ',
            'E' => 'ɛ',
            'I' => 'ɪ',
            'O' => 'ɔ',
            'U' => 'ʊ',
            'Y' => 'ʏ',
            'φ' => 'ɸ',
            'g' => 'ɡ',
            '?' => 'ʔ',
            '!' => 'ǃ',
            other => other
        }
    }

    fn alias_apply_mods(&self, seg: &mut Segment, mods: &Modifiers, err_pos: AliasPosition) -> Result<(), AliasRuntimeError> {
        for (i, m) in mods.nodes.iter().enumerate() {
            let node = NodeKind::from_usize(i);
            if let Some(kind) = m {
                match kind {
                    ModKind::Binary(bm) => match bm {
                        BinMod::Negative => match node {
                            NodeKind::Root      => return Err(AliasRuntimeError::NodeCannotBeNone("Root".to_owned(), err_pos)),
                            NodeKind::Manner    => return Err(AliasRuntimeError::NodeCannotBeNone("Manner".to_owned(), err_pos)),
                            NodeKind::Laryngeal => return Err(AliasRuntimeError::NodeCannotBeNone("Largyneal".to_owned(), err_pos)),
                            NodeKind::Place => *seg.place = None, // e.g. Debuccalization
                            _ => seg.set_node(node, None),
                            
                        },
                        BinMod::Positive => match node {
                            NodeKind::Root      => return Err(AliasRuntimeError::NodeCannotBeSome("Root".to_owned(), err_pos)),
                            NodeKind::Manner    => return Err(AliasRuntimeError::NodeCannotBeSome("Manner".to_owned(), err_pos)),
                            NodeKind::Laryngeal => return Err(AliasRuntimeError::NodeCannotBeSome("Largyneal".to_owned(), err_pos)),
                            NodeKind::Place     => return Err(AliasRuntimeError::NodeCannotBeSome("Place".to_owned(), err_pos)),
                            // preserve node if already positive
                            _ if seg.get_node(node).is_none() => seg.set_node(node, Some(0)),
                            _ => {}
                        },
                    },
                    ModKind::Alpha(_) => unreachable!(),
                }
            }
        }

        for (i, m) in mods.feats.iter().enumerate() {
            let (n, f) = FeatKind::from_usize(i).as_node_mask();
            if let Some(kind) = m {
                match kind {
                    ModKind::Binary(bm) => match bm {
                        BinMod::Negative => seg.set_feat(n, f, false),
                        BinMod::Positive => seg.set_feat(n, f, true),
                    },
                    ModKind::Alpha(_) => unreachable!(),
                }
            }
        }

        Ok(())
    }

    fn alias_apply_length(&self, mods: &Modifiers, err_pos: AliasPosition) -> Result<Option<usize>, AliasRuntimeError> {
        match mods.suprs.length {
            [None, None] => Ok(None),
            [None, Some(over)] => match over {
                ModKind::Binary(bm) => match bm {
                    BinMod::Positive => Ok(Some(3)),
                    BinMod::Negative => Ok(Some(1)),
                },
                ModKind::Alpha(_) => unreachable!(),
            },
            [Some(long), None] => match long {
                ModKind::Binary(bm) => match bm {
                    BinMod::Positive => Ok(Some(2)),
                    BinMod::Negative => Ok(Some(1)),
                },
                ModKind::Alpha(_) => unreachable!(),
            },
            [Some(long), Some(over)] => match (long, over) {
                (ModKind::Binary(bl), ModKind::Binary(bo)) => match (bl, bo) {
                    (BinMod::Positive, BinMod::Positive) => Ok(Some(3)),
                    (BinMod::Positive, BinMod::Negative) => Ok(Some(2)),
                    (BinMod::Negative, BinMod::Negative) => Ok(Some(1)),
                    (BinMod::Negative, BinMod::Positive) => Err(AliasRuntimeError::OverlongPosLongNeg(err_pos)),
                },
                _ => unreachable!()
            },
        }
    }

    fn alias_apply_stress(&self, sy: &mut Syllable, mods: &Modifiers, err_pos: AliasPosition) -> Result<(), AliasRuntimeError> {
        match mods.suprs.stress {
            [None, None] => {},
            [None, Some(sec)] => match sec.as_bin_mod().unwrap() {
                BinMod::Positive => sy.stress = StressKind::Secondary,
                BinMod::Negative => if sy.stress == StressKind::Secondary {
                    sy.stress = StressKind::Unstressed;
                },
            },
            [Some(prim), None] => match prim.as_bin_mod().unwrap() {
                BinMod::Positive => sy.stress = StressKind::Primary,
                BinMod::Negative => sy.stress = StressKind::Unstressed,
            },
            [Some(prim), Some(sec)] => match (prim.as_bin_mod().unwrap(), sec.as_bin_mod().unwrap()) {
                (BinMod::Positive, BinMod::Positive) => sy.stress = StressKind::Secondary,
                (BinMod::Positive, BinMod::Negative) => sy.stress = StressKind::Primary,
                (BinMod::Negative, BinMod::Negative) => sy.stress = StressKind::Unstressed,
                (BinMod::Negative, BinMod::Positive) => return Err(AliasRuntimeError::SecStrPosStrNeg(err_pos)),
            },
        }

        Ok(())
    }

    fn fill_segments(&mut self, input_txt: &String, txt: &[char], i: &mut usize, sy: &mut Syllable, aliases: &[Transformation]) -> Result<bool, ASCAError> {
        for alias in aliases {
            if let AliasParseElement::Replacement(string, plus) = &alias.input.kind {
                let back_pos = *i;
                let mut is_match = true;
                for ch in string.chars() {
                    if *i >= txt.len() || ch != txt[*i] {
                        is_match = false;
                    }
                    *i += 1;
                }
                if is_match {
                    match &alias.output.kind {
                        AliasParseElement::Segments(vec) => for segtype in vec {
                            match segtype {
                                SegType::Ipa(seg, modifiers) => {
                                    let mut seg = *seg;
                                    let mut length = 1;
                                    if let Some(mods) = modifiers {
                                        self.alias_apply_mods(&mut seg, mods, alias.output.position)?;
                                        self.alias_apply_stress(sy, mods, alias.output.position)?;
                                        
                                        length = self.alias_apply_length(mods, alias.output.position)?.unwrap_or(1);

                                        if let Some(tn) = &mods.suprs.tone {
                                            sy.tone = *tn;
                                        }
                                    }

                                    for _ in 0..length {
                                        sy.segments.push_back(seg);
                                    }
                                },
                                SegType::Matrix(mods) => if *plus {

                                    let seg_indices = sy.seg_indices();
                                    let Some(pos) = seg_indices.last() else {
                                        Err(AliasRuntimeError::EmptySyllable(alias.input.position))?
                                    };

                                    self.alias_apply_stress(sy, mods, alias.output.position)?;
                                    if let Some(tn) = &mods.suprs.tone {
                                        sy.tone = *tn;
                                    }
                                    
                                    let cur_length = sy.segments.len() - pos;
                                    let maybe_new_length = self.alias_apply_length(mods, alias.output.position)?;
                                    let seg = {
                                        let seg = sy.segments.get_mut(*pos).unwrap();
                                        self.alias_apply_mods(seg, mods, alias.output.position)?;
                                        *seg
                                    };
                                    if let Some(new_length) = maybe_new_length {
                                        match new_length.cmp(&cur_length) {
                                            std::cmp::Ordering::Equal => {},
                                            std::cmp::Ordering::Greater => for _ in cur_length..new_length {
                                                sy.segments.push_back(seg);
                                            },
                                            std::cmp::Ordering::Less => for _ in new_length..cur_length {
                                                sy.segments.pop_back();
                                            },
                                        }
                                    }

                                } else {
                                    self.alias_apply_stress(sy, mods, alias.output.position)?;
                                    if let Some(tn) = &mods.suprs.tone {
                                        sy.tone = *tn;
                                    }
                                    
                                    if mods.nodes.iter().any(|x| x.is_some()) || mods.feats.iter().any(|x| x.is_some()) {
                                        Err(AliasRuntimeError::IndefiniteFeatures(alias.output.position))?
                                    }

                                    if mods.suprs.length.iter().any(|x| x.is_some()) {
                                        Err(AliasRuntimeError::LengthNoSegment(alias.output.position))?
                                    }
                                    
                                },
                            }
                            

                        },
                        _ => unreachable!()
                    }
                    return Ok(true);
                }
                *i = back_pos;
            }
        }

        // GET SEG
        let mut buffer = self.to_ipa(txt[*i]).to_string();
        
        if CARDINALS_TRIE.contains_prefix(buffer.as_str()) {
            *i += 1;
            while *i < txt.len() {
                let mut tmp = buffer.clone(); tmp.push(self.to_ipa(txt[*i]));
                if CARDINALS_TRIE.contains_prefix(tmp.as_str()) {
                    buffer.push(self.to_ipa(txt[*i]));
                    *i += 1;
                    continue;
                }
                if txt[*i] == '^' {
                    tmp.pop();
                    tmp.push('\u{0361}');
                    if CARDINALS_TRIE.contains_prefix(tmp.as_str()) {
                        buffer.push('\u{0361}');
                        *i += 1;
                        continue;
                    }
                    tmp.pop();
                    tmp.push('\u{035C}');
                    if CARDINALS_TRIE.contains_prefix(tmp.as_str()) {
                        buffer.push('\u{035C}');
                        *i += 1;
                        continue;
                    }

                    if let Some(ch) = txt.get(*i + 1) {
                        // if a click consonant
                        if let 'ʘ' | 'ǀ' | 'ǁ' | 'ǃ'  | '‼' | 'ǂ' = self.to_ipa(*ch) {
                            tmp.pop(); tmp.push(self.to_ipa(txt[*i]));
                            *i += 1;
                            continue;
                        }
                        // if a contour click
                        if let 'q'| 'ɢ' | 'ɴ' | 'χ' | 'ʁ' = self.to_ipa(*ch) {
                            if let Some('ʘ' | 'ǀ' | 'ǁ' | 'ǃ' | '‼' | 'ǂ') = tmp.chars().next() {
                                tmp.pop(); tmp.push(self.to_ipa(txt[*i]));
                                *i += 1;
                                continue;
                            }
                        }
                    }
                }
                break;
            }
            let maybe_seg = CARDINALS_MAP.get(&buffer);
            if let Some(seg_stuff) = maybe_seg {
                sy.segments.push_back(*seg_stuff);
                Ok(false)
            } else {
                // Is this needed?
                *i -= 1;

                let last_char = buffer.pop();
                let last_buffer = buffer;

                let maybe_seg = if last_buffer.is_empty() {
                    CARDINALS_MAP.get(&last_char.expect("buffer should not be empty").to_string())
                } else {
                    CARDINALS_MAP.get(&last_buffer)
                };

                if let Some(seg) = maybe_seg {
                    sy.segments.push_back(*seg) ;
                    Ok(false)
                } else {
                    Err(WordSyntaxError::UnknownChar(input_txt.to_string(), *i).into())
                }
            }
        } else {
            let c = buffer.chars().next().unwrap();
            for d in DIACRITS.iter() {
                if c == d.diacrit {
                    match sy.segments.back_mut() {
                        Some(s) => match s.check_and_apply_diacritic(d) {
                            Ok(_) => {
                                *i += 1;
                                return Ok(true)
                            },
                            Err((mod_index, is_node)) => {
                                if !is_node {
                                    let ft = FeatKind::from_usize(mod_index);
                                    let pos = match d.prereqs.feats[mod_index].unwrap() {
                                        ModKind::Binary(bin_mod) => bin_mod == BinMod::Positive,
                                        _ => unreachable!(),
                                    };
                                    return Err(WordSyntaxError::DiacriticDoesNotMeetPreReqsFeat(input_txt.to_string(), *i, ft.to_string(), pos).into())
                                } else {
                                    let nt = NodeKind::from_usize(mod_index);
                                    let pos = match d.prereqs.nodes[mod_index].unwrap() {
                                        ModKind::Binary(bin_mod) => bin_mod == BinMod::Positive,
                                        _ => unreachable!(),
                                    };
                                    return Err(WordSyntaxError::DiacriticDoesNotMeetPreReqsNode(input_txt.to_string(), *i, nt.to_string(), pos).into())
                                };
                            },
                        },
                        None => return Err(WordSyntaxError::DiacriticBeforeSegment(input_txt.to_string(), *i).into())
                    }
                }
            }
            Err(WordSyntaxError::UnknownChar(input_txt.to_string(), *i).into())
        }
    }

    fn setup(&mut self, input_txt: String, aliases: &[Transformation]) -> Result<(), ASCAError> {
        let mut i = 0;
        let txt: Vec<char> = input_txt.chars().collect();

        let mut sy = Syllable::new();

        while i < txt.len() {

            // Primary or Secondary Stress
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
            // Break or Tone
            if txt[i] == '.' || txt[i].is_ascii_digit() {
                if sy.segments.is_empty() {
                    i+=1;           // NOTE: We could (or maybe should) error here, but we can just skip
                    continue;
                }
                // Tone
                if txt[i].is_ascii_digit() {
                    let mut tone_buffer = String::new();
                    while i < txt.len() && txt[i].is_ascii_digit() {
                        tone_buffer.push(txt[i]);
                        i+=1;
                    }
                    tone_buffer = tone_buffer.replace('0', "");
                    if tone_buffer.chars().count() > 4 {
                        Err(WordSyntaxError::ToneTooBig(input_txt.clone(), i-tone_buffer.chars().count()))?
                    }
                    sy.tone = tone_buffer.parse().unwrap_or(0);
                    i-=1;
                }
                self.syllables.push(sy.clone());
                // Reset syllable for next pass
                sy = Syllable::new();
                i+=1;
                continue;
            }
            // Length
            if txt[i] == 'ː' {
                if sy.segments.is_empty() {
                    return Err(WordSyntaxError::NoSegmentBeforeColon(input_txt, i).into())
                }
                sy.segments.push_back(*sy.segments.back().unwrap());
                i += 1;
                continue;
            }
            
            // GET SEG
            if self.fill_segments(&input_txt, &txt, &mut i, &mut sy, aliases)? {
                continue;
            }
        }
        if sy.segments.is_empty() {
            if sy.tone != 0 || sy.stress != StressKind::Unstressed {
                if sy.stress == StressKind::Primary {
                    if let Some(syll) = self.syllables.last() {
                        if !syll.segments.is_empty() {
                            return Err(WordSyntaxError::CouldNotParseEjective(input_txt).into())
                        }
                    }
                }
                return Err(WordSyntaxError::CouldNotParse(input_txt).into());
            } 
        } else {
            self.syllables.push(sy);
        }
        Ok(())

    }

    fn render_normal(&self) -> String {
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
                    buffer.push_str(&seg.get_as_grapheme().unwrap_or("�".to_owned()));
                }
            }
            if syll.tone != 0 {
                buffer.push_str(&syll.tone.to_string());
            }
        }

        if self.americanist {
            buffer.replace("t͡s", "¢")
                  .replace("t͡ɬ",  "ƛ")
                  .replace("d͡ɮ", "λ")
                  .replace("ɬ",  "ł")
                  .replace("ɲ",  "ñ")
        } else {
            buffer
        }
    }

    fn alias_match_node(&self, seg: Segment, node: NodeKind, val: &ModKind) -> bool {
        match val {
            ModKind::Binary(bt) => if node == NodeKind::Place {
                let x = seg.get_node(NodeKind::Labial).is_some() 
                || seg.get_node(NodeKind::Coronal).is_some()
                || seg.get_node(NodeKind::Dorsal).is_some()
                || seg.get_node(NodeKind::Pharyngeal).is_some();
                match bt {
                    BinMod::Positive => x,
                    BinMod::Negative => !x,
                }
            } else {
                match bt {
                    BinMod::Positive => seg.get_node(node).is_some(),
                    BinMod::Negative => seg.get_node(node).is_none(),
                }
            },
            ModKind::Alpha(_) => unreachable!(),
        }
    }

    fn alias_match_seg_kind(&self, kind: &ModKind, seg: Segment, node: NodeKind, mask: u8) -> bool {
        match kind {
            ModKind::Binary(bt) => match bt {
                BinMod::Negative => seg.feat_match(node, mask, false),
                BinMod::Positive => seg.feat_match(node, mask, true),
            },
            ModKind::Alpha(_) => unreachable!(),
        }
    }

    fn alias_match_node_mod(&self, mod_kind: &Option<ModKind>, node_index: usize, seg: Segment) -> bool {
        if let Some(kind) = mod_kind {
            let node = NodeKind::from_usize(node_index);
            return self.alias_match_node(seg, node, kind)
        }
        true
    }

    fn alias_match_feat_mod(&self, md: &Option<ModKind>, feat_index: usize, seg: Segment) -> bool {
        if let Some(kind) = md { 
            let (node, mask) = FeatKind::from_usize(feat_index).as_node_mask();
            return self.alias_match_seg_kind(kind, seg, node, mask)
        }
        true
    }

    fn alias_match_stress(&self, stress: &[Option<ModKind>; 2], syll: &Syllable) -> bool {
        if let Some(str) = stress[0] {
            match str {
                ModKind::Binary(bm) => match bm {
                    BinMod::Negative => if syll.stress != StressKind::Unstressed { return false },
                    BinMod::Positive => if syll.stress == StressKind::Unstressed { return false },
                },
                ModKind::Alpha(_) => unreachable!(),
            }
        }
        if let Some(str) = stress[1] {
            match str {
                ModKind::Binary(bm) => match bm {
                    BinMod::Negative => if syll.stress == StressKind::Secondary { return false },
                    BinMod::Positive => if syll.stress != StressKind::Secondary { return false },
                },
                ModKind::Alpha(_) => unreachable!(),
            }
        }
        true
    }

    fn alias_match_seg_length(&self, length: &[Option<ModKind>; 2], syll_index: usize, seg_index: usize) -> (bool, Option<usize>) {
        let seg_length = self.seg_length_at(SegPos { syll_index, seg_index });
        let mut matched_len = false;
        if let Some(len) = length[0] {
            matched_len = true;
            match len {
                ModKind::Binary(bm) => match bm {
                    BinMod::Positive => if seg_length < 2 { return (false, None) },
                    BinMod::Negative => if seg_length > 1 { return (false, None) },
                },
                ModKind::Alpha(_) => unreachable!(),
            }
        }
        if let Some(len) = length[1] {
            matched_len = true;
            match len {
                ModKind::Binary(bm) => match bm {
                    BinMod::Positive => if seg_length < 3 { return (false, None) },
                    BinMod::Negative => if seg_length > 2 { return (false, None) },
                },
                ModKind::Alpha(_) => unreachable!(),
            }
        }

        if matched_len {
            (true, Some(seg_length))
        } else {
            (true, None)
        }
    }

    fn alias_match_supr_mod_seg(&self, mods: &SupraSegs, syll_index: usize, seg_index: usize) -> (bool, Option<usize>, bool) {
        let syll = &self.syllables[syll_index];

        if !self.alias_match_stress(&mods.stress, syll) { return (false, None, false) }

        let is_matching_tone = if let Some(t) = mods.tone.as_ref() {
            if *t != syll.tone { return (false, None, true) }
            true
        } else { false };

        let (is_match, is_matching_len) = self.alias_match_seg_length(&mods.length, syll_index, seg_index);

        (is_match, is_matching_len, is_matching_tone)
    }

    fn alias_match_modifiers(&self, syll_index: usize, seg_index: usize, mods: &Modifiers) -> (bool, Option<usize>, bool) {
        let seg = self.syllables[syll_index].segments[seg_index];

        for (i, m) in mods.feats.iter().enumerate() {
            if !self.alias_match_feat_mod(m, i, seg) {
                return (false, None, false)
            }
        }
        for (i, m) in mods.nodes.iter().enumerate() {
            if !self.alias_match_node_mod(m, i, seg) {
                return (false, None, false)
            }
        }
        self.alias_match_supr_mod_seg( &mods.suprs, syll_index, seg_index)
    }

    fn alias_match_ipa_with_mods(&self, syll_index: usize, seg_index: usize, ipa: &Segment, mods: &Modifiers) -> (bool, Option<usize>, bool) {
        let mut joined_mods = ipa.as_modifiers();
        for (i, n) in mods.nodes.iter().enumerate() {
            if n.is_some() { joined_mods.nodes[i] = *n }
        }
        for (i, f) in mods.feats.iter().enumerate() {
            if f.is_some() { joined_mods.feats[i] = *f }
        }
        joined_mods.suprs = mods.suprs;

        self.alias_match_modifiers(syll_index, seg_index, &joined_mods)   
    }

    pub(crate) fn render(&self, aliases: &[Transformation]) -> String {
        if aliases.is_empty() {
            return self.render_normal()
        }

        let empty: String = String::new();
        let mut bound_repl_str = None;
        let mut strip_tone = false;

        let mut buffer = String::new();

        for (i, syll) in self.syllables.iter().enumerate() {
            match syll.stress {
                StressKind::Primary => buffer.push('ˈ'), 
                StressKind::Secondary => buffer.push('ˌ'),
                StressKind::Unstressed =>  if i > 0 { buffer.push('.') },
            }

            let mut j = 0;
            'outer: while j < syll.segments.len() {
                if j != 0 && syll.segments[j] == syll.segments[j-1] {
                    // TODO: Need to skip if we matched length last time
                    buffer.push('ː');
                    j+=1; continue;
                }

                for alias in aliases {
                    if let AliasParseElement::Segments(segments) = &alias.input.kind {
                        let back_pos = j;
                        let mut is_match = true;
                        let mut plus_match_len = false;
                        for segtype in segments {
                            if j >= syll.segments.len() {
                                is_match = false; break;
                            }
                            match segtype {
                                SegType::Ipa(segment, modifiers) => {
                                    if let Some(mods) = modifiers {
                                        let (m, maybe_len, maybe_tone) = self.alias_match_ipa_with_mods(i, j, segment, mods);
                                        if !m { is_match = false; break; }
                                        if let Some(len) = maybe_len { j+=len; } else { j+=1; }
                                        if maybe_tone { strip_tone = true; }
                                    } else {
                                        if j >= syll.segments.len() || syll.segments[j] != *segment {
                                            is_match = false; break;
                                        }
                                        j+=1;
                                    }
                                },
                                SegType::Matrix(modifiers) => {
                                    let (m, maybe_len, maybe_tone) = self.alias_match_modifiers(i, j, modifiers);
                                    if !m { is_match = false; break; }
                                    if let Some(len) = maybe_len { j+=len; plus_match_len = true; } else { j+=1; }
                                    if maybe_tone { strip_tone = true; }
                                },
                            }
                        }
                        if is_match {
                            match &alias.output.kind {
                                AliasParseElement::Replacement(repl, plus) => {
                                    if *plus {
                                        if !plus_match_len {
                                            for ind in back_pos..j {
                                                buffer.push_str(&syll.segments[ind].get_nearest_grapheme());
                                            }
                                        } else {
                                            buffer.push_str(&syll.segments[j-1].get_nearest_grapheme());
                                        }
                                    } 
                                    buffer.push_str(repl);
                                },
                                AliasParseElement::Empty => {},
                                _ => unreachable!()
                            }
                            continue 'outer; 
                        }
                        j = back_pos;
                    }

                }

                buffer.push_str(&syll.segments[j].get_as_grapheme().unwrap_or("�".to_owned()));
                j += 1;
            }

            for alias in aliases {
                if alias.input.kind == AliasParseElement::SyllBound {
                    match &alias.output.kind {
                        AliasParseElement::Replacement(repl, _) => bound_repl_str = Some(repl),
                        AliasParseElement::Empty => bound_repl_str = Some(&empty),
                        _ => unreachable!()
                    }
                }
            }

            if !strip_tone && syll.tone != 0{
                buffer.push_str(&syll.tone.to_string());
            }
        }

        if let Some(b_repl) = bound_repl_str {
            if !b_repl.is_empty() && buffer.starts_with(['ˈ', 'ˌ']) {
                let mut chars = buffer.chars();
                chars.next();
                buffer = chars.as_str().to_string();
            }
            buffer = buffer.replace(['.', 'ˈ', 'ˌ'], b_repl);
        }

        buffer
    }
    
    pub fn get_syll_segments(&self, syll_index: usize) -> Option<&VecDeque<Segment>> {
        Some(&self.syllables.get(syll_index)?.segments)
    } 
    
    pub fn get_syll_segments_mut(&mut self, syll_index: usize) -> Option<&mut VecDeque<Segment>> {
        Some(&mut self.syllables.get_mut(syll_index)?.segments)
    } 

    /// Removes and returns the syllable at `syll_index` within the word, shifting all elements after it to the left.
    ///
    /// # Panics
    ///
    /// Panics if `syll_index` is out of bounds.
    pub fn remove_syll(&mut self, syll_index: usize) {
        debug_assert!(self.syllables.len() > 1);
        debug_assert!(syll_index < self.syllables.len());
        self.syllables.remove(syll_index);
    }
    /// Swaps two syllable in the word.
    ///
    /// If `a` equals to `b`, it's guaranteed that elements won't change value.
    ///
    /// # Arguments
    ///
    /// * `a` - The index of the first syllable
    /// * `b` - The index of the second syllable
    ///
    /// # Panics
    ///
    /// Panics if `a` or `b` are out of bounds.
    pub fn swap_sylls(&mut self, a: usize, b: usize) {
        debug_assert!(a < self.syllables.len());
        debug_assert!(b < self.syllables.len());
        self.syllables.swap(a, b)
    }

    /// Finds number of consecutive identical segments ***within a syllable*** starting from the given index.
    /// <br>
    /// Does not take into account if said index is in the middle of the repetition
    /// # Panics
    /// If SegPos is out of bounds
    pub fn seg_length_at(&self, seg_index: SegPos) -> usize {
        self.syllables[seg_index.syll_index].get_seg_length_at(seg_index.seg_index)
    }

    // pub(crate) fn find_start_of_long_seg(&self, seg_pos: SegPos) -> SegPos {
    //     if seg_pos.seg_index == 0 {
    //         return seg_pos
    //     }
    //     let syll = &self.syllables[seg_pos.syll_index];
    //     let mut s_i = seg_pos.seg_index - 1;
    //     while s_i > 0 && syll.segments[seg_pos.seg_index] == syll.segments[s_i] {
    //         s_i -= 1;
    //     }
    //     SegPos { syll_index: seg_pos.syll_index, seg_index: s_i }
    // }

    pub(crate) fn in_bounds(&self, seg_pos: SegPos) -> bool {
        seg_pos.syll_index < self.syllables.len() && seg_pos.seg_index < self.syllables[seg_pos.syll_index].segments.len()
    }

    pub(crate) fn out_of_bounds(&self, seg_pos: SegPos) -> bool {
        seg_pos.syll_index >= self.syllables.len() || seg_pos.seg_index >= self.syllables[seg_pos.syll_index].segments.len()
    }

    pub(crate) fn get_seg_at(&self, seg_pos: SegPos) -> Option<Segment> {
        if self.in_bounds(seg_pos) {
            Some(self.syllables[seg_pos.syll_index].segments[seg_pos.seg_index])
        } else {
            None
        }
    }

    pub(crate) fn apply_seg_mods(&mut self, alphas: &RefCell<HashMap<char, Alpha>>, mods: &Modifiers, start_pos: SegPos, err_pos: Position) -> Result<i8, RuleRuntimeError> {
        self.syllables[start_pos.syll_index].apply_seg_mods(alphas, mods, start_pos.seg_index, err_pos)
    }
    
    // This is not efficient in the slightest
    // but it allows us to properly bounds check when matching the before context
    pub(crate) fn reverse(&self) -> Self {
        let mut word = self.clone();
        for syll in &mut word.syllables {
            syll.segments.make_contiguous().reverse();
        }
        word.syllables.reverse();

        word
    }
}

#[cfg(test)]
mod word_tests {

    use crate::{alias::{lexer::AliasLexer, parser::AliasParser, AliasKind}, normalise};

    use super::*;

    #[test]
    fn test_get_grapheme() {
        let s = Word::new("n".to_owned(), &[]).unwrap().syllables[0].segments[0];
        assert_eq!(s.get_as_grapheme(), Some("n".to_owned()));

        let s = Word::new("x".to_owned(), &[]).unwrap().syllables[0].segments[0];
        assert_eq!(s.get_as_grapheme(), Some("x".to_owned()));

        let s = Word::new("xʷ".to_owned(), &[]).unwrap().syllables[0].segments[0];
        assert_eq!(s.get_as_grapheme(), Some("xʷ".to_owned()));
    }

    #[test]
    fn test_render_word() {
        let w = Word::new(normalise("ˌna.kiˈsa"), &[]).unwrap();
        assert_eq!(w.render(&[]), "ˌna.kiˈsa");

        let w = Word::new(normalise(",na.ki'sa"), &[]).unwrap();
        assert_eq!(w.render(&[]), "ˌna.kiˈsa");

        let w = Word::new(normalise("ˈna.ki.sa123"), &[]).unwrap();
        assert_eq!(w.render(&[]), "ˈna.ki.sa123");

        let w = Word::new(normalise("aɫ.ɫa:h"), &[]).unwrap();
        assert_eq!(w.render(&[]), "aɫ.ɫaːh");

        let w = Word::new(normalise("aɫ.ɫa;hu"), &[]).unwrap();
        assert_eq!(w.render(&[]), "aɫ.ɫaː.hu");

        let w = Word::new(normalise("ˈɫɫaa"), &[]).unwrap();
        assert_eq!(w.render(&[]), "ˈɫːaː");

        let w = Word::new(normalise("ˈt͡saa"), &[]).unwrap();
        assert_eq!(w.render(&[]), "ˈt͡saː");

        let w = Word::new(normalise("ˈt^saa"), &[]).unwrap();
        assert_eq!(w.render(&[]), "ˈt͡saː");

        let w = Word::new(normalise("ɴǃa"), &[]).unwrap();
        assert_eq!(w.render(&[]), "ɴǃa");

        let w = Word::new(normalise("ǃɴa"), &[]).unwrap();
        assert_eq!(w.render(&[]), "ǃɴa");
    }

    #[test]
    fn test_render_tone() {
        let w = Word::new(normalise("ma12"), &[]).unwrap(); 
        assert_eq!(w.render(&[]), "ma12");

        let w = Word::new(normalise("ma196"), &[]).unwrap(); 
        assert_eq!(w.render(&[]), "ma196");

        let w = Word::new(normalise("ma51"), &[]).unwrap(); 
        assert_eq!(w.render(&[]), "ma51");

        let w = Word::new(normalise("ma05"), &[]).unwrap(); 
        assert_eq!(w.render(&[]), "ma5");

        let w = Word::new(normalise("ma00"), &[]).unwrap(); 
        assert_eq!(w.render(&[]), "ma");

        let w = Word::new(normalise("ma1965"), &[]).unwrap(); 
        assert_eq!(w.render(&[]), "ma1965");
    }

    #[test]
    fn test_render_diacritics() {
        // TODO: Test other diacritic combinations
        let w = Word::new(normalise("ˈmu.ðr̩"), &[]).unwrap(); 
        assert_eq!(w.render(&[]), "ˈmu.ðr̩");

        let w = Word::new(normalise("ˈpʰiːkʲ"), &[]).unwrap(); 
        assert_eq!(w.render(&[]), "ˈpʰiːkʲ");

        let w = Word::new(normalise("ˈpʰiikʲ"), &[]).unwrap(); 
        assert_eq!(w.render(&[]), "ˈpʰiːkʲ");
    }

    #[test]
    fn test_render_aliases() {
        match Word::new(normalise("'GAN;CEUN!eB.gRǝ:S.φXOI?,HYZ"), &[]) {
            Ok(w) => assert_eq!(w.render(&[]), "ˈɢɐɴː.ɕɛʊɴǃeʙ.ɡʀəːʃ.ɸχɔɪʔˌʜʏʒ"),
            Err(e) => {
                println!("{}", e.format_word_error());
                assert!(false);
            }
        } 
    }

    #[test]
    fn test_americanist_aliases() {
        match Word::new(normalise("¢añ.φλełƛ"), &[]) {
            Ok(w) => assert_eq!(w.render(&[]), "¢añ.ɸλełƛ"),
            Err(e) => {
                println!("{}", e.format_word_error());
                assert!(false);
            }
        }
    }

    #[test]
    fn test_romanisation_simple() {
        let t = AliasParser::new(crate::alias::AliasKind::Romaniser, AliasLexer::new(crate::alias::AliasKind::Romaniser, &"ʃ, a:[+str], $ > sh, á, *".chars().collect::<Vec<_>>(), 0).get_line().unwrap(), 0).parse().unwrap();
        match Word::new(normalise("ʃa'ta"), &[]) {
            Ok(w) => assert_eq!(w.render(&t), "shatá"),
            Err(e) => {
                println!("{}", e.format_word_error());
                assert!(false);
            }
        }
    }

    #[test]
    fn test_romanisation_length() {
        let t = AliasParser::new(crate::alias::AliasKind::Romaniser, AliasLexer::new(crate::alias::AliasKind::Romaniser, &"ʃ:[+long], a:[+str, +long], t:[+long], $ > ssh, â, tt, *".chars().collect::<Vec<_>>(), 0).get_line().unwrap(), 0).parse().unwrap();
        match Word::new(normalise("ʃ:a't:a:"), &[]) {
            Ok(w) => assert_eq!(w.render(&t), "sshattâ"),
            Err(e) => {
                println!("{}", e.format_word_error());
                assert!(false);
            }
        }
    }

    #[test]
    fn test_romanisation_syllables() {
        let t = AliasParser::new(AliasKind::Romaniser, AliasLexer::new(AliasKind::Romaniser, &"ka, ta, na, $ > カ, タ, ナ, *".chars().collect::<Vec<_>>(), 0).get_line().unwrap(), 0).parse().unwrap();
        match Word::new(normalise("ka.ta.ka.na"), &[]) {
            Ok(w) => assert_eq!(w.render(&t), "カタカナ"),
            Err(e) => {
                println!("{}", e.format_word_error());
                assert!(false);
            }
        }
    }

    #[test]
    fn test_romanisation_syllables_with_tone() {
        let t = AliasParser::new(AliasKind::Romaniser, AliasLexer::new(AliasKind::Romaniser, &"ha:[tone: 55]n, ha:[tone: 51]n, y:[tone:214], $ > A, 汉, 语, *".chars().collect::<Vec<_>>(), 0).get_line().unwrap(), 0).parse().unwrap();
        match Word::new(normalise("han51.y214"), &[]) {
            Ok(w) => assert_eq!(w.render(&t), "汉语"),
            Err(e) => {
                println!("{}", e.format_word_error());
                assert!(false);
            }
        }
    }

    #[test]
    fn test_romanisation_segment_with_unicode_plus() {
        let t = AliasParser::new(AliasKind::Romaniser, AliasLexer::new(AliasKind::Romaniser, &"a:[+str], $ > +@{acute}, *".chars().collect::<Vec<_>>(), 0).get_line().unwrap(), 0).parse().unwrap();
        match Word::new(normalise("'ka.ta"), &[]) {
            Ok(w) => assert_eq!(w.render(&t), "káta"),
            Err(e) => {
                println!("{}", e.format_word_error());
                assert!(false);
            }
        }

        let t = AliasParser::new(AliasKind::Romaniser, AliasLexer::new(AliasKind::Romaniser, &"a:[+str], $ > @{acute}, *".chars().collect::<Vec<_>>(), 0).get_line().unwrap(), 0).parse().unwrap();
        match Word::new(normalise("'ka.ta"), &[]) {
            Ok(w) => assert_eq!(w.render(&t), "ḱta"),
            Err(e) => {
                println!("{}", e.format_word_error());
                assert!(false);
            }
        }
    }

    #[test]
    fn test_romanisation_group_with_unicode_plus() {
        let t = AliasParser::new(AliasKind::Romaniser, AliasLexer::new(AliasKind::Romaniser, &"V:[+str], $ > +@{acute}, *".chars().collect::<Vec<_>>(), 0).get_line().unwrap(), 0).parse().unwrap();
        match Word::new(normalise("'ka.ta"), &[]) {
            Ok(w) => assert_eq!(w.render(&t), "káta"),
            Err(e) => {
                println!("{}", e.format_word_error());
                assert!(false);
            }
        }

        let t = AliasParser::new(AliasKind::Romaniser, AliasLexer::new(AliasKind::Romaniser, &"V:[+str,+long], $ > +@{acute}, *".chars().collect::<Vec<_>>(), 0).get_line().unwrap(), 0).parse().unwrap();
        match Word::new(normalise("'ka:.ta"), &[]) {
            Ok(w) => assert_eq!(w.render(&t), "káta"),
            Err(e) => {
                println!("{}", e.format_word_error());
                assert!(false);
            }
        }
    }

    #[test]
    fn test_romanisation_remove_segment() {
        let t = AliasParser::new(AliasKind::Romaniser, AliasLexer::new(AliasKind::Romaniser, &"a > *".chars().collect::<Vec<_>>(), 0).get_line().unwrap(), 0).parse().unwrap();
        match Word::new(normalise("san.da"), &[]) {
            Ok(w) => assert_eq!(w.render(&t), "sn.d"),
            Err(e) => {
                println!("{}", e.format_word_error());
                assert!(false);
            }
        }
        // TODO: Should probably fix this
        match Word::new(normalise("sa:n.da"), &[]) {
            Ok(w) => assert_eq!(w.render(&t), "sːn.d"),
            Err(e) => {
                println!("{}", e.format_word_error());
                assert!(false);
            }
        }

        let t = AliasParser::new(AliasKind::Romaniser, AliasLexer::new(AliasKind::Romaniser, &"a:[+long] > *".chars().collect::<Vec<_>>(), 0).get_line().unwrap(), 0).parse().unwrap();
        match Word::new(normalise("sa:n.da"), &[]) {
            Ok(w) => assert_eq!(w.render(&t), "sn.da"),
            Err(e) => {
                println!("{}", e.format_word_error());
                assert!(false);
            }
        }
    }

    #[test]
    fn test_deromanisation_simple() {
        let t = AliasParser::new(AliasKind::Deromaniser, AliasLexer::new(AliasKind::Deromaniser, &"sh, á => ʃ, a:[+str]".chars().collect::<Vec<_>>(), 0).get_line().unwrap(), 0).parse().unwrap();
        match Word::new(normalise("sha.tá"), &t) {
            Ok(w) => assert_eq!(w.render(&[]), "ʃaˈta"),
            Err(e) => {
                println!("{}", e.format_word_error());
                assert!(false);
            }
        }
    }

    #[test]
    fn test_deromanisation_length() {
        let t = AliasParser::new(AliasKind::Deromaniser, AliasLexer::new(AliasKind::Deromaniser, &"ssh, â => ʃ:[+long], a:[+str, +long]".chars().collect::<Vec<_>>(), 0).get_line().unwrap(), 0).parse().unwrap();
        match Word::new(normalise("ssha.tâ"), &t) {
            Ok(w) => assert_eq!(w.render(&[]), "ʃːaˈtaː"),
            Err(e) => {
                println!("{}", e.format_word_error());
                assert!(false);
            }
        }

        let t = AliasParser::new(AliasKind::Deromaniser, AliasLexer::new(AliasKind::Deromaniser, &"+@{circum} => [+str, +long]".chars().collect::<Vec<_>>(), 0).get_line().unwrap(), 0).parse().unwrap();
        match Word::new(normalise("tâ"), &t) {
            Ok(w) => assert_eq!(w.render(&[]), "ˈtaː"),
            Err(e) => {
                println!("{}", e.format_word_error());
                assert!(false);
            }
        }
    }

    #[test]
    fn test_deromanisation_syllables() {
        let t = AliasParser::new(AliasKind::Deromaniser, AliasLexer::new(AliasKind::Deromaniser, &"カ, タ, ナ > ka, ta, na".chars().collect::<Vec<_>>(), 0).get_line().unwrap(), 0).parse().unwrap();
        match Word::new(normalise("カ.タ.カ.ナ"), &t) {
            Ok(w) => assert_eq!(w.render(&[]), "ka.ta.ka.na"),
            Err(e) => {
                println!("{}", e.format_word_error());
                assert!(false);
            }
        }
    }

    #[test]
    fn test_deromanisation_syllables_with_tone() {
        let t = AliasParser::new(AliasKind::Deromaniser, AliasLexer::new(AliasKind::Deromaniser, &"汉, 语 => ha:[tn: 51]n, y:[tone:214]".chars().collect::<Vec<_>>(), 0).get_line().unwrap(), 0).parse().unwrap();
        match Word::new(normalise("汉.语"), &t) {
            Ok(w) => assert_eq!(w.render(&[]), "han51.y214"),
            Err(e) => {
                println!("{}", e.format_word_error());
                assert!(false);
            }
        }
    }
}