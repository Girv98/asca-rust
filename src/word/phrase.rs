use std::fmt;

use crate::{alias::{self, Transformation}, rule::Rule, word::{SegPos, Segment}, ASCAError};

use super::Word;

#[derive(Clone, PartialEq, Default)]
pub struct Phrase (Vec<Word>);

impl std::ops::Deref for Phrase {
    type Target = Vec<Word>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl std::ops::DerefMut for Phrase {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl FromIterator<Word> for Phrase {
    fn from_iter<T: IntoIterator<Item = Word>>(iter: T) -> Self {
        Phrase(iter.into_iter().collect())
    }
}

impl fmt::Debug for Phrase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, word) in self.iter().enumerate() {
            writeln!(f, "Word {}", i+1)?;
            write!(f, "{word:?}")?;
        }

        Ok(())
    }
}


impl Phrase {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self(Vec::with_capacity(cap))
    }

    pub fn try_from_vec<S: AsRef<str>>(unparsed_phrases: &[S], aliases: &[S]) -> Result<Vec<Self>, ASCAError> {
        let alias_into = alias::parse_into(aliases)?;

        unparsed_phrases.iter().map(|up| 
            up.as_ref().trim_end().split(' ').map(|w| Word::with(w, &alias_into)).collect()
        ).collect()
    }

    pub fn try_from(unparsed_phrase: &str, aliases: &[String]) -> Result<Self, ASCAError> {
        let alias_into = alias::parse_into(aliases)?;
        unparsed_phrase.trim_end().split(' ').map(|w| Word::with(w, &alias_into)).collect()
    }

    pub fn render_debug(&self, aliases: &[Transformation]) -> Result<(String, Vec<Segment>), ASCAError> {
        let mut buffer = String::new();
        let mut unknowns = Vec::new();
        for word in self.iter() {
            let (w, u) = word.render_debug(aliases)?;
            buffer.push(' ');
            buffer.push_str(&w);
            unknowns.extend(u);
        }

        // TODO / NOTE: the trim() here means that phrases with leading
        // spaces will be stripped of such padding in the output.
        // This is currently advantageous for word boundary deletion rules,
        // however this means that the user's formatting is not respected.
        Ok((buffer.trim().to_string(), unknowns))
    }

    pub fn render(&self) -> String {
        let mut buffer = String::new();
        for word in self.iter() {
            let w = word.render();
            buffer.push(' ');
            buffer.push_str(&w);
        }

        // TODO / NOTE: the trim() here means that phrases with leading
        // spaces will be stripped of such padding in the output.
        // This is currently advantageous for word boundary deletion rules,
        // however this means that the user's formatting is not respected.
        buffer.trim().to_string()
    }

    pub fn render_with(&self, aliases: &[Transformation]) -> Result<String, ASCAError> {
        let mut buffer = String::new();
        for word in self.iter() {
            let w = word.render_with(aliases)?;
            buffer.push(' ');
            buffer.push_str(&w);
        }

        // TODO / NOTE: the trim() here means that phrases with leading
        // spaces will be stripped of such padding in the output.
        // This is currently advantageous for word boundary deletion rules,
        // however this means that the user's formatting is not respected.
        Ok(buffer.trim().to_string())
    }

    #[inline]
    pub(crate) fn apply_all(&self, rules: &[Vec<Rule>]) -> Result<Self, ASCAError> {
        let mut res = self.clone();

        for rule_group in rules {
            for rule in rule_group {
                res = rule.apply(res)?;
            }
        }

        Ok(res)
    }
    
    pub(crate) fn reversed(&self) -> Self {
        let mut new = Self::with_capacity(self.len());
        for w in self.iter().rev() {
            new.push(w.reverse());
        }

        new
    }

    pub(crate) fn get_seg_at(&self, sp: SegPos) -> Option<Segment> {
        self[sp.word_index].get_seg_at(sp)
    }

    pub(crate) fn seg_length_at(&self, sp: SegPos) -> usize {
        self[sp.word_index].syllables[sp.syll_index].get_seg_length_at(sp.seg_index)
    }

    pub(crate) fn in_bounds(&self, pos: SegPos) -> bool {
        pos.syll_index < self[pos.word_index].syllables.len() && pos.seg_index < self[pos.word_index].syllables[pos.syll_index].segments.len()
    }

    pub(crate) fn swap_sylls(&mut self, lw: usize, ls: usize, rw: usize, rs: usize) {
        let lft_temp = self[lw].syllables[ls].clone();
        self[lw].syllables[ls] = self[rw].syllables[rs].clone();
        self[rw].syllables[rs] = lft_temp;
    }
}
