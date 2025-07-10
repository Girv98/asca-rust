use crate::{alias, rule::Rule, word::SegPos, ASCAError};

use super::Word;

#[derive(Debug, Clone, PartialEq, Default)]
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


impl Phrase {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self(Vec::with_capacity(cap))
    }

    pub fn try_from(unparsed_phrase: &str, aliases: &[String]) -> Result<Self, ASCAError> {
        let alias_into = alias::parse_into(aliases)?;
        unparsed_phrase.split(' ').map(|w| Word::new(w, &alias_into)).collect()
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
    
    pub(crate) fn rev(&self) -> Self {
        let mut new = Self::with_capacity(self.len());
        // TODO: for w in self.iter().rev() {
        // Need to rev word_index in segpos.rev() for ^ to work
        for w in self.iter() {
            new.push(w.reverse());
        }

        new
    }

    pub(crate) fn seg_length_at(&self, sp: SegPos) -> usize {
        self[sp.word_index].syllables[sp.syll_index].get_seg_length_at(sp.seg_index)
    }

    pub(crate) fn in_bounds(&self, pos: SegPos) -> bool {
        pos.syll_index < self[pos.word_index].syllables.len() && pos.seg_index < self[pos.word_index].syllables[pos.syll_index].segments.len()
    }

    pub(crate) fn swap_sylls(&mut self, lw: usize, ls: usize, rw: usize, rs: usize) {
        if lw == rw {
            self[lw].swap_sylls(ls, rs);
            return;
        } 

        todo!("Swap between words")
    }

}
