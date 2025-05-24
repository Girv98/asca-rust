use crate::{alias, ASCAError};

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
}
