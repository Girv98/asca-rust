use std::fmt;

use crate::rule::BinMod;
use super::{FeatKind, NodeKind};

#[derive(Debug, Clone)]
pub(crate) struct Diacritic {
    #[allow(unused)]
    pub(crate) name: String,
    pub(crate) diacrit: char,
    pub(crate) prereqs: DiaMods,
    pub(crate) payload: DiaMods,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) struct DiaMods {
    pub(crate) nodes: [Option<BinMod>; NodeKind::Pharyngeal as usize + 1],
    pub(crate) feats: [Option<BinMod>; FeatKind::RetractedTongueRoot as usize + 1],
}

impl DiaMods {
    pub(crate) fn new() -> Self {
        Self { 
            nodes: [();NodeKind::Pharyngeal as usize + 1].map(|_| None), 
            feats: [();FeatKind::RetractedTongueRoot as usize + 1].map(|_| None), 
        }
    }
}

impl fmt::Debug for DiaMods {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {

        let mut nodes = String::new();
        for val in self.nodes.iter() {
            match val {
                Some(v) => match v {
                    BinMod::Negative => nodes.push('-'),
                    BinMod::Positive => nodes.push('+'),
                },
                None => nodes.push('0'),
            }
        }

        let mut feats = String::new();
        for val in self.feats.iter() {
            match val {
                Some(v) => match v {
                    BinMod::Negative => feats.push('-'),
                    BinMod::Positive => feats.push('+'),
                },
                None => feats.push('0'),
            }
        }

        f.debug_struct("DiaMods").field("nodes", &nodes).field("feats", &feats).finish()
    }
}
