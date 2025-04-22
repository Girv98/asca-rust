use std::fmt;

use crate::rule::{AlphaMod, BinMod, ModKind};
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
    pub(crate) nodes: [Option<ModKind>; NodeKind::Pharyngeal as usize + 1],
    pub(crate) feats: [Option<ModKind>; FeatKind::RetractedTongueRoot as usize + 1],
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
                    ModKind::Binary(b) => match b {
                        BinMod::Negative => nodes.push('-'),
                        BinMod::Positive => nodes.push('+'),
                    },
                    ModKind::Alpha(am) => match am {
                        AlphaMod::Alpha(a) => nodes.push(*a),
                        AlphaMod::InvAlpha(ia) => nodes.push_str(&ia.to_uppercase().to_string()),
                    },
                },
                None => nodes.push('0'),
            }
        }

        let mut feats = String::new();
        for val in self.feats.iter() {
            match val {
                Some(v) => match v {
                    ModKind::Binary(b) => match b {
                        BinMod::Negative => feats.push('-'),
                        BinMod::Positive => feats.push('+'),
                    },
                    ModKind::Alpha(am) => match am {
                        AlphaMod::Alpha(a) => feats.push(*a),
                        AlphaMod::InvAlpha(ia) => feats.push_str(&ia.to_uppercase().to_string()),
                    },
                },
                None => feats.push('0'),
            }
        }

        f.debug_struct("DiaMods").field("nodes", &nodes).field("feats", &feats).finish()
    }
}
