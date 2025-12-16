use std::{cell::RefCell, collections::HashMap};

use crate::{error::RuleRuntimeError, word::{FeatKind, NodeKind, Tone}};
use super::{Alpha, Position};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Modifiers {
    pub(crate) nodes: [Option<ModKind>; NodeKind::count()],
    pub(crate) feats: [Option<ModKind>; FeatKind::count()],
    pub(crate) suprs: SupraSegs, 
}

impl Modifiers {
    pub(crate) fn new() -> Self {
        debug_assert_eq!(NodeKind::Pharyngeal as usize + 1, NodeKind::count());
        debug_assert_eq!(FeatKind::RetractedTongueRoot as usize + 1, FeatKind::count());

        Self { 
            nodes: [();NodeKind::count()].map(|_| None), 
            feats: [();FeatKind::count()].map(|_| None), 
            suprs: SupraSegs::new()
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Mods {
    Binary(BinMod),
    Number(Tone),
    Alpha(AlphaMod),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BinMod {
    Positive,
    Negative,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AlphaMod {
    Alpha(char),
    InvAlpha(char)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ModKind {
    Binary(BinMod),
    Alpha(AlphaMod),
}

impl ModKind {
    pub(crate) fn as_bool(&self, alphas: &RefCell<HashMap<char, Alpha>>, err_pos: Position) -> Result<bool, RuleRuntimeError> {
        match self {
            ModKind::Binary(bin_mod) => Ok(*bin_mod == BinMod::Positive),
            ModKind::Alpha(alpha_mod) => match alpha_mod {
                AlphaMod::Alpha(ch) => {
                    if let Some(alpha) = alphas.borrow().get(ch) {
                        Ok(alpha.as_binary())
                    } else {
                        Err(RuleRuntimeError::AlphaUnknown(err_pos))
                    }
                },
                AlphaMod::InvAlpha(ch) => {
                    if let Some(alpha) = alphas.borrow().get(ch) {
                        Ok(!alpha.as_binary())
                    } else {
                        Err(RuleRuntimeError::AlphaUnknown(err_pos))
                    }
                },
            },
        }
    }

    pub(crate) fn as_bin_mod(&self) -> Option<&BinMod> {
        if let Self::Binary(v) = self {
            Some(v)
        } else {
            None
        }
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct SupraSegs {
    pub(crate) stress: Option<SpecMod>,
    pub(crate) length: Option<SpecMod>,
    pub(crate) tone: Option<Tone>,
}

/// For modifiers that have a primary and a secondary component;
/// namely Stress `(Stress, SecStress)` and Length `(Long, Overlong)`.
/// 
/// This allows us to assign an Alpha to the entire modifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SpecMod {
    First(ModKind),
    Second(ModKind),
    Both(ModKind, ModKind), // (Primary Component, Secondary Component)
    // Used for alpha'ing both simultaneously
    // This could be just AlphaMod as it cannot be Binary
    Joined(ModKind),
}

impl SupraSegs {
    pub(crate) fn new() -> Self {
        Self { stress: None, length: None, tone: None }
    }

    pub(crate) fn from(stress: Option<SpecMod>, length: Option<SpecMod>, tone: Option<Tone>) -> Self {
        Self { stress, length, tone }
    }
}