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

impl From<ModKind> for Mods {
    fn from(value: ModKind) -> Self {
        match value {
            ModKind::Binary(bm) => Self::Binary(bm),
            ModKind::Alpha(am)  => Self::Alpha(am),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BinMod {
    Positive,
    Negative,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AlphaMod {
    Alpha(AlphaChar),
    InvAlpha(AlphaChar)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ModKind {
    Binary(BinMod),
    Alpha(AlphaMod),
}

impl ModKind {
    pub(crate) fn from_str(value: &str) -> Option<Self> {
        match AlphaChar::from_str(value) {
            Some(alpha) if value.starts_with('-') => Some(Self::Alpha(AlphaMod::InvAlpha(alpha))),
            Some(alpha)                           => Some(Self::Alpha(AlphaMod::Alpha(alpha))),

            None => match value {
                "+" => Some(Self::Binary(BinMod::Positive)),
                "-" => Some(Self::Binary(BinMod::Negative)),
                _ => None
            }
        }
    }
}

// Previously, we just stored the character,
// But that leads to Modifiers being quite large at 312 bytes.
// Using this enum reduces it to 80 bytes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum AlphaChar {
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
    Alpha, Beta, Gamma, Delta, Epsilon, Zeta, Eta, Theta, Iota, Kappa, Lambda, Mu, Nu, 
    Ksi, Omicron, Pi, Rho, Sigma, Stigma, SigmaTeliko, Tau, Upsilon, Phi, Chi, Psi, Omega
}

impl AlphaChar {
    pub(crate) fn from_str(s: &str) -> Option<Self> {
        match s {
            "A" | "-A" => Some(Self::A),  "α" | "-α" => Some(Self::Alpha),
            "B" | "-B" => Some(Self::B),  "β" | "-β" => Some(Self::Beta),
            "C" | "-C" => Some(Self::C),  "γ" | "-γ" => Some(Self::Gamma),
            "D" | "-D" => Some(Self::D),  "δ" | "-δ" => Some(Self::Delta),
            "E" | "-E" => Some(Self::E),  "ε" | "-ε" => Some(Self::Epsilon),
            "F" | "-F" => Some(Self::F),  "ζ" | "-ζ" => Some(Self::Zeta),
            "G" | "-G" => Some(Self::G),  "η" | "-η" => Some(Self::Eta),
            "H" | "-H" => Some(Self::H),  "θ" | "-θ" => Some(Self::Theta),
            "I" | "-I" => Some(Self::I),  "ι" | "-ι" => Some(Self::Iota),
            "J" | "-J" => Some(Self::J),  "κ" | "-κ" => Some(Self::Kappa),
            "K" | "-K" => Some(Self::K),  "λ" | "-λ" => Some(Self::Lambda),
            "L" | "-L" => Some(Self::L),  "μ" | "-μ" => Some(Self::Mu),
            "M" | "-M" => Some(Self::M),  "ν" | "-ν" => Some(Self::Nu),
            "N" | "-N" => Some(Self::N),  "ξ" | "-ξ" => Some(Self::Ksi),
            "O" | "-O" => Some(Self::O),  "ο" | "-ο" => Some(Self::Omicron),
            "P" | "-P" => Some(Self::P),  "π" | "-π" => Some(Self::Pi),
            "Q" | "-Q" => Some(Self::Q),  "ρ" | "-ρ" => Some(Self::Rho),
            "R" | "-R" => Some(Self::R),  "σ" | "-σ" => Some(Self::Sigma),  
            "S" | "-S" => Some(Self::S),  "ϛ" | "-ϛ" => Some(Self::Stigma),
            "T" | "-T" => Some(Self::T),  "ς" | "-ς" => Some(Self::SigmaTeliko),
            "U" | "-U" => Some(Self::U),  "τ" | "-τ" => Some(Self::Tau),
            "V" | "-V" => Some(Self::V),  "υ" | "-υ" => Some(Self::Upsilon),
            "W" | "-W" => Some(Self::W),  "φ" | "-φ" => Some(Self::Phi),
            "X" | "-X" => Some(Self::X),  "χ" | "-χ" => Some(Self::Chi),
            "Y" | "-Y" => Some(Self::Y),  "v" | "-v" => Some(Self::Psi),
            "Z" | "-Z" => Some(Self::Z),  "ω" | "-ω" => Some(Self::Omega),
            
            _ => None
        }
    }
    
    pub(crate) fn as_char(&self) -> char {
        match self {
            AlphaChar::A => 'A', AlphaChar::Alpha       => 'α',
            AlphaChar::B => 'B', AlphaChar::Beta        => 'β',
            AlphaChar::C => 'C', AlphaChar::Gamma       => 'γ',
            AlphaChar::D => 'D', AlphaChar::Delta       => 'δ',
            AlphaChar::E => 'E', AlphaChar::Epsilon     => 'ε',
            AlphaChar::F => 'F', AlphaChar::Zeta        => 'ζ',
            AlphaChar::G => 'G', AlphaChar::Eta         => 'η',
            AlphaChar::H => 'H', AlphaChar::Theta       => 'θ',
            AlphaChar::I => 'I', AlphaChar::Iota        => 'ι',
            AlphaChar::J => 'J', AlphaChar::Kappa       => 'κ',
            AlphaChar::K => 'K', AlphaChar::Lambda      => 'λ',
            AlphaChar::L => 'L', AlphaChar::Mu          => 'μ',
            AlphaChar::M => 'M', AlphaChar::Nu          => 'ν',
            AlphaChar::N => 'N', AlphaChar::Ksi         => 'ξ',
            AlphaChar::O => 'O', AlphaChar::Omicron     => 'ο',
            AlphaChar::P => 'P', AlphaChar::Pi          => 'π',
            AlphaChar::Q => 'Q', AlphaChar::Rho         => 'ρ',
            AlphaChar::R => 'R', AlphaChar::Sigma       => 'σ',
            AlphaChar::S => 'S', AlphaChar::Stigma      => 'ϛ',
            AlphaChar::T => 'T', AlphaChar::SigmaTeliko => 'ς',
            AlphaChar::U => 'U', AlphaChar::Tau         => 'τ',
            AlphaChar::V => 'V', AlphaChar::Upsilon     => 'υ',
            AlphaChar::W => 'W', AlphaChar::Phi         => 'φ',
            AlphaChar::X => 'X', AlphaChar::Chi         => 'χ',
            AlphaChar::Y => 'Y', AlphaChar::Psi         => 'v',
            AlphaChar::Z => 'Z', AlphaChar::Omega       => 'ω',
        }
    }
}

impl ModKind {
    pub(crate) fn as_bool(&self, alphas: &RefCell<HashMap<AlphaChar, Alpha>>, err_pos: Position) -> Result<bool, RuleRuntimeError> {
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

    #[allow(unused)]
    pub(crate) fn as_alpha_mod(&self) -> Option<&AlphaMod> {
        if let Self::Alpha(v) = self {
            Some(v)
        } else {
            None
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

    #[allow(unused)]
    pub(crate) fn from(stress: Option<SpecMod>, length: Option<SpecMod>, tone: Option<Tone>) -> Self {
        Self { stress, length, tone }
    }
}