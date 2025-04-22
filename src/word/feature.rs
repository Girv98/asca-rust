use std::fmt;

use serde::{Deserialize, Serialize};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Deserialize, Hash)]
pub enum FeatureCategory {
    Node(NodeKind),
    Feat(FeatKind),
    Supr(SupraKind),
}

impl fmt::Display for FeatureCategory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FeatureCategory::Node(x) => write!(f, "{x}"),
            FeatureCategory::Feat(x) => write!(f, "{x}"),
            FeatureCategory::Supr(x) => write!(f, "{x}"),
        }
    }
}

/// Represents a distinctive feature node
#[derive(Debug, Copy, Clone, PartialEq, Eq, Deserialize, Serialize, Hash)]
pub enum NodeKind {
    Root,
    Manner,
    Laryngeal,
    Place,
    Labial,
    Coronal,
    Dorsal,
    Pharyngeal
}

impl NodeKind {
    pub(crate) const fn count() -> usize { 8 }

    pub(crate) fn from_usize(value: usize) -> Self {
        debug_assert!(NodeKind::count() == 8);
        use NodeKind::*;
        match value {
            0 => {debug_assert_eq!(value, Root as usize); Root}
            1 => {debug_assert_eq!(value, Manner as usize); Manner}
            2 => {debug_assert_eq!(value, Laryngeal as usize); Laryngeal}
            3 => {debug_assert_eq!(value, Place as usize); Place}
            4 => {debug_assert_eq!(value, Labial as usize); Labial}
            5 => {debug_assert_eq!(value, Coronal as usize); Coronal}
            6 => {debug_assert_eq!(value, Dorsal as usize); Dorsal}
            7 => {debug_assert_eq!(value, Pharyngeal as usize); Pharyngeal},
            _ => unreachable!("\nOut of Range converting `{value}` to `NodeType`(max: 7) \nThis is a bug!\n")
        }
    }
}

impl fmt::Display for NodeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Root         => write!(f, "RUT"),
            Self::Manner       => write!(f, "MAN"),
            Self::Laryngeal    => write!(f, "LAR"),
            Self::Place        => write!(f, "PLC"),
            Self::Labial       => write!(f, "LAB"),
            Self::Coronal      => write!(f, "COR"),
            Self::Dorsal       => write!(f, "DOR"),
            Self::Pharyngeal   => write!(f, "PHR")
        }
    }
}

/// Represents a distinctive feature
#[derive(Debug, Copy, Clone, PartialEq, Eq, Deserialize, Hash)]
pub enum FeatKind { 
    /*RUT*/ Consonantal, Sonorant, Syllabic,      
    /*MAN*/ Continuant, Approximant, Lateral, Nasal, DelayedRelease, Strident, Rhotic, Click,          
    /*LAR*/ Voice, SpreadGlottis, ConstrGlottis,   
    // PLACE Node
    /*LAB*/ Labiodental, Round,          
    /*COR*/ Anterior, Distributed,     
    /*DOR*/ Front, Back, High, Low, Tense, Reduced,        
    /*PHR*/ AdvancedTongueRoot, RetractedTongueRoot, 
}

impl fmt::Display for FeatKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FeatKind::Consonantal         => write!(f, "consonantal"),
            FeatKind::Sonorant            => write!(f, "sonorant"),
            FeatKind::Syllabic            => write!(f, "syllablic"),
            FeatKind::Continuant          => write!(f, "continuant"),
            FeatKind::Approximant         => write!(f, "approx."),
            FeatKind::Lateral             => write!(f, "lateral"),
            FeatKind::Nasal               => write!(f, "nasas"),
            FeatKind::DelayedRelease      => write!(f, "del.rel."),
            FeatKind::Strident            => write!(f, "strident"),
            FeatKind::Rhotic              => write!(f, "rhotic"),
            FeatKind::Click               => write!(f, "click"),
            FeatKind::Voice               => write!(f, "voice"),
            FeatKind::SpreadGlottis       => write!(f, "s.g."),
            FeatKind::ConstrGlottis       => write!(f, "c.g."),
            FeatKind::Labiodental         => write!(f, "labiodental"),
            FeatKind::Round               => write!(f, "round"),
            FeatKind::Anterior            => write!(f, "anterior"),
            FeatKind::Distributed         => write!(f, "distributed"),
            FeatKind::Front               => write!(f, "front"),
            FeatKind::Back                => write!(f, "back"),
            FeatKind::High                => write!(f, "high"),
            FeatKind::Low                 => write!(f, "low"),
            FeatKind::Tense               => write!(f, "tense"),
            FeatKind::Reduced             => write!(f, "reduced"),
            FeatKind::AdvancedTongueRoot  => write!(f, "atr"),
            FeatKind::RetractedTongueRoot => write!(f, "rtr")
        }
    }
}

impl FeatKind {
    pub(crate) const fn count() -> usize { 26 }

    pub(crate) fn from_usize(value: usize) -> Self {
        match value {
            // ROOT node
             0 => Self::Consonantal,
             1 => Self::Sonorant,
             2 => Self::Syllabic,
            // MANNER node
             3 => Self::Continuant,
             4 => Self::Approximant,
             5 => Self::Lateral,
             6 => Self::Nasal,
             7 => Self::DelayedRelease,
             8 => Self::Strident,
             9 => Self::Rhotic,
            10 => Self::Click,
            // LAR node
            11 => Self::Voice,
            12 => Self::SpreadGlottis,
            13 => Self::ConstrGlottis,
            // PLACE Node
            // LABIAL subnode
            14 => Self::Labiodental,
            15 => Self::Round,
            // CORONAL subnode
            16 => Self::Anterior,
            17 => Self::Distributed,
            // DORSAL subnode
            18 => Self::Front,
            19 => Self::Back,
            20 => Self::High,
            21 => Self::Low,
            22 => Self::Tense,
            23 => Self::Reduced,
            // PHAR subnode
            24 => Self::AdvancedTongueRoot,
            25 => Self::RetractedTongueRoot,
            _  => unreachable!("\nOut of Range Error converting `usize` to `FeatType`\nThis is a bug!\n")
        }
    }

    /// Returns the node and bitmask of the feature
    pub const fn as_node_mask(&self) -> (NodeKind, u8) {
        match self {
            Self::Consonantal         => (NodeKind::Root, 0b100),
            Self::Sonorant            => (NodeKind::Root, 0b010),
            Self::Syllabic            => (NodeKind::Root, 0b001),
            
            Self::Continuant          => (NodeKind::Manner, 0b10000000),
            Self::Approximant         => (NodeKind::Manner, 0b01000000),
            Self::Lateral             => (NodeKind::Manner, 0b00100000),
            Self::Nasal               => (NodeKind::Manner, 0b00010000),
            Self::DelayedRelease      => (NodeKind::Manner, 0b00001000),
            Self::Strident            => (NodeKind::Manner, 0b00000100),
            Self::Rhotic              => (NodeKind::Manner, 0b00000010),
            Self::Click               => (NodeKind::Manner, 0b00000001),
            
            Self::Voice               => (NodeKind::Laryngeal, 0b100),
            Self::SpreadGlottis       => (NodeKind::Laryngeal, 0b010),
            Self::ConstrGlottis       => (NodeKind::Laryngeal, 0b001),
            
            Self::Labiodental         => (NodeKind::Labial, 0b10),
            Self::Round               => (NodeKind::Labial, 0b01),

            Self::Anterior            => (NodeKind::Coronal, 0b10),
            Self::Distributed         => (NodeKind::Coronal, 0b01),

            Self::Front               => (NodeKind::Dorsal, 0b100000),
            Self::Back                => (NodeKind::Dorsal, 0b010000),
            Self::High                => (NodeKind::Dorsal, 0b001000),
            Self::Low                 => (NodeKind::Dorsal, 0b000100),
            Self::Tense               => (NodeKind::Dorsal, 0b000010),
            Self::Reduced             => (NodeKind::Dorsal, 0b000001),

            Self::AdvancedTongueRoot  => (NodeKind::Pharyngeal, 0b10),
            Self::RetractedTongueRoot => (NodeKind::Pharyngeal, 0b01),
        }
    }
}

/// Represents a suprasegmental feature
#[derive(Debug, Copy, Clone, PartialEq, Eq, Deserialize, Hash)]
pub enum SupraKind {
    Long,       // ±long
    Overlong,   // ±overlong
    Stress,     // ±stress    (+ matches prim and sec, - matches unstressed)
    SecStress,  // ±secstress (+ matches sec, - matches prim and unstressed)
    Tone,       // Can only be used with : notation (e.g. Tone : 213 )
}

impl fmt::Display for SupraKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SupraKind::Long            => write!(f, "long"),
            SupraKind::Overlong        => write!(f, "overlng"),
            SupraKind::Stress          => write!(f, "str"),
            SupraKind::SecStress       => write!(f, "secstr"),
            SupraKind::Tone            => write!(f, "tone"),
        }
    }
}