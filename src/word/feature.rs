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

    pub(crate) fn as_str(&self) -> &'static str {
        match self {
            NodeKind::Root       => "root",
            NodeKind::Manner     => "manner",
            NodeKind::Laryngeal  => "laryngeal",
            NodeKind::Place      => "place",
            NodeKind::Labial     => "labial",
            NodeKind::Coronal    => "coronal",
            NodeKind::Dorsal     => "dorsal",
            NodeKind::Pharyngeal => "pharyngeal",
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
    /*MAN*/ Continuant, Approximant, Lateral, Nasal, DelayedRelease, Strident, Rhotic, Click, Double,          
    /*LAR*/ Voice, SpreadGlottis, ConstrGlottis,   
    // PLACE Node
    /*LAB*/ Labiodental, Round,          
    /*COR*/ Anterior, Distributed,     
    /*DOR*/ Front, Back, High, Low, Tense, Reduced,        
    /*PHR*/ AdvancedTongueRoot, RetractedTongueRoot, 
}

impl fmt::Display for FeatKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl FeatKind {
    pub(crate) const fn count() -> usize { 27 }

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
            11 => Self::Double,
            // LAR node
            12 => Self::Voice,
            13 => Self::SpreadGlottis,
            14 => Self::ConstrGlottis,
            // PLACE Node
            // LABIAL subnode
            15 => Self::Labiodental,
            16 => Self::Round,
            // CORONAL subnode
            17 => Self::Anterior,
            18 => Self::Distributed,
            // DORSAL subnode
            19 => Self::Front,
            20 => Self::Back,
            21 => Self::High,
            22 => Self::Low,
            23 => Self::Tense,
            24 => Self::Reduced,
            // PHAR subnode
            25 => Self::AdvancedTongueRoot,
            26 => Self::RetractedTongueRoot,
            _  => unreachable!("\nOut of Range Error converting `usize` to `FeatType`\nThis is a bug!\n")
        }

    }
    
    pub(crate) fn as_str(&self) -> &'static str {
        match self {
            FeatKind::Consonantal         => "consonantal",
            FeatKind::Sonorant            => "sonorant",
            FeatKind::Syllabic            => "syllabic",
            FeatKind::Continuant          => "continuant",
            FeatKind::Approximant         => "approx.",
            FeatKind::Lateral             => "lateral",
            FeatKind::Nasal               => "nasal",
            FeatKind::DelayedRelease      => "del.rel.",
            FeatKind::Strident            => "strident",
            FeatKind::Rhotic              => "rhotic",
            FeatKind::Click               => "click",
            FeatKind::Double              => "double",
            FeatKind::Voice               => "voice",
            FeatKind::SpreadGlottis       => "s.g.",
            FeatKind::ConstrGlottis       => "c.g.",
            FeatKind::Labiodental         => "labiodental",
            FeatKind::Round               => "round",
            FeatKind::Anterior            => "anterior",
            FeatKind::Distributed         => "distributed",
            FeatKind::Front               => "front",
            FeatKind::Back                => "back",
            FeatKind::High                => "high",
            FeatKind::Low                 => "low",
            FeatKind::Tense               => "tense",
            FeatKind::Reduced             => "reduced",
            FeatKind::AdvancedTongueRoot  => "atr",
            FeatKind::RetractedTongueRoot => "rtr",
        }
    }

    /// Returns the node and bitmask of the feature
    pub const fn as_node_mask(&self) -> (NodeKind, u16) {
        match self {
            Self::Consonantal         => (NodeKind::Root, 0b100),
            Self::Sonorant            => (NodeKind::Root, 0b010),
            Self::Syllabic            => (NodeKind::Root, 0b001),
            
            Self::Continuant          => (NodeKind::Manner, 0b1000_0000_00),
            Self::Approximant         => (NodeKind::Manner, 0b0100_0000_00),
            Self::Lateral             => (NodeKind::Manner, 0b0010_0000_00),
            Self::Nasal               => (NodeKind::Manner, 0b0001_0000_00),
            Self::DelayedRelease      => (NodeKind::Manner, 0b0000_1000_00),
            Self::Strident            => (NodeKind::Manner, 0b0000_0100_00),
            Self::Rhotic              => (NodeKind::Manner, 0b0000_0010_00),
            Self::Click               => (NodeKind::Manner, 0b0000_0001_00),
            Self::Double              => (NodeKind::Manner, 0b0000_0000_10),
            // UNUSED => (NodeKind::Manner, 0b0000_0000_01),
            
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
    LengthPair, // Can only be used with alpha notation (e.g. Alength)
    StressPair, // Can only be used with alpha notation (e.g. Astress)
}

impl fmt::Display for SupraKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SupraKind::Long       => write!(f, "long"),
            SupraKind::Overlong   => write!(f, "overlng"),
            SupraKind::Stress     => write!(f, "str"),
            SupraKind::SecStress  => write!(f, "secstr"),
            SupraKind::Tone       => write!(f, "tone"),
            SupraKind::LengthPair => write!(f, "lenpair"),
            SupraKind::StressPair => write!(f, "strpair"),
        }
    }
}