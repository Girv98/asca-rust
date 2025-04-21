use std::fmt;

use lexer::AliasLexer;
use parser::{AliasItem, AliasParser};

use crate::{error::AliasSyntaxError, rule::FeatureCategory};

pub mod lexer;
pub mod parser;

#[derive(Debug, Clone)]
pub(crate) struct Transformation {
    pub(crate) input: AliasItem,
    pub(crate) output: AliasItem
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AliasKind {
    Deromaniser,
    Romaniser
}

impl fmt::Display for AliasKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AliasKind::Deromaniser => write!(f, "deromaniser"),
            AliasKind::Romaniser   => write!(f, "romaniser"),
        }
    }
}


#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum AliasTokenKind {
    LeftSquare,       // [
    RightSquare,      // ]




    LessThan,         // <
    GreaterThan,      // >
    Equals,           // =
    Underline,        // _
    Arrow,            // -> or =>
    Comma,            // ,
    Colon,            // :
    WordBoundary,     // #
    SyllBoundary,     // $
    Syllable,         // %

    Group,            // Primitive Category i.e. C for Cons, V for Vowel
    Number,           // Number
    Plus,             // +


    Cardinal,         // IPA character
    Diacritic(u8),    // IPA Diacritic
    Star,             // *
    EmptySet,         // ∅

    Feature(FeatureCategory),
    String, 
    Eol,              // End of Line 
}
impl AliasTokenKind {
    pub(crate) fn as_diacritic(&self) -> Option<&u8> {
        if let Self::Diacritic(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct AliasPosition {
    pub(crate) kind: AliasKind,
    pub(crate) line: usize,
    pub(crate) start: usize,
    pub(crate) end: usize
}
impl AliasPosition {
    fn new(kind: AliasKind, line: usize, start: usize, end: usize) -> Self {
        Self { kind, line, start, end }
    }
}

impl fmt::Display for AliasPosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}-{}", self.kind, self.line, self.start, self.end)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AliasToken {
    pub(crate) kind: AliasTokenKind,
    pub(crate) value: String, 
    pub(crate) position: AliasPosition
}
impl AliasToken {
    fn new(kind: AliasTokenKind, value: String, position: AliasPosition) -> Self {
        Self { kind, value, position }
    }
}


#[repr(u16)]
pub(crate) enum NamedEscape {
    Space       =  32, // \u0020
    Grave       = 768, // \u0300
    Acute       = 769, // \u0301
    Circumflex  = 770, // \u0302
    Tilde       = 771, // \u0303
    Macron      = 772, // \u0304
    OverLine    = 773, // \u0305
    Breve       = 774, // \u0306
    OverDot     = 775, // \u0307
    Umlaut      = 776, // \u0308 
    OverHook    = 777, // \u0309
    OverRing    = 778, // \u030A
    DoubleAcute = 779, // \u030B
    Caron       = 780, // \u030C
    /* ... */
    DoubleGrave = 783, // \u030F
    /* ... */
    InvBreve    = 785, // \u0311
    /* ... */
    Horn        = 795, // \u031B
    /* ... */
    UnderDot    = 803, // \u0323
    UnderUmlaut = 804, // \u0324
    UnderRing   = 805, // \u0325
    UnderComma  = 806, // \u0326
    Cedilla     = 807, // \u0327
    Ogonek      = 808, // \u0328
}

impl NamedEscape {
    pub fn to_char(&self) -> char {
        // There *must* be a better way to do this lol
        unsafe { char::from_u32_unchecked(*(self as *const Self as *const u16) as u32) }
    }
}

impl TryFrom<&str> for NamedEscape {
    type Error = ();
    
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value.to_lowercase().as_str() {
            "space" | "spac" | "spc"    => Ok(Self::Space),
            // "ayin" | "ayn"             => Ok(Self::Ayin),
            "grave" | "grav" | "grv"    => Ok(Self::Grave),
            "acute" | "acut" | "acu"    | 
            "apex"  | "act"  | "apx"    => Ok(Self::Acute),
            "circumflex" | "circum"     | 
            "circflex"   | "cflex"      => Ok(Self::Circumflex),
            "tilde" | "tild" | "tlde"   |
            "tld"                       => Ok(Self::Tilde),
            "macron"   | "mcrn"         => Ok(Self::Macron),
            "overline" | "lineabove"    |
            "line"                      => Ok(Self::OverLine),
            "breve" | "brev" | "brv"    => Ok(Self::Breve),
            "overdot"  | "dotabove"     => Ok(Self::OverDot),
            "umlaut"   | "diaeresis"    |
            "dubdot"   | "doubledot"    => Ok(Self::Umlaut),
            "overhook" | "hookabove"    |
            "hook"                      => Ok(Self::OverHook),
            "overring" | "ringabove"    | 
            "ring"                      => Ok(Self::OverRing),
            "doubleacute" | "dubacute"  |
            "doubleapex"  | "dubapex"   => Ok(Self::DoubleAcute),
            "caron" | "karen" | "crn"   => Ok(Self::Caron),
            /* ... */
            "doublegrave" | "dubgrave"  |
            "dubgrav" | "dubgrv"        => Ok(Self::DoubleGrave),
            /* ... */
            "invertedbreve" | "invbrev" |
            "invbreve" | "invbrv"       => Ok(Self::InvBreve),
            /* ... */
            "horn" | "hrn" => Ok(Self::Horn),
            /* ... */
            "underdot" | "dotbelow"     => Ok(Self::UnderDot),
            "underumlaut" | "underdiaeresis" |
            "umlautbelow" | "diaeresisbelow" 
                                        => Ok(Self::UnderUmlaut),
            "underring"  | "ringbelow"  => Ok(Self::UnderRing),
            "undercomma" | "commabelow" => Ok(Self::UnderComma),
            "cedilla" | "cedi" | "ced"  => Ok(Self::Cedilla),
            "ogonek"  | "ogon" | 
            "ogonk"   | "ognk"          => Ok(Self::Ogonek),
            _ => Err(())
        }
    }
}


#[test]
fn test_escape_repr() {
    assert_eq!(NamedEscape::Space.to_char()      , ' ');
    // ʿayin 02BF
    assert_eq!(NamedEscape::Grave.to_char()      , '\u{0300}');
    assert_eq!(NamedEscape::Acute.to_char()      , '\u{0301}');
    assert_eq!(NamedEscape::Circumflex.to_char() , '\u{0302}');
    assert_eq!(NamedEscape::Tilde.to_char()      , '\u{0303}');
    assert_eq!(NamedEscape::Macron.to_char()     , '\u{0304}');
    assert_eq!(NamedEscape::OverLine.to_char()   , '\u{0305}');
    assert_eq!(NamedEscape::Breve.to_char()      , '\u{0306}');
    assert_eq!(NamedEscape::OverDot.to_char()    , '\u{0307}');
    assert_eq!(NamedEscape::Umlaut.to_char()     , '\u{0308}');
    assert_eq!(NamedEscape::OverHook.to_char()   , '\u{0309}');
    assert_eq!(NamedEscape::OverRing.to_char()   , '\u{030A}');
    assert_eq!(NamedEscape::DoubleAcute.to_char(), '\u{030B}');
    assert_eq!(NamedEscape::Caron.to_char()      , '\u{030C}');
    /* ... */
    assert_eq!(NamedEscape::DoubleGrave.to_char(), '\u{030F}');
    /* ... */
    assert_eq!(NamedEscape::InvBreve.to_char()   , '\u{0311}'); 
    /* ... */
    assert_eq!(NamedEscape::Horn.to_char()       , '\u{031B}'); 
    /* ... */
    assert_eq!(NamedEscape::UnderDot.to_char()   , '\u{0323}');
    assert_eq!(NamedEscape::UnderUmlaut.to_char(), '\u{0324}');
    assert_eq!(NamedEscape::UnderRing.to_char()  , '\u{0325}');
    assert_eq!(NamedEscape::UnderComma.to_char() , '\u{0326}');
    assert_eq!(NamedEscape::Cedilla.to_char()    , '\u{0327}');
    assert_eq!(NamedEscape::Ogonek.to_char()     , '\u{0328}');
}

pub(crate) fn parse_into(into: &[String]) -> Result<Vec<Transformation>, AliasSyntaxError> {
    let mut into_parsed = Vec::with_capacity(into.len());
    for (line, alias) in into.iter().enumerate() {
        into_parsed.extend(AliasParser::new(AliasKind::Deromaniser, AliasLexer::new(AliasKind::Deromaniser, &alias.chars().collect::<Vec<_>>(), line).get_line()?, line).parse()?);
    }

    Ok(into_parsed)
}

pub(crate) fn parse_from(from: &[String]) -> Result<Vec<Transformation>, AliasSyntaxError> {
    let mut from_parsed = Vec::with_capacity(from.len());
    for (line, alias) in from.iter().enumerate() {
        from_parsed.extend(AliasParser::new(AliasKind::Romaniser, AliasLexer::new(AliasKind::Romaniser, &alias.chars().collect::<Vec<_>>(), line).get_line()?, line).parse()?);
    }
    Ok(from_parsed)
}

#[allow(unused)]
pub(crate) fn parse_aliases(into: &[String], from: &[String]) -> Result<(Vec<Transformation>, Vec<Transformation>), AliasSyntaxError> {
    Ok((parse_into(into)?, parse_from(from)?))
}