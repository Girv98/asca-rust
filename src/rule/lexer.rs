use std::{ fmt::{self, Display}, rc::Rc };
use serde::Deserialize;

use crate :: {
    error :: RuleSyntaxError, 
    word  :: NodeKind, 
    CARDINALS_TRIE, 
    DIACRITS
};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Deserialize, Hash)]
pub(crate) enum NodeType {
    Root,      
    Manner,
    Laryngeal,   
    Place,      
    Labial,      
    Coronal,     
    Dorsal,      
    Pharyngeal, 
}

impl NodeType {
    pub(crate) const fn count() -> usize { 8 }

    pub(crate) fn from_usize(value: usize) -> Self {
        use NodeType::*;
        match value {
            0 => {debug_assert_eq!(value, Root as usize); Root},
            1 => {debug_assert_eq!(value, Manner as usize); Manner},
            2 => {debug_assert_eq!(value, Laryngeal as usize); Laryngeal},
            3 => {debug_assert_eq!(value, Place as usize); Place},
            4 => {debug_assert_eq!(value, Labial as usize); Labial},
            5 => {debug_assert_eq!(value, Coronal as usize); Coronal},
            6 => {debug_assert_eq!(value, Dorsal as usize); Dorsal},
            7 => {debug_assert_eq!(value, Pharyngeal as usize); Pharyngeal},
            _ => unreachable!()
        }
    }
}

impl Display for NodeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NodeType::Root         => write!(f, "RUT"),
            NodeType::Manner       => write!(f, "MAN"),
            NodeType::Laryngeal    => write!(f, "LAR"),
            NodeType::Place        => write!(f, "PLC"),
            NodeType::Labial       => write!(f, "LAB"),
            NodeType::Coronal      => write!(f, "COR"),
            NodeType::Dorsal       => write!(f, "DOR"),
            NodeType::Pharyngeal   => write!(f, "PHR")
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Deserialize, Hash)]
pub(crate) enum SupraType {
    Long,       // ±long
    Overlong,   // ±overlong
    Stress,     // ±stress    (+ matches prim and sec, - matches unstressed)
    SecStress,  // ±secstress (+ matches sec, - matches prim and unstressed)
    Tone,       // Can only be used with : notation (e.g. Tone : 213 )
}

impl Display for SupraType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SupraType::Long            => write!(f, "long"),
            SupraType::Overlong        => write!(f, "overlng"),
            SupraType::Stress          => write!(f, "str"),
            SupraType::SecStress       => write!(f, "secstr"),
            SupraType::Tone            => write!(f, "tone"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Deserialize, Hash)]
pub(crate) enum FType { 
    // can be ± || α.ω 
    /*RUT*/ Consonantal, Sonorant, Syllabic,      
    /*MAN*/ Continuant, Approximant, Lateral, Nasal, DelayedRelease, Strident, Rhotic, Click,          
    /*LAR*/ Voice, SpreadGlottis, ConstrGlottis,   
    // PLACE Node
    /*LAB*/ Labiodental, Round,          
    /*COR*/ Anterior, Distributed,     
    /*DOR*/ Front, Back, High, Low, Tense, Reduced,        
    /*PHR*/ AdvancedTongueRoot, RetractedTongueRoot, 
}

impl Display for FType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FType::Consonantal         => write!(f, "consonantal"),
            FType::Sonorant            => write!(f, "sonorant"),
            FType::Syllabic            => write!(f, "syllablic"),
            FType::Continuant          => write!(f, "continuant"),
            FType::Approximant         => write!(f, "approx."),
            FType::Lateral             => write!(f, "lateral"),
            FType::Nasal               => write!(f, "nasas"),
            FType::DelayedRelease      => write!(f, "del.rel."),
            FType::Strident            => write!(f, "strident"),
            FType::Rhotic              => write!(f, "rhotic"),
            FType::Click               => write!(f, "click"),
            FType::Voice               => write!(f, "voice"),
            FType::SpreadGlottis       => write!(f, "s.g."),
            FType::ConstrGlottis       => write!(f, "c.g."),
            FType::Labiodental         => write!(f, "labiodental"),
            FType::Round               => write!(f, "round"),
            FType::Anterior            => write!(f, "anterior"),
            FType::Distributed         => write!(f, "distributed"),
            FType::Front               => write!(f, "front"),
            FType::Back                => write!(f, "back"),
            FType::High                => write!(f, "high"),
            FType::Low                 => write!(f, "low"),
            FType::Tense               => write!(f, "tense"),
            FType::Reduced             => write!(f, "reduced"),
            FType::AdvancedTongueRoot  => write!(f, "atr"),
            FType::RetractedTongueRoot => write!(f, "rtr")
        }
    }
}

impl FType {
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

    pub(crate) const fn as_node_mask(&self) -> (NodeKind, u8) {
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Deserialize, Hash)]
pub(crate) enum FeatType {
    Node(NodeType),
    Feat(FType),
    Supr(SupraType),
}

impl Display for FeatType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FeatType::Node(x) => write!(f, "{x}"),
            FeatType::Feat(x) => write!(f, "{x}"),
            FeatType::Supr(x) => write!(f, "{x}"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum TokenKind {
    LeftSquare,       // [
    RightSquare,      // ]
    LeftCurly,        // {
    RightCurly,       // }
    RightAngle,       // ⟩
    LeftAngle,        // ⟨
    LeftBracket,      // (
    RightBracket,     // )
    LeftColCurly,     // :{
    RightColCurly,    // }:
    // LessThan,         // <
    GreaterThan,      // >
    Equals,           // =
    Underline,        // _
    Arrow,            // -> or =>
    Comma,            // ,
    Colon,            // :
    WordBoundary,     // #
    SyllBoundary,     // $
    Syllable,         // %
    Ampersand,        // &
    Group,            // Primitive Category i.e. C for Cons, V for Vowel
    Number,           // Number
    Slash,            // /
    DubSlash,         // //
    Pipe,             // | 
    Cardinal,         // IPA character
    Diacritic(u8),    // IPA Diacritic
    Star,             // *
    EmptySet,         // ∅
    Ellipsis,         // ... or .. or … or ⋯
    Comment,          // Delimited by ';;'
    Feature(FeatType),
    Eol,              // End of Line 
}

impl TokenKind {
    pub(crate) fn as_diacritic(&self) -> Option<&u8> {
        if let Self::Diacritic(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        
        match self {
            TokenKind::LeftSquare    => write!(f, "LSquare"),
            TokenKind::RightSquare   => write!(f, "RSquare"),
            TokenKind::LeftCurly     => write!(f, "LCurly"),
            TokenKind::RightCurly    => write!(f, "RCurly"),
            TokenKind::LeftAngle     => write!(f, "LAngle"),
            TokenKind::RightAngle    => write!(f, "RAngle"),
            TokenKind::LeftBracket   => write!(f, "LBrack"),
            TokenKind::RightBracket  => write!(f, "RBrack"),
            TokenKind::LeftColCurly  => write!(f, "LCCurly"),
            TokenKind::RightColCurly => write!(f, "RCCurly"),
            // TokenKind::LessThan      => write!(f, "LT"),
            TokenKind::GreaterThan   => write!(f, "GT"),
            TokenKind::Equals        => write!(f, "Eq"),
            TokenKind::Underline     => write!(f, "UL"),
            TokenKind::Arrow         => write!(f, "Arrow"),
            TokenKind::Comma         => write!(f, "Comma"),
            TokenKind::Colon         => write!(f, "Colon"),
            TokenKind::WordBoundary  => write!(f, "WBound"),
            TokenKind::SyllBoundary  => write!(f, "SBound"),
            TokenKind::Syllable      => write!(f, "Syll"),
            TokenKind::Ampersand     => write!(f, "Amper"),
            TokenKind::Group         => write!(f, "Prim"),
            TokenKind::Number        => write!(f, "Num"),
            TokenKind::Slash         => write!(f, "Slash"),
            TokenKind::DubSlash      => write!(f, "DoubleSlash"),
            TokenKind::Pipe          => write!(f, "Pipe"),
            TokenKind::Cardinal      => write!(f, "Cardinal"),
            TokenKind::Diacritic(_)  => write!(f, "Diacritic"),
            TokenKind::Star          => write!(f, "Star"),
            TokenKind::EmptySet      => write!(f, "Empty"),
            TokenKind::Ellipsis      => write!(f, "Ellipsis"),
            TokenKind::Comment       => write!(f, "Comment"),
            TokenKind::Feature(x)    => write!(f, "{x}"),
            TokenKind::Eol           => write!(f, "End of Line"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Position {
    pub(crate) group: usize,
    pub(crate) line: usize,
    pub(crate) start: usize,
    pub(crate) end: usize,
}

impl Position {
    pub(crate) fn new(group: usize, line: usize, start: usize, end: usize) -> Self {
        Self {group, line, start, end }
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({},{})", self.start, self.end)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Token {
    pub(crate) kind: TokenKind,
    pub(crate) value: Rc<str>, 
    pub(crate) position: Position,
}

impl Token {
    pub(crate) fn new(kind: TokenKind, value: &str, group: usize, line: usize, start: usize, end: usize) -> Self {
        Self { kind, value: Rc::from(value), position: Position::new(group, line, start, end) }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut spaces = " ".to_string();
        if self.position.end   <= 9 {spaces += " "}
        if self.position.start <= 9 {spaces += " "}
        match self.kind {
            TokenKind::Feature(x) => write!(f, "{}{}`{}{}`",  self.position, spaces, self.value, x),
            _ => write!(f, "{}{}{} `{}`",  self.position, spaces, self.kind, self.value)
        }
    }
}

#[derive(Default)]
pub(crate) struct Lexer<'a> {
    source: &'a [char],
    group: usize,
    line: usize,
    pos: usize,
    inside_matrix: bool,
    inside_option: bool,
    inside_syll: bool,
    inside_set: bool,
    inside_env_set: bool
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(source: &'a [char], group: usize, line: usize) -> Self {
        Self { source, group, line, pos: 0, inside_matrix: false , inside_option: false, inside_syll: false, inside_set: false, inside_env_set: false}
    }

    fn has_more_chars(&self) -> bool { !self.source.is_empty() }

    fn trim_whitespace(&mut self) {
        while self.has_more_chars() && self.source[0].is_whitespace() {
            self.advance();
        }
    }

    fn chop(&mut self, n: usize) -> String {
        let token = &self.source[0..n];
        self.source = &self.source[n..];
        self.pos += n;
        token.iter().collect()
    }

    fn chop_while<P>(&mut self, mut predicate: P) -> String where P: FnMut(&char) -> bool {
        let mut n = 0;
        while n < self.source.len() && predicate(&self.source[n]) {
            n += 1;
        }
        self.chop(n)
    }

    fn curr_char(&self) -> char {
        if self.has_more_chars() {
            self.source[0]
        } else {
            '\0'
        }
    }

    fn next_char(&self) -> char {
        if self.source.len() > 1 {
            self.source[1]
        } else {
            '\0'
        }
    }

    fn advance(&mut self) {
        self.source = &self.source[1..];
        self.pos += 1;
    }

    fn get_bracket(&mut self) -> Result<Option<Token>, RuleSyntaxError> {
        let start = self.pos;
        let tokenkind: TokenKind;
        let value;

        match self.curr_char() {
            ')' => { tokenkind = TokenKind::RightBracket; value = ")"; self.inside_option = false },
            ']' => { tokenkind = TokenKind::RightSquare;  value = "]"; self.inside_matrix = false },
            '⟩' => { tokenkind = TokenKind::RightAngle;   value = "⟩"; self.inside_syll   = false },
            '}' => match self.next_char() {
                ':' => { tokenkind = TokenKind::RightColCurly; value = "}:"; self.inside_env_set = false; self.advance(); },
                 _  => { tokenkind = TokenKind::RightCurly;    value = "}";  self.inside_set = false; }
            },
            '⟨' => { 
                if self.inside_syll {
                    return Err(RuleSyntaxError::NestedBrackets(self.group, self.line, start));
                }
                tokenkind = TokenKind::LeftAngle; value = "⟨"; self.inside_syll = true;
            },
            '{' => { 
                if self.inside_set {
                    return Err(RuleSyntaxError::NestedBrackets(self.group, self.line, start));
                }
                tokenkind = TokenKind::LeftCurly; value = "{";  self.inside_set = true;
            },
            '(' => { 
                if self.inside_option {
                    return Err(RuleSyntaxError::NestedBrackets(self.group, self.line, start));
                }
                tokenkind = TokenKind::LeftBracket; value = "("; self.inside_option = true;
            },
            '[' => { 
                if self.inside_matrix {
                    return Err(RuleSyntaxError::NestedBrackets(self.group, self.line, start));
                }
                tokenkind = TokenKind::LeftSquare; value = "[";  self.inside_matrix = true;
            },
            _ => return Ok(None)
        }
        self.advance();

        Ok(Some(Token::new(tokenkind, value, self.group, self.line, start, self.pos)))

    }

    fn get_primative(&mut self) -> Option<Token> {
        if self.inside_matrix || !self.curr_char().is_ascii_uppercase() { return None }

        let start = self.pos;

        let c = self.chop(1);
        
        Some(Token::new(TokenKind::Group, c.as_ref(), self.group, self.line, start, self.pos))
    }

    fn get_numeric(&mut self) -> Option<Token> {
        if !self.curr_char().is_ascii_digit() { return None }

        let start = self.pos;

        let buffer = self.chop_while(|x| x.is_ascii_digit());

        Some(Token::new(TokenKind::Number, buffer.as_str(), self.group, self.line, start, self.pos))
    }

    fn get_feature(&mut self) -> Result<Option<Token>, RuleSyntaxError> {
        if !self.inside_matrix || self.curr_char() != '+' && self.curr_char() != '-' && !matches!(self.curr_char(), 'α'..='ω') && !self.curr_char().is_ascii_uppercase() {
            return Ok(None);
        }
        
        let start = self.pos;
        let val = self.curr_char();

        self.advance();
        
        let mod_val = if val == '-' && (matches!(self.curr_char(), 'α'..='ω') || self.curr_char().is_ascii_uppercase()) {
            let mut tmp = String::from('-'); tmp.push(self.curr_char());
            self.advance();
            tmp
        } else {
            String::from(val)
        };

        self.trim_whitespace();
        
        let mut buffer = String::new();

        while self.curr_char().is_ascii_alphabetic() || self.curr_char() == '.' {
            buffer.push(self.curr_char());
            self.advance();
            self.trim_whitespace();
        }

        if buffer.len() <= 1 { 
            return Err(RuleSyntaxError::ExpectedAlphabetic(self.curr_char(), self.group, self.line, self.pos))
        }

        let tkn_kind = self.feature_match(buffer, start, self.pos)?;
        
        if let TokenKind::Feature(FeatType::Supr(SupraType::Tone)) = tkn_kind { if mod_val == "+" || mod_val == "-" {
            return Err(RuleSyntaxError::WrongModTone(self.group, self.line, start))
        } }

        Ok(Some(Token::new(tkn_kind, &mod_val, self.group, self.line, start, self.pos)))
    }

    fn get_special_char(&mut self) -> Result<Option<Token>, RuleSyntaxError> {
        let start = self.pos;
        let tokenkind: TokenKind;
        
        let value = match self.curr_char() {
            ',' => { tokenkind = TokenKind::Comma;        self.chop(1) },
            '#' => { tokenkind = TokenKind::WordBoundary; self.chop(1) },
            '$' => { tokenkind = TokenKind::SyllBoundary; self.chop(1) },
            '%' => { tokenkind = TokenKind::Syllable;     self.chop(1) },
            '*' => { tokenkind = TokenKind::Star;         self.chop(1) },
            '∅' => { tokenkind = TokenKind::EmptySet;     self.chop(1) },
            '&' => { tokenkind = TokenKind::Ampersand;    self.chop(1) },
            '_' => { tokenkind = TokenKind::Underline;    self.chop_while(|c| *c == '_' ) },
            ':' => match self.next_char() { 
                '{' => { 
                    if self.inside_env_set {
                        return Err(RuleSyntaxError::NestedBrackets(self.group, self.line, start));
                    }

                    tokenkind = TokenKind::LeftColCurly;
                    self.inside_env_set = true;
                    self.chop(2) 
                },
                 _  => { tokenkind = TokenKind::Colon;        self.chop(1) }
            },
            '<' => { 
                if self.inside_syll {
                    return Err(RuleSyntaxError::NestedBrackets(self.group, self.line, start));
                }
                tokenkind = TokenKind::LeftAngle; self.inside_syll = true;
                self.chop(1)
            },
            '>' => {
                match self.inside_syll {
                    true  => tokenkind = TokenKind::RightAngle,
                    false => tokenkind = TokenKind::GreaterThan,
                }
                self.inside_syll = false;
                self.chop(1) 
            },
            '|' => { tokenkind = TokenKind::Pipe;         self.chop(1) },
            '/' => match self.next_char() {
                '/' => { tokenkind = TokenKind::DubSlash; self.chop(2) },
                 _  => { tokenkind = TokenKind::Slash;    self.chop(1) }
            },
            '=' => match self.next_char() { 
                '>' => { tokenkind = TokenKind::Arrow;    self.chop(2) },
                 _  => { tokenkind = TokenKind::Equals;   self.chop(1) },
             },
            '-' => match self.next_char() {
                '>' => { tokenkind = TokenKind::Arrow;    self.chop(2) },
                 _  => return Err(RuleSyntaxError::ExpectedCharArrow(self.next_char(), self.group, self.line, self.pos))
            },
            '…' | '⋯' => { tokenkind = TokenKind::Ellipsis; self.chop(1) },
            '.' => match self.next_char() {
                '.' => { tokenkind = TokenKind::Ellipsis; self.chop_while(|x| *x == '.') },
                _ => return Err(RuleSyntaxError::ExpectedCharDot(self.next_char(), self.group, self.line, self.pos))
            },
            _ => return Ok(None)
        };
        Ok(Some(Token::new(tokenkind, &value, self.group, self.line, start, self.pos)))
    }

    fn get_diacritic(&mut self) -> Option<Token> {
        if self.inside_matrix { return None }
        let start = self.pos;

        let mut char = self.curr_char();
        if char == '\'' { char = 'ʼ'; }
        for (i, d) in DIACRITS.iter().enumerate() {
            if char == d.diacrit {
                self.advance();
                return Some(Token::new(TokenKind::Diacritic(i as u8), &char.to_string(), self.group, self.line, start, self.pos))
            }
        }
        None
    }
    
    fn cur_as_ipa(&self) -> char {
        match self.curr_char() {
            'g' => 'ɡ',
            '?' => 'ʔ',
            '!' => 'ǃ',
            'ł' => 'ɬ',
            'ñ' => 'ɲ',
            'φ' => 'ɸ',
            // 'S' => 'ʃ', Can't have any of these in rules as they will be parsed as groups
            // 'Z' => 'ʒ',
            // 'C' => 'ɕ',
            // 'G' => 'ɢ',
            // 'N' => 'ɴ',
            // 'B' => 'ʙ',
            // 'R' => 'ʀ',
            // 'X' => 'χ',
            // 'H' => 'ʜ',
            // 'A' => 'ɐ',
            // 'E' => 'ɛ',
            // 'I' => 'ɪ',
            // 'O' => 'ɔ',
            // 'U' => 'ʊ',
            // 'Y' => 'ʏ',
            other => other,
        }
    }

    fn get_ipa(&mut self) -> Option<Token> {
        if self.inside_matrix { return None }
        let start = self.pos;

        let mut buffer = self.cur_as_ipa().to_string();

        // For americanist notation
        // TODO: This wont work with the pre-nasalised diacritic
        if buffer == "¢" {
            buffer = "t͡s".to_string()
        } else if buffer == "ƛ" {
            buffer = "t͡ɬ".to_string()
        } else if buffer == "λ" {
            buffer = "d͡ɮ".to_string()
        }
        
        if CARDINALS_TRIE.contains_prefix(buffer.as_str()) {
            self.advance();
            loop {
                let mut tmp = buffer.clone(); 
                tmp.push(self.cur_as_ipa());
                if CARDINALS_TRIE.contains_prefix(tmp.as_str()) {
                    buffer.push(self.cur_as_ipa());
                    self.advance();
                    continue;
                }

                // for prenasalisation to work with americanist chars
                if self.curr_char() == '¢' {
                    buffer.push_str("t͡s");
                    self.advance();
                    continue;
                } else if self.curr_char() == 'ƛ' {
                    buffer.push_str("t͡ɬ");
                    self.advance();
                    continue;
                } else if self.curr_char() == 'λ' {
                    buffer.push_str("d͡ɮ");
                    self.advance();
                    continue;
                }

                if self.curr_char() == '^' {
                    tmp.pop(); tmp.push('\u{0361}');
                    if CARDINALS_TRIE.contains_prefix(tmp.as_str()) {
                        buffer.push('\u{0361}');
                        self.advance();
                        continue;
                    }
                    tmp.pop(); tmp.push('\u{035C}');
                    if CARDINALS_TRIE.contains_prefix(tmp.as_str()) {
                        buffer.push('\u{035C}');
                        self.advance();
                        continue;
                    }
                    // if a click consonant
                    if let 'ʘ' | 'ǀ' | 'ǁ' | 'ǃ' | '!' | '‼' | 'ǂ' = self.next_char() {
                        tmp.pop(); tmp.push(self.cur_as_ipa());
                        self.advance();
                        continue;
                    }
                    // if a contour click
                    if let 'q'| 'ɢ' | 'ɴ' | 'χ' | 'ʁ' = self.next_char() {
                        if let Some('ʘ' | 'ǀ' | 'ǁ' | 'ǃ' | '!' | '‼' | 'ǂ') = tmp.chars().next() {
                            tmp.pop(); tmp.push(self.cur_as_ipa());
                            self.advance();
                            continue;
                        }
                    }
                }

                // if buffer.ends_with('\u{0361}') || buffer.ends_with('\u{035C}') {
                //     invalid 
                // }
                
                return Some(Token::new(TokenKind::Cardinal, &buffer, self.group, self.line, start, self.pos))
            }
        }
        None
    }

    fn get_string(&mut self) -> Result<Option<Token>, RuleSyntaxError> { 

        if !self.curr_char().is_ascii_alphabetic() { return Ok(None) }

        if !self.inside_matrix { 
            return Err(RuleSyntaxError::OutsideBrackets(self.group, self.line, self.pos))
        }

        let start = self.pos;

        let buffer = self.chop_while(|x| x.is_ascii_alphabetic());

        let tkn_kind: TokenKind = self.string_match(buffer, start)?;

        self.trim_whitespace();

        match self.curr_char() {
            ':' => self.advance(),
            _ => return Err(RuleSyntaxError::ExpectedCharColon(self.curr_char(), self.group, self.line, self.pos))
        } 
        
        self.trim_whitespace();

        match self.get_numeric() {
            Some(num) => Ok(Some(Token::new(tkn_kind, &num.value, self.group, self.line, start, self.pos))),
            _ => Err(RuleSyntaxError::ExpectedNumber(self.curr_char(), self.group, self.line, self.pos))

        }
    }

    fn string_match(&mut self, buffer: String, start: usize) -> Result<TokenKind, RuleSyntaxError> {
        use TokenKind::*;
        use FeatType::*;
        use SupraType::*;
        match buffer.to_lowercase().as_str() {
            "tone" | "ton" | "tne" | "tn" => Ok(Feature(Supr(Tone))),
            _ => Err(RuleSyntaxError::UnknownEnbyFeature(buffer.clone(), Position::new(self.group, self.line, start, start+buffer.len())))
        }
    }

    fn feature_match(&mut self, buffer: String, start: usize, end: usize) -> Result<TokenKind, RuleSyntaxError> {
        use TokenKind::*;
        use FeatType::*;
        use NodeType::*;
        use FType::*;
        use SupraType::*;
        match buffer.to_lowercase().as_str() {
            // Root Node Features
            "root"        | "rut"       | "rt"                    => Ok(Feature(Node(Root))),
            "consonantal" | "consonant" | "cons" | "cns"          => Ok(Feature(Feat(Consonantal))),
            "sonorant"    | "sonor"     | "son"  | "snrt" | "sn"  => Ok(Feature(Feat(Sonorant))),
            "syllabic"    | "syllab"    | "syll" | "syl"          => Ok(Feature(Feat(Syllabic))),
            // Manner Node Features
            "manner"         | "mann"   | "man"  | "mnnr" | "mnr" => Ok(Feature(Node(Manner))),
            "continuant"     | "contin" | "cont" | "cnt"          => Ok(Feature(Feat(Continuant))),
            "approximant"    | "approx" | "appr" | "app"          => Ok(Feature(Feat(Approximant))),
            "lateral"        | "latrl"  | "ltrl" | "lat"          => Ok(Feature(Feat(Lateral))),
            "nasal"          | "nsl"    | "nas"                   => Ok(Feature(Feat(Nasal))),
            "delayedrelease" | "delrel" | "d.r." | "del.rel." | 
            "delayed" | "dl" | "dlrl"   | "dr"   | "delay"    |
            "drelease"       | "del.rel"| "drel"                  => Ok(Feature(Feat(DelayedRelease))),
            "strident"       | "strid"  | "stri" | "stridnt"      => Ok(Feature(Feat(Strident))),
            "rhotic"         | "rhot"   | "rho"  | "rhtc" | "rh"  => Ok(Feature(Feat(Rhotic))),
            "click"          | "clik"   | "clk"  | "clck"          => Ok(Feature(Feat(Click))),
            // Laryngeal Node Features
            "laryngeal"      | "laryng"     | "laryn"  | "lar"    => Ok(Feature(Node(Laryngeal))),
            "voice"          | "voi"        | "vce"    | "vc"     => Ok(Feature(Feat(Voice))),
            "spreadglottis"  | "spreadglot" | 
            "spread"         | "s.g."       | "s.g"    | "sg"     => Ok(Feature(Feat(SpreadGlottis))),
            "constrictedglottis"            | "constricted"  |
            "constglot"      | "constr"     | "c.g." | "c.g" | 
            "cg"                                                  => Ok(Feature(Feat(ConstrGlottis))),
            // Place Node Feature
            "place"       | "plce"    | "plc"                     => Ok(Feature(Node(Place))),
            // Labial Place Node Features
            "labial"      | "lbl"     | "lab"                     => Ok(Feature(Node(Labial))),
            "labiodental" | "ldental" | "labiodent" | "labio" | 
            "labiod" | "labdent" | "lbdntl" | "ldent" | "ldl"     => Ok(Feature(Feat(Labiodental))),
            "round"       | "rund"    | "rnd"       | "rd"        => Ok(Feature(Feat(Round))),
            // Coronal Place Node Features
            "coronal"     | "coron"   | "crnl" | "cor"            => Ok(Feature(Node(Coronal))),
            "anterior"    | "anter"   | "antr" | "ant"            => Ok(Feature(Feat(Anterior))),
            "distributed" | "distrib" | "dist" | "dis" | "dst"    => Ok(Feature(Feat(Distributed))),
            // Dorsal Place Node Features
            "dorsal"  | "drsl"  | "dors" | "dor"                  => Ok(Feature(Node(Dorsal))),
            "front"   | "frnt"  | "fnt"  | "fro" | "frt" | "fr"   => Ok(Feature(Feat(Front))),
            "back"    | "bck"   | "bk"                            => Ok(Feature(Feat(Back))),
            "high"    | "hgh"   | "hi"                            => Ok(Feature(Feat(High))),
            "low"     | "lw"    | "lo"                            => Ok(Feature(Feat(Low))),
            "tense"   | "tens"  | "tns"  | "ten"                  => Ok(Feature(Feat(Tense))),
            "reduced" | "reduc" | "redu" | "rdcd" | "red"         => Ok(Feature(Feat(Reduced))),
            // Pharyngeal Place Node Features
            "pharyngeal" | "pharyng" | "pharyn"  |
            "phar"       | "phr"                                  => Ok(Feature(Node(Pharyngeal))),
            "advancedtongueroot"     | "a.t.r."  | "a.t.r" | 
            "a.tr" | "at.r" | "atr"                               => Ok(Feature(Feat(AdvancedTongueRoot))),
            "retractedtongueroot"    | "r.t.r."  | "r.t.r" | 
            "r.tr" | "rt.r" | "rtr"                               => Ok(Feature(Feat(RetractedTongueRoot))),
            // Suprasegmental Features
            "long"     | "lng"                                    => Ok(Feature(Supr(Long))),
            "overlong" | "overlng" | "ovrlng" | "vlong" | 
            "olong" | "vlng" | "olng"                             => Ok(Feature(Supr(Overlong))),
            "stress"   | "str"                                    => Ok(Feature(Supr(Stress))),
            "secondarystress"| "sec.stress" | "secstress" |
            "sec.str."       | "sec.str"    | "secstr"    | "sec" => Ok(Feature(Supr(SecStress))),
            
            _ => Err(RuleSyntaxError::UnknownFeature(buffer, Position::new(self.group, self.line, start, end)))
        }
    }

    fn get_comment(&mut self) -> Result<Option<Token>, RuleSyntaxError> {
        if self.curr_char() != ';' { return Ok(None) }
        let start = self.pos;
        self.advance();

        if self.curr_char() != ';' { return Err(RuleSyntaxError::MalformedComment(self.curr_char(), self.group, self.line, self.pos))}
        self.advance();
        let buffer = self.chop_while(|_| true);

        Ok(Some(Token::new(TokenKind::Comment, &buffer, self.group, self.line, start, self.pos)))
    }

    fn get_next_token(&mut self) -> Result<Token, RuleSyntaxError> {
        
        self.trim_whitespace();
        
        if !self.has_more_chars() { return Ok(Token::new(TokenKind::Eol, "", self.group, self.line, self.pos, self.pos+1)) }

        if let Some(com_token) = self.get_comment()?      { return Ok(com_token) }
        if let Some(bkt_token) = self.get_bracket()?      { return Ok(bkt_token) }
        if let Some(pmt_token) = self.get_primative()     { return Ok(pmt_token) }
        if let Some(num_token) = self.get_numeric()       { return Ok(num_token) }
        if let Some(ftr_token) = self.get_feature()?      { return Ok(ftr_token) }
        if let Some(spc_token) = self.get_special_char()? { return Ok(spc_token) }
        if let Some(ipa_token) = self.get_ipa()           { return Ok(ipa_token) }
        if let Some(dia_token) = self.get_diacritic()     { return Ok(dia_token) }
        if let Some(str_token) = self.get_string()?       { return Ok(str_token) } 
        
        Err(RuleSyntaxError::UnknownCharacter(self.curr_char(), self.group, self.line, self.pos))
    }

    pub(crate) fn get_line(&mut self) -> Result<Vec<Token>, RuleSyntaxError> {
        let mut token_list: Vec<Token> =  Vec::new();
        loop {
            let next_token = self.get_next_token()?;
            if let TokenKind::Eol = next_token.kind {
                token_list.push(next_token);
                break
            }
            token_list.push(next_token);
        }
        Ok(token_list)
    }
}

#[cfg(test)]
mod lexer_tests {

    use super::*;

    #[test]
    fn test_syll() {

        let test_input = "%";
        let expected_result = TokenKind::Syllable;

        let result = Lexer::new(&test_input.chars().collect::<Vec<_>>(), 0, 0).get_next_token().unwrap();

        assert_eq!(result.kind, expected_result);
        assert_eq!(result.value.as_ref(), test_input);
    }

    #[test]
    fn test_americanist_aliases() {
        let test_input= String::from("¢ ñ λ ł ƛ ⁿ¢ ⁿλ ⁿƛ");
        //                                    t͡s ɲ d͡ɮ ɬ t͡ɬ ⁿt͡s ⁿd͡ɮ ⁿt͡ɬ 
        let expected_result = vec![
            Token::new(TokenKind::Cardinal,  "t͡s"   , 0, 0,  0,  1),
            Token::new(TokenKind::Cardinal,   "ɲ"   , 0, 0,  2,  3),
            Token::new(TokenKind::Cardinal,  "d͡ɮ"   , 0,  0,  4,  5),
            Token::new(TokenKind::Cardinal,   "ɬ"   , 0, 0, 6, 7),
            Token::new(TokenKind::Cardinal,  "t͡ɬ"   , 0, 0, 8, 9),
            Token::new(TokenKind::Cardinal, "ⁿt͡s"   , 0, 0, 10, 12),
            Token::new(TokenKind::Cardinal, "ⁿd͡ɮ"   , 0, 0, 13, 15),
            Token::new(TokenKind::Cardinal, "ⁿt͡ɬ"   , 0, 0, 16, 18),
            Token::new(TokenKind::Eol,         ""    ,0, 0, 18, 19),
        ];

        let result = Lexer::new(&test_input.chars().collect::<Vec<_>>(), 0, 0).get_line().unwrap();        

        // assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_ipa_sep() {
        
        let test_input= String::from("t͡ɕ b͡β b a");
        
        let expected_result = vec![
            Token::new(TokenKind::Cardinal, "t͡ɕ", 0, 0,  0,  3),
            Token::new(TokenKind::Cardinal, "b͡β", 0, 0,  4,  7),
            Token::new(TokenKind::Cardinal,  "b", 0, 0,  8,  9),
            Token::new(TokenKind::Cardinal,  "a", 0, 0, 10, 11),
            Token::new(TokenKind::Eol,        "", 0, 0, 11, 12),
        ];

        let result = Lexer::new(&test_input.chars().collect::<Vec<_>>(), 0, 0).get_line().unwrap();        

        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_ipa_joined() {
        
        let test_input= String::from("t^ɕb͡βba");
        let expected_result = vec![
            Token::new(TokenKind::Cardinal, "t͡ɕ", 0, 0, 0, 3),
            Token::new(TokenKind::Cardinal, "b͡β", 0, 0, 3, 6),
            Token::new(TokenKind::Cardinal,  "b", 0, 0, 6, 7),
            Token::new(TokenKind::Cardinal,  "a", 0, 0, 7, 8),
            Token::new(TokenKind::Eol,        "", 0, 0, 8, 9),
        ];

        let result = Lexer::new(&test_input.chars().collect::<Vec<_>>(), 0, 0).get_line().unwrap();  

        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_ipa_tie() {
        
        let test_input= String::from("t^ɕ > b^β");
        let expected_result = vec![
            Token::new(TokenKind::Cardinal,   "t͡ɕ", 0, 0, 0, 3),
            Token::new(TokenKind::GreaterThan, ">", 0, 0, 4, 5),
            Token::new(TokenKind::Cardinal,   "b͡β", 0, 0, 6, 9),
            Token::new(TokenKind::Eol,          "", 0, 0, 9, 10),
        ];

        let result = Lexer::new(&test_input.chars().collect::<Vec<_>>(), 0, 0).get_line().unwrap();  

        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_metathesis() {
        
        let test_input= String::from("t^ɕ...b͡β > &");
        let expected_result = vec![
            Token::new(TokenKind::Cardinal,   "t͡ɕ", 0, 0,  0,  3),
            Token::new(TokenKind::Ellipsis,  "...", 0, 0,  3,  6),
            Token::new(TokenKind::Cardinal,   "b͡β", 0, 0,  6,  9),
            Token::new(TokenKind::GreaterThan, ">", 0, 0, 10, 11),
            Token::new(TokenKind::Ampersand,   "&", 0, 0, 12, 13),
            Token::new(TokenKind::Eol,          "", 0, 0, 13, 14),
        ];

        let result = Lexer::new(&test_input.chars().collect::<Vec<_>>(), 0, 0).get_line().unwrap();        

        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_feature_matrix() {
        use FeatType::*;
        let test_input= String::from("[+voi, -sg, αPLACE]");
        let expected_result = vec![
            Token::new(TokenKind::LeftSquare,                          "[", 0, 0,  0,  1),
            Token::new(TokenKind::Feature(Feat(FType::Voice)),         "+", 0, 0,  1,  5),
            Token::new(TokenKind::Comma,                               ",", 0, 0,  5,  6),
            Token::new(TokenKind::Feature(Feat(FType::SpreadGlottis)), "-", 0, 0,  7, 10),
            Token::new(TokenKind::Comma,                               ",", 0, 0, 10, 11),
            Token::new(TokenKind::Feature(Node(NodeType::Place)),      "α", 0, 0, 12, 18),
            Token::new(TokenKind::RightSquare,                         "]", 0, 0, 18, 19),
            Token::new(TokenKind::Eol,                                  "", 0, 0, 19, 20),
        ];

        let result = Lexer::new(&test_input.chars().collect::<Vec<_>>(), 0, 0).get_line().unwrap();        

        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);

        }
    }

    #[test]
    fn test_negative_alpha() {
        use FeatType::*;
        let test_input= String::from("[-αPLACE]");
        let expected_result = vec![
            Token::new(TokenKind::LeftSquare,                          "[", 0, 0,  0,  1),
            Token::new(TokenKind::Feature(Node(NodeType::Place)),     "-α", 0, 0,  1,  8),
            Token::new(TokenKind::RightSquare,                         "]", 0, 0,  8,  9),
            Token::new(TokenKind::Eol,                                  "", 0, 0,  9, 10),
        ];

        let result = Lexer::new(&test_input.chars().collect::<Vec<_>>(), 0, 0).get_line().unwrap();        

        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);

        }
    }

    #[test]
    fn test_variables() {
        
        let test_input= String::from("C=1 V=2 > 2 1 / _C // ___");
        let expected_result = vec![
            Token::new(TokenKind::Group,       "C", 0, 0,  0,  1),
            Token::new(TokenKind::Equals,      "=", 0, 0,  1,  2),
            Token::new(TokenKind::Number,      "1", 0, 0,  2,  3),
            Token::new(TokenKind::Group,       "V", 0, 0,  4,  5),
            Token::new(TokenKind::Equals,      "=", 0, 0,  5,  6),
            Token::new(TokenKind::Number,      "2", 0, 0,  6,  7),
            Token::new(TokenKind::GreaterThan, ">", 0, 0,  8,  9),
            Token::new(TokenKind::Number,      "2", 0, 0, 10, 11),
            Token::new(TokenKind::Number,      "1", 0, 0, 12, 13),
            Token::new(TokenKind::Slash,       "/", 0, 0, 14, 15),
            Token::new(TokenKind::Underline,   "_", 0, 0, 16, 17),
            Token::new(TokenKind::Group,       "C", 0, 0, 17, 18),
            Token::new(TokenKind::DubSlash,   "//", 0, 0, 19, 21),
            Token::new(TokenKind::Underline, "___", 0, 0, 22, 25),
            Token::new(TokenKind::Eol,          "", 0, 0, 25, 26),
        ];

        let result = Lexer::new(&test_input.chars().collect::<Vec<_>>(), 0, 0).get_line().unwrap();        

        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_brackets() {
        let test_input= String::from("{C,V} > [] / :{ j_{} }:");
        let expected_result = vec![
            Token::new(TokenKind::LeftCurly,      "{", 0, 0,  0,  1),
            Token::new(TokenKind::Group,          "C", 0, 0,  1,  2),
            Token::new(TokenKind::Comma,          ",", 0, 0,  2,  3),
            Token::new(TokenKind::Group,          "V", 0, 0,  3,  4),
            Token::new(TokenKind::RightCurly,     "}", 0, 0,  4,  5),
            Token::new(TokenKind::GreaterThan,    ">", 0, 0,  6,  7),
            Token::new(TokenKind::LeftSquare,     "[", 0, 0,  8,  9),
            Token::new(TokenKind::RightSquare,    "]", 0, 0,  9, 10),
            Token::new(TokenKind::Slash,          "/", 0, 0, 11, 12),
            Token::new(TokenKind::LeftColCurly,  ":{", 0, 0, 13, 15),
            Token::new(TokenKind::Cardinal,       "j", 0, 0, 16, 17),
            Token::new(TokenKind::Underline,      "_", 0, 0, 17, 18),
            Token::new(TokenKind::LeftCurly,      "{", 0, 0, 18, 19),
            Token::new(TokenKind::RightCurly,     "}", 0, 0, 19, 20),
            Token::new(TokenKind::RightColCurly, "}:", 0, 0, 21, 23),
            Token::new(TokenKind::Eol,             "", 0, 0, 23, 24),
        ];

        let result = Lexer::new(&test_input.chars().collect::<Vec<_>>(), 0, 0).get_line().unwrap();        

        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

}