use std :: {
    fmt :: { self, Display }, 
    rc  :: Rc
};

use crate :: {
    error :: RuleSyntaxError, 
    word  :: { FeatKind, FeatureCategory, NodeKind, SupraKind }, 
    CARDINALS_TRIE, DIACRITS
};

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
    GreaterThan,      // >
    Equals,           // =
    Underline,        // _
    Arrow,            // -> or =>
    Reverse,          // ~ or ~>
    Comma,            // ,
    Colon,            // :
    ExternBoundary,   // ##
    WordBoundary,     // #
    SyllBoundary,     // $
    Syllable,         // %
    Ampersand,        // &
    Group,            // Primitive Category i.e. C for Cons, V for Vowel
    Number,           // Number
    Slash,            // /
    Pipe,             // | 
    Cardinal,         // IPA character
    Diacritic(u8),    // IPA Diacritic
    Star,             // *
    EmptySet,         // ∅
    Ellipsis,         // ... or .. or … or ⋯
    WrappedEllipsis,  // (...) or (..) or (…) or (⋯)
    Comment,          // Delimited by ';;'
    Feature(FeatureCategory),
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
            TokenKind::LeftSquare      => write!(f, "LSquare"),
            TokenKind::RightSquare     => write!(f, "RSquare"),
            TokenKind::LeftCurly       => write!(f, "LCurly"),
            TokenKind::RightCurly      => write!(f, "RCurly"),
            TokenKind::LeftAngle       => write!(f, "LAngle"),
            TokenKind::RightAngle      => write!(f, "RAngle"),
            TokenKind::LeftBracket     => write!(f, "LBrack"),
            TokenKind::RightBracket    => write!(f, "RBrack"),
            TokenKind::LeftColCurly    => write!(f, "LCCurly"),
            TokenKind::RightColCurly   => write!(f, "RCCurly"),
            // TokenKind::LessThan      => write!(f, "LT"),
            TokenKind::GreaterThan     => write!(f, "GT"),
            TokenKind::Equals          => write!(f, "Eq"),
            TokenKind::Underline       => write!(f, "UL"),
            TokenKind::Arrow           => write!(f, "Arrow"),
            TokenKind::Reverse         => write!(f, "Reverse"),
            TokenKind::Comma           => write!(f, "Comma"),
            TokenKind::Colon           => write!(f, "Colon"),
            TokenKind::ExternBoundary  => write!(f, "XBound"),
            TokenKind::WordBoundary    => write!(f, "WBound"),
            TokenKind::SyllBoundary    => write!(f, "SBound"),
            TokenKind::Syllable        => write!(f, "Syll"),
            TokenKind::Ampersand       => write!(f, "Amper"),
            TokenKind::Group           => write!(f, "Prim"),
            TokenKind::Number          => write!(f, "Num"),
            TokenKind::Slash           => write!(f, "Slash"),
            TokenKind::Pipe            => write!(f, "Pipe"),
            TokenKind::Cardinal        => write!(f, "Cardinal"),
            TokenKind::Diacritic(i)    => write!(f, "Diacritic({i})"),
            TokenKind::Star            => write!(f, "Star"),
            TokenKind::EmptySet        => write!(f, "Empty"),
            TokenKind::Ellipsis        => write!(f, "Ellipsis"),
            TokenKind::WrappedEllipsis => write!(f, "WrappedEllipsis"),
            TokenKind::Comment         => write!(f, "Comment"),
            TokenKind::Feature(x)      => write!(f, "{x}"),
            TokenKind::Eol             => write!(f, "End of Line"),
        }
    }
}

#[doc(hidden)]
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

#[doc(hidden)]
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
        Self { 
            source, 
            group, 
            line, 
            pos: 0, 
            inside_matrix: false, 
            inside_option: false, 
            inside_syll: false, 
            inside_set: false, 
            inside_env_set: false, 
        }
    }

    fn has_more_chars(&self) -> bool { self.pos < self.source.len() }

    fn trim_whitespace(&mut self) {
        while self.has_more_chars() && self.source[self.pos].is_whitespace() {
            self.advance();
        }
    }

    fn chop(&mut self, n: usize) -> String {
        let token = &self.source[self.pos..self.pos+n];
        self.pos += n;
        token.iter().collect()
    }

    fn chop_while<P>(&mut self, mut predicate: P) -> String where P: FnMut(&char) -> bool {
        let mut n = 0;
        while self.pos+n < self.source.len() && predicate(&self.source[self.pos+n]) {
            n += 1;
        }
        self.chop(n)
    }

    fn curr_char(&self) -> char {
        if self.has_more_chars() {
            self.source[self.pos]
        } else {
            '\0'
        }
    }

    fn next_char(&self) -> char {
        if self.source.len() > self.pos+1 {
            self.source[self.pos+1]
        } else {
            '\0'
        }
    }

    fn last_char_eq(&self, ch: char) -> bool {
        if self.pos == 0 { return false }

        self.source[self.pos-1] == ch
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn back(&mut self) {
        debug_assert!(self.pos > 0);
        self.pos -= 1;
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
        
        if let TokenKind::Feature(FeatureCategory::Supr(SupraKind::Tone)) = tkn_kind { if mod_val == "+" || mod_val == "-" {
            return Err(RuleSyntaxError::WrongModTone(self.group, self.line, start))
        } }

        Ok(Some(Token::new(tkn_kind, &mod_val, self.group, self.line, start, self.pos)))
    }

    fn get_special_char(&mut self) -> Result<Option<Token>, RuleSyntaxError> {
        let start = self.pos;
        let tokenkind: TokenKind;
        
        let value = match self.curr_char() {
            ',' => { tokenkind = TokenKind::Comma;        self.chop(1) },
            '#' => match self.next_char() {
                '#' => { tokenkind = TokenKind::ExternBoundary; self.chop(2) },
                 _  => { tokenkind = TokenKind::WordBoundary;   self.chop(1) },
            },
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
                '/' => { tokenkind = TokenKind::Pipe;     self.chop(2) },
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
            '~' => match self.next_char() { 
                '>' => { tokenkind = TokenKind::Reverse;  self.chop(2) },
                 _  => { tokenkind = TokenKind::Reverse;  self.chop(1) },
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

        let char = self.cur_as_ipa();
        for (i, d) in DIACRITS.iter().enumerate() {
            if char == d.diacrit {
                self.advance();
                return Some(Token::new(TokenKind::Diacritic(i as u8), &char.to_string(), self.group, self.line, start, self.pos))
            }
        }
        None
    }
    
    fn cur_as_ipa(&mut self) -> char {
        match self.curr_char() {
            'g' => 'ɡ',
            '?' => 'ʔ',
            '!' => 'ǃ',
            'ł' => 'ɬ',
            'ñ' => 'ɲ',
            'φ' => 'ɸ',
            // TODO: Any printed errors may be off by +1
            '^' => match self.next_char() {
                'ʘ' | 'ǀ' | 'ǁ' | 'ǃ' | '!' | '‼' | 'ǂ' |
                'q' | 'ɢ' | 'ɴ' | 'χ' | 'ʁ'  => { self.advance(); self.cur_as_ipa() }
                _ => '^'
            },
            '"' => match self.next_char() {
                '\'' => { self.advance(); 'ʼ' },
                'j' => { self.advance(); 'ʲ' },
                'w' => { self.advance(); 'ʷ' },
                'g' | 'ɡ' => { self.advance(); 'ˠ' },
                'h' => { self.advance(); 'ʰ' },
                'ɦ' | 'H' => { self.advance(); 'ʱ' },
                's' => { self.advance(); 'ˢ' },
                'z' => { self.advance(); 'ᶻ' },
                'l' => { self.advance(); 'ˡ' },
                'r' => { self.advance(); 'ʵ'},
                
                'm' => { self.advance(); 'ᵐ' },
                'n' => { self.advance(); 'ⁿ' },
                'P' | 'ñ' | 'ɲ' => { self.advance(); 'ᶮ' },
                'T' | 'ɳ' => { self.advance(); 'ᶯ' },
                'G' | 'ŋ' => { self.advance(); 'ᵑ' },
                'N' | 'ɴ' => { self.advance(); 'ᶰ' },
                
                'v' | 'ʋ' => { self.advance(); 'ᶹ' },
                'y' | 'ɥ' => { self.advance(); 'ᶣ' },
                'X' | 'χ' => { self.advance(); 'ᵡ' },
                'R' | 'ʁ' => { self.advance(); 'ʶ' },
                'e' | 'ə' => { self.advance(); 'ᵊ' },
                '?' | 'ʔ' => { self.advance(); 'ˀ' },
                
                _ => '"'
            }
            'ꭤ' => 'ɑ',
            'ǝ' => 'ə',
            'ℇ' => 'ɛ',
            'ℎ' => 'h',
            'ℏ' => 'ħ',
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
                }

                if self.last_char_eq('^') || self.last_char_eq('"') {
                    self.back();
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
        use FeatureCategory::*;
        use SupraKind::*;
        match buffer.to_lowercase().as_str() {
            "tone" | "ton" | "tne" | "tn" => Ok(Feature(Supr(Tone))),
            _ => Err(RuleSyntaxError::UnknownEnbyFeature(buffer.clone(), Position::new(self.group, self.line, start, start+buffer.len())))
        }
    }

    fn feature_match(&mut self, buffer: String, start: usize, end: usize) -> Result<TokenKind, RuleSyntaxError> {
        use TokenKind::*;
        use FeatureCategory::*;
        use NodeKind::*;
        use FeatKind::*;
        use SupraKind::*;
        match buffer.to_lowercase().as_str() {
            // Root Node Features
            "root"        | "rut"       | "rt"                    => Ok(Feature(Node(Root))),
            "consonantal" | "consonant" | "cons" | "cns"          => Ok(Feature(Feat(Consonantal))),
            "sonorant"    | "sonor"     | "son"  | "snrt" | "sn"  => Ok(Feature(Feat(Sonorant))),
            "syllabic"    | "syllab"    | "syll" | "syl"  | "sl"  => Ok(Feature(Feat(Syllabic))),
            // Manner Node Features
            "manner"         | "mann"   | "man"  | "mnnr" | "mnr" => Ok(Feature(Node(Manner))),
            "continuant"     | "contin" | "cont" | "cnt"          => Ok(Feature(Feat(Continuant))),
            "approximant"    | "approx" | "appr" | "app"          => Ok(Feature(Feat(Approximant))),
            "lateral"        | "latrl"  | "ltrl" | "lat"  | "lt"  => Ok(Feature(Feat(Lateral))),
            "nasal"          | "nsl"    | "nas"  | "ns" | "nl"    => Ok(Feature(Feat(Nasal))),
            "delayedrelease" | "delrel" | "d.r." | "del.rel." | 
            "delayed" | "dl" | "dlrl"   | "dr"   | "delay"    |
            "drelease"       | "del.rel"| "drel" | "d.r" | "dr."  => Ok(Feature(Feat(DelayedRelease))),
            "strident"       | "strid"  | "stri" | "stridnt"  | 
            "strdent"        | "strdnt"                           => Ok(Feature(Feat(Strident))),
            "rhotic"         | "rhot"   | "rho"  | "rhtc" | "rh"
            | "rht" | "rhc"                                       => Ok(Feature(Feat(Rhotic))),
            "click"          | "clik"   | "clk"  | "clck"         => Ok(Feature(Feat(Click))),
            // Laryngeal Node Features
            "laryngeal"      | "laryng"     | "laryn"  | "lar"    => Ok(Feature(Node(Laryngeal))),
            "voice"          | "voi"        | "vce"    | "vc"     => Ok(Feature(Feat(Voice))),
            "spreadglottis"  | "spreadglot" | "spread" | 
            "s.g."           | "s.g"        | "sg."    | "sg"     => Ok(Feature(Feat(SpreadGlottis))),
            "constrictedglottis"            | "constricted"  |
            "constglot"      | "constr"     | "c.g." | "c.g" | 
            "cg." | "cg"                                          => Ok(Feature(Feat(ConstrGlottis))),
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
            "stress"   | "strs" | "str"                           => Ok(Feature(Supr(Stress))),
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
        if let Some(dia_token) = self.get_diacritic()     { return Ok(dia_token) }
        if let Some(ipa_token) = self.get_ipa()           { return Ok(ipa_token) }
        if let Some(str_token) = self.get_string()?       { return Ok(str_token) } 
        
        Err(RuleSyntaxError::UnknownCharacter(self.curr_char(), self.group, self.line, self.pos))
    }

    pub(crate) fn get_line(&mut self) -> Result<Vec<Token>, RuleSyntaxError> {
        let mut token_list: Vec<Token> =  Vec::new();
        loop {
            let next_token = self.get_next_token()?;

            match next_token.kind {
                TokenKind::Eol => {
                    token_list.push(next_token);
                    break
                },
                // Check if penult and ult tokens are '(' and '...'
                // If so, remove them and push on a new token '(...)'
                TokenKind::RightBracket => match (token_list.get(token_list.len().wrapping_sub(2)), token_list.last()) {
                    (Some(mb), Some(me)) if mb.kind == TokenKind::LeftBracket && me.kind == TokenKind::Ellipsis => {
                        let tk = Token { 
                            kind: TokenKind::WrappedEllipsis, 
                            value: format!("({})", me.value).into(), 
                            position: Position::new(self.group, self.line, mb.position.start, next_token.position.end)
                        };
                        
                        token_list.pop();
                        token_list.pop();
                        token_list.push(tk);
                    },
                    _ => token_list.push(next_token)
                }
                _ => token_list.push(next_token)
            }
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
            Token::new(TokenKind::Eol,         ""   ,0, 0, 18, 19),
        ];

        let result = Lexer::new(&test_input.chars().collect::<Vec<_>>(), 0, 0).get_line().unwrap();        

        // assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_floating_dia() {
        let test_input= String::from("\"H > \"h");

        let expected_res = vec![
            Token::new(TokenKind::Diacritic(29),  "ʱ", 0, 0, 0, 2),
            Token::new(TokenKind::GreaterThan,    ">", 0, 0, 3, 4),
            Token::new(TokenKind::Diacritic(28),  "ʰ", 0, 0, 5, 7),
            Token::new(TokenKind::Eol,             "", 0, 0, 7, 8),
        ];

        let res = Lexer::new(&test_input.chars().collect::<Vec<_>>(), 0, 0).get_line().unwrap();

        assert_eq!(res.len(), expected_res.len());

        for i in 0..res.len() {
            assert_eq!(res[i], expected_res[i]);
        }
    }

    #[test]
    fn test_ipa_sep() {
        
        let test_input= String::from("t͡ɕ b͡β b a ʘq ʘ^q qʘ q^ʘ b\"H");
        
        let expected_result = vec![
            Token::new(TokenKind::Cardinal, "t͡ɕ", 0, 0,  0,  3),
            Token::new(TokenKind::Cardinal, "b͡β", 0, 0,  4,  7),
            Token::new(TokenKind::Cardinal,  "b", 0, 0,  8,  9),
            Token::new(TokenKind::Cardinal,  "a", 0, 0, 10, 11),
            Token::new(TokenKind::Cardinal,  "ʘq", 0, 0, 12, 14),
            Token::new(TokenKind::Cardinal,  "ʘq", 0, 0, 15, 18),
            Token::new(TokenKind::Cardinal,  "qʘ", 0, 0, 19, 21),
            Token::new(TokenKind::Cardinal,  "qʘ", 0, 0, 22, 25),
            Token::new(TokenKind::Cardinal,  "b", 0, 0, 26, 27),
            Token::new(TokenKind::Diacritic(29),  "ʱ", 0, 0, 27, 29),

            Token::new(TokenKind::Eol,        "", 0, 0, 29, 30),
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
        // Standard Ellipsis
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

        // Wrapped Ellipsis
        let test_input= String::from("t^ɕ(...)b͡β > &");
        let expected_result = vec![
            Token::new(TokenKind::Cardinal,            "t͡ɕ", 0, 0,  0,  3),
            Token::new(TokenKind::WrappedEllipsis,  "(...)", 0, 0,  3,  8),
            Token::new(TokenKind::Cardinal,            "b͡β", 0, 0,  8, 11),
            Token::new(TokenKind::GreaterThan,          ">", 0, 0, 12, 13),
            Token::new(TokenKind::Ampersand,            "&", 0, 0, 14, 15),
            Token::new(TokenKind::Eol,                   "", 0, 0, 15, 16),
        ];

        let result = Lexer::new(&test_input.chars().collect::<Vec<_>>(), 0, 0).get_line().unwrap();        

        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_feature_matrix() {
        use FeatureCategory::*;
        let test_input= String::from("[+voi, -sg, αPLACE]");
        let expected_result = vec![
            Token::new(TokenKind::LeftSquare,                          "[", 0, 0,  0,  1),
            Token::new(TokenKind::Feature(Feat(FeatKind::Voice)),         "+", 0, 0,  1,  5),
            Token::new(TokenKind::Comma,                               ",", 0, 0,  5,  6),
            Token::new(TokenKind::Feature(Feat(FeatKind::SpreadGlottis)), "-", 0, 0,  7, 10),
            Token::new(TokenKind::Comma,                               ",", 0, 0, 10, 11),
            Token::new(TokenKind::Feature(Node(NodeKind::Place)),      "α", 0, 0, 12, 18),
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
        use FeatureCategory::*;
        let test_input= String::from("[-αPLACE]");
        let expected_result = vec![
            Token::new(TokenKind::LeftSquare,                          "[", 0, 0,  0,  1),
            Token::new(TokenKind::Feature(Node(NodeKind::Place)),     "-α", 0, 0,  1,  8),
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
            Token::new(TokenKind::Pipe,       "//", 0, 0, 19, 21),
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

    #[test]
    fn test_external_boundary() {
        // Deletion
        let test_input= String::from("## > *");

        let expected_result = vec![
            Token::new(TokenKind::ExternBoundary, "##", 0, 0,  0,  2),
            Token::new(TokenKind::GreaterThan,     ">", 0, 0,  3,  4),
            Token::new(TokenKind::Star,            "*", 0, 0,  5,  6),
            Token::new(TokenKind::Eol,              "", 0, 0,  6,  7),
        ];

        let result = Lexer::new(&test_input.chars().collect::<Vec<_>>(), 0, 0).get_line().unwrap();        

        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }

        // Environment
        let test_input= String::from("O > F / V ## _ V");

        let expected_result = vec![
            Token::new(TokenKind::Group,           "O", 0, 0,  0,  1),
            Token::new(TokenKind::GreaterThan,     ">", 0, 0,  2,  3),
            Token::new(TokenKind::Group,           "F", 0, 0,  4,  5),
            Token::new(TokenKind::Slash,           "/", 0, 0,  6,  7),
            Token::new(TokenKind::Group,           "V", 0, 0,  8,  9),
            Token::new(TokenKind::ExternBoundary, "##", 0, 0, 10, 12),
            Token::new(TokenKind::Underline,       "_", 0, 0, 13, 14),
            Token::new(TokenKind::Group,           "V", 0, 0, 15, 16),
            Token::new(TokenKind::Eol,              "", 0, 0, 16, 17),
        ];


        let result = Lexer::new(&test_input.chars().collect::<Vec<_>>(), 0, 0).get_line().unwrap();        

        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

}