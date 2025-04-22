use crate :: { 
    error :: AliasSyntaxError, 
    word  :: { FeatureCategory, SupraKind }, 
    CARDINALS_TRIE, DIACRITS
};

use super :: { AliasKind, AliasPosition, AliasToken, AliasTokenKind, NamedEscape };


#[allow(unused)]
pub(crate) struct AliasLexer<'a> {
    kind: AliasKind,
    source: &'a [char],
    line: usize,
    pos: usize,
    past_arrow: bool,
    inside_matrix: bool,
    inside_angle: bool,
}

impl<'a> AliasLexer<'a> {
    pub(crate) fn new(kind: AliasKind, source: &'a [char], line: usize) -> Self {
        Self { kind, source, line, pos: 0, past_arrow: false, inside_matrix: false, inside_angle: false}
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

    fn get_bracket(&mut self) -> Result<Option<AliasToken>, AliasSyntaxError> {
        let start = self.pos;
        let tokenkind: AliasTokenKind;
        let value: String;

        match self.curr_char() {
            ']' => { tokenkind = AliasTokenKind::RightSquare;  value = "]".to_string(); self.inside_matrix = false },
            '[' => { 
                if self.inside_matrix {
                    return Err(AliasSyntaxError::NestedBrackets(self.kind, self.line, start));
                }
                tokenkind = AliasTokenKind::LeftSquare; value = "[".to_string();  self.inside_matrix = true
            },
            _ => return Ok(None)
        }
        self.advance();

        Ok(Some(AliasToken::new(tokenkind, value, AliasPosition::new(self.kind, self.line, start, self.pos))))
    }

    // TODO: Factor this out with Lexer::feature_match
    fn feature_match(&mut self, buffer: &str) -> Option<AliasTokenKind> {
        use AliasTokenKind::*;
        use crate::word::{FeatKind::*, FeatureCategory::*, NodeKind::*, SupraKind::*};
        match buffer.to_lowercase().as_str() {
            // Root Node Features
            "root"        | "rut"       | "rt"                    => Some(Feature(Node(Root))),
            "consonantal" | "consonant" | "cons" | "cns"          => Some(Feature(Feat(Consonantal))),
            "sonorant"    | "sonor"     | "son"  | "snrt" | "sn"  => Some(Feature(Feat(Sonorant))),
            "syllabic"    | "syllab"    | "syll" | "syl"          => Some(Feature(Feat(Syllabic))),
            // Manner Node Features
            "manner"         | "mann"   | "man"  | "mnnr" | "mnr" => Some(Feature(Node(Manner))),
            "continuant"     | "contin" | "cont" | "cnt"          => Some(Feature(Feat(Continuant))),
            "approximant"    | "approx" | "appr" | "app"          => Some(Feature(Feat(Approximant))),
            "lateral"        | "latrl"  | "ltrl" | "lat"          => Some(Feature(Feat(Lateral))),
            "nasal"          | "nsl"    | "nas"                   => Some(Feature(Feat(Nasal))),
            "delayedrelease" | "delrel" | "d.r." | "del.rel." | 
            "delayed" | "dl" | "dlrl"   | "dr"   | "delay"    |
            "drelease"       | "del.rel"| "drel"                  => Some(Feature(Feat(DelayedRelease))),
            "strident"       | "strid"  | "stri" | "stridnt"      => Some(Feature(Feat(Strident))),
            "rhotic"         | "rhot"   | "rho"  | "rhtc" | "rh"  => Some(Feature(Feat(Rhotic))),
            "click"          | "clik"   | "clk"  | "clck"         => Some(Feature(Feat(Click))),
            // Laryngeal Node Features
            "laryngeal"      | "laryng"     | "laryn"  | "lar"    => Some(Feature(Node(Laryngeal))),
            "voice"          | "voi"        | "vce"    | "vc"     => Some(Feature(Feat(Voice))),
            "spreadglottis"  | "spreadglot" | 
            "spread"         | "s.g."       | "s.g"    | "sg"     => Some(Feature(Feat(SpreadGlottis))),
            "constrictedglottis"            | "constricted"  |
            "constglot"      | "constr"     | "c.g." | "c.g" | 
            "cg"                                                  => Some(Feature(Feat(ConstrGlottis))),
            // Place Node Feature
            "place"       | "plce"    | "plc"                     => Some(Feature(Node(Place))),
            // Labial Place Node Features
            "labial"      | "lbl"     | "lab"                     => Some(Feature(Node(Labial))),
            "labiodental" | "ldental" | "labiodent" | "labio" | 
            "labiod" | "labdent" | "lbdntl" | "ldent" | "ldl"     => Some(Feature(Feat(Labiodental))),
            "round"       | "rund"    | "rnd"       | "rd"        => Some(Feature(Feat(Round))),
            // Coronal Place Node Features
            "coronal"     | "coron"   | "crnl" | "cor"            => Some(Feature(Node(Coronal))),
            "anterior"    | "anter"   | "antr" | "ant"            => Some(Feature(Feat(Anterior))),
            "distributed" | "distrib" | "dist" | "dis" | "dst"    => Some(Feature(Feat(Distributed))),
            // Dorsal Place Node Features
            "dorsal"  | "drsl"  | "dors" | "dor"                  => Some(Feature(Node(Dorsal))),
            "front"   | "frnt"  | "fnt"  | "fro" | "frt" | "fr"   => Some(Feature(Feat(Front))),
            "back"    | "bck"   | "bk"                            => Some(Feature(Feat(Back))),
            "high"    | "hgh"   | "hi"                            => Some(Feature(Feat(High))),
            "low"     | "lw"    | "lo"                            => Some(Feature(Feat(Low))),
            "tense"   | "tens"  | "tns"  | "ten"                  => Some(Feature(Feat(Tense))),
            "reduced" | "reduc" | "redu" | "rdcd" | "red"         => Some(Feature(Feat(Reduced))),
            // Pharyngeal Place Node Features
            "pharyngeal" | "pharyng" | "pharyn"  |
            "phar"       | "phr"                                  => Some(Feature(Node(Pharyngeal))),
            "advancedtongueroot"     | "a.t.r."  | "a.t.r" | 
            "a.tr" | "at.r" | "atr"                               => Some(Feature(Feat(AdvancedTongueRoot))),
            "retractedtongueroot"    | "r.t.r."  | "r.t.r" | 
            "r.tr" | "rt.r" | "rtr"                               => Some(Feature(Feat(RetractedTongueRoot))),
            // Suprasegmental Features
            "long"     | "lng"                                    => Some(Feature(Supr(Long))),
            "overlong" | "overlng" | "ovrlng" | "vlong" | 
            "olong" | "vlng" | "olng"                             => Some(Feature(Supr(Overlong))),
            "stress"   | "str"                                    => Some(Feature(Supr(Stress))),
            "secondarystress"| "sec.stress" | "secstress" |
            "sec.str."       | "sec.str"    | "secstr"    | "sec" => Some(Feature(Supr(SecStress))),
            
            _ => None
        }
    }

    fn get_feature(&mut self) -> Result<Option<AliasToken>, AliasSyntaxError> {
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
            return Err(AliasSyntaxError::ExpectedAlphabetic(self.curr_char(), self.kind, self.line, self.pos))
        }

        let Some(tkn_kind) = self.feature_match(&buffer) else {
            return Err(AliasSyntaxError::UnknownFeature(buffer, AliasPosition::new(self.kind, self.line, start, self.pos)))
        };
        
        if let AliasTokenKind::Feature(FeatureCategory::Supr(SupraKind::Tone)) = tkn_kind { if mod_val == "+" || mod_val == "-" {
            return Err(AliasSyntaxError::WrongModTone(self.kind, self.line, start))
        } }

        Ok(Some(AliasToken::new(tkn_kind, mod_val, AliasPosition::new(self.kind, self.line, start, self.pos))))
    }

    fn get_repl_plus(&mut self) -> Option<AliasToken> {
        if '+' != self.curr_char() { return None };
        let start = self.pos;
        let value = self.chop(1);
        Some(AliasToken::new(AliasTokenKind::Plus, value, AliasPosition::new(self.kind, self.line, start, self.pos))) 
    }

    fn get_special_char(&mut self) -> Result<Option<AliasToken>, AliasSyntaxError> {
        let start = self.pos;
        let tokenkind: AliasTokenKind;

        let value = match self.curr_char() {
            ',' => { tokenkind = AliasTokenKind::Comma;        self.chop(1) },
            ':' => { tokenkind = AliasTokenKind::Colon;        self.chop(1) },
            '#' => { tokenkind = AliasTokenKind::WordBoundary; self.chop(1) },
            '$' => { tokenkind = AliasTokenKind::SyllBoundary; self.chop(1) },
            '%' => { tokenkind = AliasTokenKind::Syllable;     self.chop(1) },
            '*' => { tokenkind = AliasTokenKind::Star;         self.chop(1) },
            '∅' => { tokenkind = AliasTokenKind::EmptySet;     self.chop(1) },
            '_' => { tokenkind = AliasTokenKind::Underline;    self.chop(1) },
            '<' => { 
                tokenkind = AliasTokenKind::LessThan;
                self.inside_angle = true;
                self.chop(1) 
            },
            '>' => { 
                tokenkind = AliasTokenKind::GreaterThan; 
                self.past_arrow = !self.inside_angle;
                self.inside_angle = false;
                self.chop(1) 
            },
            '=' => match self.next_char() { 
                '>' => { 
                    tokenkind = AliasTokenKind::Arrow; 
                    self.past_arrow = true; 
                    self.chop(2)
                },
                 _  => { tokenkind = AliasTokenKind::Equals;   self.chop(1) },
             },
            '-' => match self.next_char() {
                '>' => { 
                    tokenkind = AliasTokenKind::Arrow; 
                    self.past_arrow = true; 
                    self.chop(2) 
                },
                 _  => return Err(AliasSyntaxError::ExpectedCharArrow(self.next_char(), self.kind, self.line, self.pos))
            },
            // '…' | '⋯' => { tokenkind = TokenKind::Ellipsis; self.chop(1) },
            // '.' => match self.next_char() {
            //     '.' => { tokenkind = TokenKind::Ellipsis; self.chop_while(|x| *x == '.') },
            //     _ => return Err(RuleSyntaxError::ExpectedCharDot(self.next_char() self.line, self.pos))
            // },
            _ => return Ok(None)
        };

        Ok(Some(AliasToken::new(tokenkind, value, AliasPosition::new(self.kind, self.line, start, self.pos))))
    }

    fn get_diacritic(&mut self) -> Option<AliasToken> {
        if self.inside_matrix { return None }
        let start = self.pos;

        let mut char = self.curr_char();
        if char == '\'' { char = 'ʼ'; }
        for (i, d) in DIACRITS.iter().enumerate() {
            if char == d.diacrit {
                self.advance();
                return Some(AliasToken::new(AliasTokenKind::Diacritic(i as u8), char.to_string(), AliasPosition::new(self.kind, self.line, start, self.pos)))
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
            other => other,
        }
    }

    fn get_ipa(&mut self) -> Option<AliasToken> {
        if self.inside_matrix { return None }
        let start = self.pos;

        let mut buffer = self.cur_as_ipa().to_string();

        // For americanist notation
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
                return Some(AliasToken::new(AliasTokenKind::Cardinal, buffer, AliasPosition::new(self.kind, self.line, start, self.pos)))
            }
        }
        None
    }

    fn get_primative(&mut self) -> Option<AliasToken> {
        if self.inside_matrix || !self.curr_char().is_ascii_uppercase() { return None }
        let start = self.pos;
        let c = self.chop(1);
        
        Some(AliasToken::new(AliasTokenKind::Group, c, AliasPosition::new(self.kind, self.line, start, self.pos)))
    }


    fn get_enby(&mut self) -> Result<Option<AliasToken>, AliasSyntaxError> {

        if !self.curr_char().is_ascii_alphabetic() { return Ok(None) }

        if !self.inside_matrix { 
            return Err(AliasSyntaxError::OutsideBrackets(self.kind, self.line, self.pos))
        }

        let start = self.pos;
        let buffer = self.chop_while(|x| x.is_ascii_alphabetic());
        let tkn_kind: AliasTokenKind = self.string_match(buffer, start)?;

        self.trim_whitespace();

        match self.curr_char() {
            ':' => self.advance(),
            _ => return Err(AliasSyntaxError::ExpectedCharColon(self.curr_char(), self.kind, self.line, self.pos))
        } 
        
        self.trim_whitespace();

        match self.get_numeric() {
            Some(num) => Ok(Some(AliasToken::new(tkn_kind, num.value, AliasPosition::new(self.kind, self.line, start, self.pos)))),
            _ => Err(AliasSyntaxError::ExpectedNumber(self.curr_char(), self.kind, self.line, self.pos))

        }
    }

    fn string_match(&mut self, buffer: String, start: usize) -> Result<AliasTokenKind, AliasSyntaxError> {
        use AliasTokenKind::*;
        use FeatureCategory::*;
        use SupraKind::*;
        match buffer.to_lowercase().as_str() {
            "tone" | "ton" | "tne" | "tn" => Ok(Feature(Supr(Tone))),
            _ => Err(AliasSyntaxError::UnknownEnbyFeature(buffer.clone(), AliasPosition::new(self.kind, self.line, start, start+buffer.len())))
        }
    }


    fn get_numeric(&mut self) -> Option<AliasToken> {
        if !self.curr_char().is_ascii_digit() { return None }

        let start = self.pos;
        let buffer = self.chop_while(|x| x.is_ascii_digit());

        Some(AliasToken::new(AliasTokenKind::Number, buffer, AliasPosition::new(self.kind, self.line, start, self.pos)))
    }
    
    // Make sure this matches with char escapes (minus whitespace)
    fn is_valid_char(ch: &char) -> bool {
        !(ch.is_whitespace() 
        || *ch == '\\' || *ch == '@' || *ch == '$' || *ch == '∅' || *ch == '*' 
        || *ch == '>'  || *ch == '=' || *ch == '+' || *ch == '-' || *ch == ',')
    }

    fn get_unicode_escape(&mut self) -> Result<char, AliasSyntaxError> {
        match self.curr_char() {
            // Char Escape
            '\\' | '@' | '$' | '∅' | '*' | '>' | '=' | '+' | '-' | ',' => {
                let buf = self.curr_char();
                self.advance();
                Ok(buf)
            },
            // Unicode Escape
            'u' => {
                self.advance();
                self.parse_unicode_escape()
            },
            _ => Err(AliasSyntaxError::UnknownEscapeChar(self.curr_char(), self.kind, self.line, self.pos))
        }
    }

    fn parse_unicode_escape(&mut self) -> Result<char, AliasSyntaxError> {
        if self.curr_char() != '{' {
            return Err(AliasSyntaxError::ExpectedLeftCurly(self.curr_char(), self.kind, self.line, self.pos))
        }
        self.advance();
        self.trim_whitespace();
        let err_pos = self.pos;
        let buf = self.chop_while(|ch| ch.is_ascii_hexdigit());
        self.trim_whitespace();
        if self.curr_char() != '}' {
            return Err(AliasSyntaxError::ExpectedRightCurly(self.curr_char(), self.kind, self.line, self.pos))
        }
        self.advance();
        // Conv hex string to u32
        let Ok(val) = u32::from_str_radix(&buf, 16) else {
            return Err(AliasSyntaxError::InvalidUnicodeEscape(buf, self.kind, self.line, err_pos))
        };
        // Conv u32 to char
        let Some(str) = char::from_u32(val) else {
            return Err(AliasSyntaxError::InvalidUnicodeEscape(buf, self.kind, self.line, err_pos))
        };

        Ok(str)
    }

    fn get_named_escape(&mut self) -> Result<char, AliasSyntaxError> {
        if self.curr_char() != '{' {
            return Err(AliasSyntaxError::ExpectedLeftCurly(self.curr_char(), self.kind, self.line, self.pos))
        }
        self.advance();
        self.trim_whitespace();
        let err_pos = self.pos;
        let mut buf = String::new();
        while self.curr_char().is_ascii_alphabetic() {
            buf.push(self.curr_char());
            self.advance();
            self.trim_whitespace();
        }
        if self.curr_char() != '}' {
            return Err(AliasSyntaxError::ExpectedRightCurly(self.curr_char(), self.kind, self.line, self.pos))
        }
        self.advance();

        if let Ok(esc) = NamedEscape::try_from(buf.as_ref()) {
            Ok(esc.to_char())
        } else {
            Err(AliasSyntaxError::InvalidNamedEscape(buf, self.kind, self.line, err_pos))
        }
    }

    fn get_escape(&mut self) -> Result<Option<char>, AliasSyntaxError> {
        if self.curr_char() == '@' {
            self.advance();
            Ok(Some(self.get_named_escape()?))
        } else if self.curr_char() == '\\' {
            self.advance();
            Ok(Some(self.get_unicode_escape()?))
        } else {
            Ok(None)
        }
    }

    fn get_unicode_string(&mut self) -> Result<Option<AliasToken>, AliasSyntaxError> {
        if !Self::is_valid_char(&self.curr_char()) && self.curr_char() != '@' && self.curr_char() != '\\' { return Ok(None) }
        let start = self.pos;
        let mut end = self.pos+1;
        let mut buffer = String::with_capacity(1);

        while self.has_more_chars() {
            if Self::is_valid_char(&self.curr_char()) {
                buffer.push(self.curr_char());
                self.advance();
            } else if let Some(esc) = self.get_escape()? {
                buffer.push(esc);
            } else {
                break;
            }
            end = self.pos;
            self.trim_whitespace();
        }

        Ok(Some(AliasToken::new(AliasTokenKind::String, buffer, AliasPosition::new(self.kind, self.line, start, end))))
    }

    fn get_next_token(&mut self) -> Result<AliasToken, AliasSyntaxError> {
        self.trim_whitespace();

        if !self.has_more_chars() { return Ok(AliasToken::new(AliasTokenKind::Eol, String::new(), AliasPosition::new(self.kind, self.line, self.pos, self.pos+1))) }
        
        match self.kind {
            AliasKind::Deromaniser => {
                if self.past_arrow {
                    if let Some(bkt_token) = self.get_bracket()?       { return Ok(bkt_token) }
                    if let Some(pmt_token) = self.get_primative()      { return Ok(pmt_token) }
                    if let Some(ftr_token) = self.get_feature()?       { return Ok(ftr_token) }
                    if let Some(ipa_token) = self.get_ipa()            { return Ok(ipa_token) }
                    if let Some(dia_token) = self.get_diacritic()      { return Ok(dia_token) }
                    if let Some(str_token) = self.get_enby()?          { return Ok(str_token) } 
        
                } else {
                    if let Some(plu_token) = self.get_repl_plus()       { return Ok(plu_token) }
                    if let Some(str_token) = self.get_unicode_string()? { return Ok(str_token) }
                }
                
            },
            AliasKind::Romaniser => {
                if !self.past_arrow {
                    if let Some(bkt_token) = self.get_bracket()?       { return Ok(bkt_token) }
                    if let Some(pmt_token) = self.get_primative()      { return Ok(pmt_token) }
                    if let Some(ftr_token) = self.get_feature()?       { return Ok(ftr_token) }
                    if let Some(ipa_token) = self.get_ipa()            { return Ok(ipa_token) }
                    if let Some(dia_token) = self.get_diacritic()      { return Ok(dia_token) }
                    if let Some(str_token) = self.get_enby()?          { return Ok(str_token) } 
        
                } else {
                    if let Some(plu_token) = self.get_repl_plus()       { return Ok(plu_token) }
                    if let Some(str_token) = self.get_unicode_string()? { return Ok(str_token) }
                }
            },
        };

        if let Some(spc_token) = self.get_special_char()?      { return Ok(spc_token) }

        Err(AliasSyntaxError::UnknownCharacter(self.curr_char(), self.kind, self.line, self.pos))
    }

    pub(crate) fn get_line(&mut self) -> Result<Vec<AliasToken>, AliasSyntaxError> {
        let mut token_list: Vec<AliasToken> =  Vec::new();
        loop {
            let next_token = self.get_next_token()?;
            if let AliasTokenKind::Eol = next_token.kind {
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
    fn test_deromanisation_simple() {
        let test_input= String::from("sh > ʃ");
        let expected_result = vec![
            AliasToken::new(AliasTokenKind::String,     "sh".to_owned(), AliasPosition::new(AliasKind::Deromaniser, 0, 0, 2)),
            AliasToken::new(AliasTokenKind::GreaterThan, ">".to_owned(), AliasPosition::new(AliasKind::Deromaniser, 0, 3, 4)),
            AliasToken::new(AliasTokenKind::Cardinal,    "ʃ".to_owned(), AliasPosition::new(AliasKind::Deromaniser, 0, 5, 6)),
            AliasToken::new(AliasTokenKind::Eol,          String::new(), AliasPosition::new(AliasKind::Deromaniser, 0, 6, 7)),
        ];
        
        let result = AliasLexer::new(AliasKind::Deromaniser, &test_input.chars().collect::<Vec<_>>(),  0).get_line().unwrap();  
    
        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_deromanisation_stress() {
        use FeatureCategory::*;
        use SupraKind::*;
        let test_input= String::from("á > a:[+str]");
        let expected_result = vec![
            AliasToken::new(AliasTokenKind::String,                 "á".to_owned(), AliasPosition::new(AliasKind::Deromaniser, 0,  0,  1)),
            AliasToken::new(AliasTokenKind::GreaterThan,            ">".to_owned(), AliasPosition::new(AliasKind::Deromaniser, 0,  2,  3)),
            AliasToken::new(AliasTokenKind::Cardinal,               "a".to_owned(), AliasPosition::new(AliasKind::Deromaniser, 0,  4,  5)),
            AliasToken::new(AliasTokenKind::Colon,                  ":".to_owned(), AliasPosition::new(AliasKind::Deromaniser, 0,  5,  6)),
            AliasToken::new(AliasTokenKind::LeftSquare,             "[".to_owned(), AliasPosition::new(AliasKind::Deromaniser, 0,  6,  7)),
            AliasToken::new(AliasTokenKind::Feature(Supr(Stress)),  "+".to_owned(), AliasPosition::new(AliasKind::Deromaniser, 0,  7, 11)),
            AliasToken::new(AliasTokenKind::RightSquare,            "]".to_owned(), AliasPosition::new(AliasKind::Deromaniser, 0, 11, 12)),
            AliasToken::new(AliasTokenKind::Eol,                     String::new(), AliasPosition::new(AliasKind::Deromaniser, 0, 12, 13)),
        ];
        
        let result = AliasLexer::new(AliasKind::Deromaniser, &test_input.chars().collect::<Vec<_>>(),  0).get_line().unwrap();  
    
        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_romanisation_simple() {
        let test_input= String::from("ʃ > sh");
        let expected_result = vec![
            AliasToken::new(AliasTokenKind::Cardinal,    "ʃ".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0, 0, 1)),
            AliasToken::new(AliasTokenKind::GreaterThan, ">".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0, 2, 3)),
            AliasToken::new(AliasTokenKind::String,     "sh".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0, 4, 6)),
            AliasToken::new(AliasTokenKind::Eol,          String::new(), AliasPosition::new(AliasKind::Romaniser, 0, 6, 7)),
        ];
        
        let result = AliasLexer::new(AliasKind::Romaniser, &test_input.chars().collect::<Vec<_>>(),  0).get_line().unwrap();  
    
        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_romanisation_length() {
        use FeatureCategory::*;
        use SupraKind::*;
        let test_input= String::from("ʃ:[-long] > sh");
        let expected_result = vec![
            AliasToken::new(AliasTokenKind::Cardinal,               "ʃ".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  0,  1)),
            AliasToken::new(AliasTokenKind::Colon,                  ":".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  1,  2)),
            AliasToken::new(AliasTokenKind::LeftSquare,             "[".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  2,  3)),
            AliasToken::new(AliasTokenKind::Feature(Supr(Long)),    "-".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  3,  8)),
            AliasToken::new(AliasTokenKind::RightSquare,            "]".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  8,  9)),
            AliasToken::new(AliasTokenKind::GreaterThan,            ">".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0, 10, 11)),
            AliasToken::new(AliasTokenKind::String,                "sh".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0, 12, 14)),
            AliasToken::new(AliasTokenKind::Eol,                     String::new(), AliasPosition::new(AliasKind::Romaniser, 0, 14, 15)),
        ];

        let result = AliasLexer::new(AliasKind::Romaniser, &test_input.chars().collect::<Vec<_>>(),  0).get_line().unwrap();  

        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }


        let test_input= String::from("ʃ:[+long] > ssh");
        let expected_result = vec![
            AliasToken::new(AliasTokenKind::Cardinal,               "ʃ".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  0,  1)),
            AliasToken::new(AliasTokenKind::Colon,                  ":".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  1,  2)),
            AliasToken::new(AliasTokenKind::LeftSquare,             "[".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  2,  3)),
            AliasToken::new(AliasTokenKind::Feature(Supr(Long)),    "+".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  3,  8)),
            AliasToken::new(AliasTokenKind::RightSquare,            "]".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  8,  9)),
            AliasToken::new(AliasTokenKind::GreaterThan,            ">".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0, 10, 11)),
            AliasToken::new(AliasTokenKind::String,               "ssh".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0, 12, 15)),
            AliasToken::new(AliasTokenKind::Eol,                     String::new(), AliasPosition::new(AliasKind::Romaniser, 0, 15, 16)),
        ];

        let result = AliasLexer::new(AliasKind::Romaniser, &test_input.chars().collect::<Vec<_>>(),  0).get_line().unwrap();  

        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_romanisation_stress() {
        use FeatureCategory::*;
        use SupraKind::*;
        let test_input= String::from("a:[+str] > á");
        let expected_result = vec![
            AliasToken::new(AliasTokenKind::Cardinal,               "a".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  0,  1)),
            AliasToken::new(AliasTokenKind::Colon,                  ":".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  1,  2)),
            AliasToken::new(AliasTokenKind::LeftSquare,             "[".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  2,  3)),
            AliasToken::new(AliasTokenKind::Feature(Supr(Stress)),  "+".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  3,  7)),
            AliasToken::new(AliasTokenKind::RightSquare,            "]".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  7,  8)),
            AliasToken::new(AliasTokenKind::GreaterThan,            ">".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  9, 10)),
            AliasToken::new(AliasTokenKind::String,                 "á".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0, 11, 12)),
            AliasToken::new(AliasTokenKind::Eol,                     String::new(), AliasPosition::new(AliasKind::Romaniser, 0, 12, 13)),
        ];
        
        let result = AliasLexer::new(AliasKind::Romaniser, &test_input.chars().collect::<Vec<_>>(),  0).get_line().unwrap();  
    
        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_romanisation_named_escape() {
        use FeatureCategory::*;
        use SupraKind::*;
        let test_input= String::from("a:[+str] > a@{acute}");
        let expected_result = vec![
            AliasToken::new(AliasTokenKind::Cardinal,               "a".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  0,  1)),
            AliasToken::new(AliasTokenKind::Colon,                  ":".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  1,  2)),
            AliasToken::new(AliasTokenKind::LeftSquare,             "[".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  2,  3)),
            AliasToken::new(AliasTokenKind::Feature(Supr(Stress)),  "+".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  3,  7)),
            AliasToken::new(AliasTokenKind::RightSquare,            "]".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  7,  8)),
            AliasToken::new(AliasTokenKind::GreaterThan,            ">".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  9, 10)),
            AliasToken::new(AliasTokenKind::String,         "a\u{0301}".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0, 11, 20)),
            AliasToken::new(AliasTokenKind::Eol,                     String::new(), AliasPosition::new(AliasKind::Romaniser, 0, 20, 21)),
        ];
        
        let result = AliasLexer::new(AliasKind::Romaniser, &test_input.chars().collect::<Vec<_>>(),  0).get_line().unwrap();  
    
        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_romanisation_unicode_escape() {
        use FeatureCategory::*;
        use SupraKind::*;
        let test_input= String::from("a:[+str] > a\\u{0301}");
        let expected_result = vec![
            AliasToken::new(AliasTokenKind::Cardinal,               "a".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  0,  1)),
            AliasToken::new(AliasTokenKind::Colon,                  ":".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  1,  2)),
            AliasToken::new(AliasTokenKind::LeftSquare,             "[".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  2,  3)),
            AliasToken::new(AliasTokenKind::Feature(Supr(Stress)),  "+".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  3,  7)),
            AliasToken::new(AliasTokenKind::RightSquare,            "]".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  7,  8)),
            AliasToken::new(AliasTokenKind::GreaterThan,            ">".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  9, 10)),
            AliasToken::new(AliasTokenKind::String,         "a\u{0301}".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0, 11, 20)),
            AliasToken::new(AliasTokenKind::Eol,                     String::new(), AliasPosition::new(AliasKind::Romaniser, 0, 20, 21)),
        ];
        
        let result = AliasLexer::new(AliasKind::Romaniser, &test_input.chars().collect::<Vec<_>>(),  0).get_line().unwrap();  
    
        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_romanisation_literal_escape() {
        use FeatureCategory::*;
        use SupraKind::*;
        let test_input= String::from("a:[+str] > a\\$");
        let expected_result = vec![
            AliasToken::new(AliasTokenKind::Cardinal,               "a".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  0,  1)),
            AliasToken::new(AliasTokenKind::Colon,                  ":".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  1,  2)),
            AliasToken::new(AliasTokenKind::LeftSquare,             "[".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  2,  3)),
            AliasToken::new(AliasTokenKind::Feature(Supr(Stress)),  "+".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  3,  7)),
            AliasToken::new(AliasTokenKind::RightSquare,            "]".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  7,  8)),
            AliasToken::new(AliasTokenKind::GreaterThan,            ">".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0,  9, 10)),
            AliasToken::new(AliasTokenKind::String,                "a$".to_owned(), AliasPosition::new(AliasKind::Romaniser, 0, 11, 14)),
            AliasToken::new(AliasTokenKind::Eol,                     String::new(), AliasPosition::new(AliasKind::Romaniser, 0, 14, 15)),
        ];
        
        let result = AliasLexer::new(AliasKind::Romaniser, &test_input.chars().collect::<Vec<_>>(),  0).get_line().unwrap();  
    
        assert_eq!(result.len(), expected_result.len());

        for i in 0..result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }
}