use std::{io, fmt};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum TokenKind {
    LeftSquare,     // [
    RightSquare,    // ]
    LeftCurly,      // {
    RightCurly,     // }
    Comma,          // ,
    Bang,           // !
    Tilde,          // !
    Colon,          // : 
    Tag,            // @ [Alphanumeric char]+
    String,         // '"' [Alphanumeric char]+ '"'
    Comment,        // '#'.* '\n'
    EoF,            // End of file
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::LeftSquare  => write!(f, "["),
            TokenKind::RightSquare => write!(f, "]"),
            TokenKind::LeftCurly   => write!(f, "{{"),
            TokenKind::RightCurly  => write!(f, "}}"),
            TokenKind::Comma       => write!(f, ","),
            TokenKind::Bang        => write!(f, "!"),
            TokenKind::Tilde       => write!(f, "~"),
            TokenKind::Colon       => write!(f, ":"),
            TokenKind::Tag         => write!(f, "a tag"),
            TokenKind::String      => write!(f, "a string"),
            TokenKind::Comment     => write!(f, "a comment"),
            TokenKind::EoF         => write!(f, "end of file"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Position {
    pub(super) s_line: usize,
    pub(super) s_pos: usize,
    pub(super) e_line: usize,
    pub(super) e_pos: usize,
}

impl Position {
    pub(super) fn new(s_line: usize, s_pos: usize, e_line: usize, e_pos: usize) -> Self {
        Self { s_line, s_pos, e_line, e_pos }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub(super) kind: TokenKind,
    pub(super) value: String, 
    pub(super) position: Position,
}

impl Token {
    pub(super) fn new(kind: TokenKind, value: String, s_line: usize, start: usize, e_line:usize, end: usize) -> Self {
        Self { kind, value, position: Position::new(s_line, start, e_line, end) }
    }
}

#[derive(Default)]
pub(in super::super) struct Lexer<'a> {
    source: &'a [char],
    l_num: usize,
    l_pos: usize,
    inside_square: bool,
    inside_curly: bool,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(source: &'a [char]) -> Self {
        Self { source, l_num: 1, l_pos: 0, inside_square: false, inside_curly: false }
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
        self.l_pos += n;
        token.iter().collect()
    }

    fn chop_while<P>(&mut self, mut predicate: P) -> String where P: FnMut(&char) -> bool {
        let mut n = 0;
        let mut n_pos = 0;
        while n < self.source.len() && predicate(&self.source[n]) {
            if self.source[n] == '\n' {
                self.l_num += 1;
                self.l_pos = 0;
                n_pos = n;
            }
            n += 1;
        }
        let s = self.chop(n);
        self.l_pos -= n_pos;
        s
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
        if self.curr_char() == '\r' && self.next_char() == '\n' {
            self.source = &self.source[1..];
            self.source = &self.source[1..];
            self.l_num += 1;
            self.l_pos = 0;
        } else if self.curr_char() == '\n' {
            self.source = &self.source[1..];
            self.l_num += 1;
            self.l_pos = 0;
        } else {
            self.source = &self.source[1..];
            self.l_pos += 1;
        }
    }

    fn get_bracket(&mut self) -> Result<Option<Token>, io::Error> {
        let start_line = self.l_num;
        let start = self.l_pos;
        let tokenkind: TokenKind;
        let value: String;

        match self.curr_char() {
            '}' => { tokenkind = TokenKind::RightCurly;   value = "}".to_string(); self.inside_curly  = false },
            ']' => { tokenkind = TokenKind::RightSquare;  value = "]".to_string(); self.inside_square = false },
            '{' => { 
                if self.inside_curly {
                    return Err(io::Error::other(format!("Config Parse Error: Unclosed brackets at {}:{}", self.l_num, self.l_pos)))
                }
                tokenkind = TokenKind::LeftCurly; value = "{".to_string();  self.inside_curly = true
            },
            '[' => { 
                if self.inside_square {
                    return Err(io::Error::other(format!("Config Parse Error: Unclosed brackets at {}:{}", self.l_num, self.l_pos)))
                }
                tokenkind = TokenKind::LeftSquare; value = "[".to_string();  self.inside_square = true
            },
            _ => return Ok(None)
        }
        self.advance();

        Ok(Some(Token::new(tokenkind, value, start_line, start, self.l_num, self.l_pos)))
    }

    fn get_special(&mut self) -> Result<Option<Token>, io::Error> {
        let start = self.l_pos;
        let start_line = self.l_num;
        let tokenkind: TokenKind;

        let value = match self.curr_char() {
            ',' => { tokenkind = TokenKind::Comma;  self.chop(1) },
            ':' => { tokenkind = TokenKind::Colon;  self.chop(1) },
            '~' => { tokenkind = TokenKind::Tilde;  self.chop(1) },
            '!' => { tokenkind = TokenKind::Bang;   self.chop(1) },
             _  => return Ok(None)
        };

        Ok(Some(Token::new(tokenkind, value, start_line, start, self.l_num, self.l_pos)))
    }

    fn get_comment(&mut self) -> Result<Option<Token>, io::Error> {
        if self.curr_char() != '#' { return Ok(None) }

        let start_line = self.l_num;
        let start = self.l_pos;
        let mut buffer = self.chop_while(|x| *x != '\n');

        if buffer.ends_with('\r') {
            buffer.pop();
        }

        Ok(Some(Token::new(TokenKind::Comment, buffer, start_line, start, self.l_num, self.l_pos)))
    }

    fn get_string(&mut self) -> Result<Option<Token>, io::Error> {
        if self.curr_char() != '"' { return Ok(None) }
        self.advance();

        let s_line = self.l_num;
        let start = self.l_pos;
        let buffer = self.chop_while(|x| *x != '"' && *x != '\n');

        match self.curr_char() {
            '"' => self.advance(),
            _ => return Err(io::Error::other(format!("Config Parse Error: Unclosed string at {}:{}", self.l_num, self.l_pos))),
        }


        if buffer.is_empty() {
            return Err(io::Error::other(format!("Config Parse Error: Empty string at {}:{}", self.l_num, self.l_pos)))
        }

        Ok(Some(Token::new(TokenKind::String, buffer, s_line, start, self.l_num, self.l_pos)))
    }

    fn get_tag(&mut self) -> io::Result<Option<Token>> {
        if self.curr_char() != '@' { return Ok(None) }
        self.advance();

        let s_line = self.l_num;
        let start = self.l_pos;
        let buffer = self.chop_while(|x| x.is_alphanumeric());

        if buffer.is_empty() {
            return Err(io::Error::other(format!("Config Parse Error: Empty tag at {}:{}", self.l_num, self.l_pos)))
        }

        Ok(Some(Token::new(TokenKind::Tag, buffer, s_line, start, self.l_num, self.l_pos)))
    }

    fn get_next_token(&mut self) -> Result<Token, io::Error> {
        self.trim_whitespace();

        if !self.has_more_chars() { return Ok(Token::new(TokenKind::EoF, String::new(), self.l_num, self.l_pos, self.l_num,self.l_pos+1)) }

        if let Some(tag) = self.get_tag()?     { return Ok(tag) }
        if let Some(com) = self.get_comment()? { return Ok(com) }
        if let Some(spc) = self.get_special()? { return Ok(spc) }
        if let Some(bkt) = self.get_bracket()? { return Ok(bkt) }
        if let Some(str) = self.get_string()?  { return Ok(str) }

        if self.curr_char().is_alphanumeric() {
            Err(io::Error::other(format!("Config Parse Error: Unquoted string at {}:{}", self.l_num, self.l_pos)))
        } else {
            Err(io::Error::other(format!("Config Parse Error: Unknown character at {}:{}", self.l_num, self.l_pos)))
        }
    }

    pub fn tokenise(&mut self) -> io::Result<Vec<Token>> {
        let mut tokens = Vec::new();
        loop {
            let next = self.get_next_token()?;
            if let TokenKind::EoF = next.kind {
                tokens.push(next);
                break
            }
            tokens.push(next);
        }
        Ok(tokens)
    }
}