use std::{fmt, io, rc::Rc};

use colored::Colorize;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum TokenKind {
    Comma,      // ,
    Bang,       // !
    Tilde,      // ~
    Colon,      // : 
    Semi,       // ;
    Lt,         // <
    LtEq,       // <=
    Gt,         // >
    GtEq,       // >=
    // Arrow,      // >
    Literal,   
    String, 
    Comment,    // '#'.* '\n'
    Eof,        // End of file
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Comma    => write!(f, ","),
            Self::Bang     => write!(f, "!"),
            Self::Tilde    => write!(f, "~"),
            Self::Colon    => write!(f, ":"),
            Self::Semi     => write!(f, ";"),
            Self::Lt       => write!(f, "<"),
            Self::LtEq     => write!(f, "<="),
            Self::Gt       => write!(f, ">"),
            Self::GtEq     => write!(f, ">="),
            Self::Literal  => write!(f, "a literal"),
            Self::String   => write!(f, "a string"),
            Self::Comment  => write!(f, "a comment"),
            Self::Eof      => write!(f, "end of file"),
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
    pub(super) value: Rc<str>, 
    pub(super) position: Position,
}

impl Token {
    pub(super) fn new(kind: TokenKind, value: &str, s_line: usize, start: usize, e_line:usize, end: usize) -> Self {
        Self { kind, value: Rc::from(value), position: Position::new(s_line, start, e_line, end) }
    }
}

#[derive(Default)]
pub(in super::super) struct Lexer<'a> {
    source: &'a [char],
    l_num: usize,
    l_pos: usize,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(source: &'a [char]) -> Self {
        Self { source, l_num: 1, l_pos: 0 }
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
        if self.curr_char() == '\r' && Self::is_line_terminator(&self.next_char()) {
            self.source = &self.source[2..];
            self.l_num += 1;
            self.l_pos = 0;
        } else if Self::is_line_terminator(&self.curr_char()) {
            self.source = &self.source[1..];
            self.l_num += 1;
            self.l_pos = 0;
        } else {
            self.source = &self.source[1..];
            self.l_pos += 1;
        }
    }

    fn get_special(&mut self) -> Result<Option<Token>, io::Error> {
        let start = self.l_pos;
        let start_line = self.l_num;
        let tokenkind: TokenKind;

        let value = match self.curr_char() {
            '<' => match self.next_char() {
                '=' => { tokenkind = TokenKind::LtEq; self.chop(2) }
                 _  => { tokenkind = TokenKind::Lt;   self.chop(1) }
            }
            '>' => match self.next_char() {
                '=' => { tokenkind = TokenKind::GtEq; self.chop(2) }
                 _  => { tokenkind = TokenKind::Gt;   self.chop(1) }
            }

            ',' => { tokenkind = TokenKind::Comma;  self.chop(1) }
            ':' => { tokenkind = TokenKind::Colon;  self.chop(1) }
            '~' => { tokenkind = TokenKind::Tilde;  self.chop(1) }
            '!' => { tokenkind = TokenKind::Bang;   self.chop(1) }
            ';' => { tokenkind = TokenKind::Semi;   self.chop(1) }
             _  => return Ok(None)
        };

        Ok(Some(Token::new(tokenkind, &value, start_line, start, self.l_num, self.l_pos)))
    }

    fn get_comment(&mut self) -> Result<Option<Token>, io::Error> {
        if self.curr_char() != '#' { return Ok(None) }

        let start_line = self.l_num;
        let start = self.l_pos;
        let mut buffer = self.chop_while(|x| !Self::is_line_terminator(x));
        if buffer.ends_with('\r') {
            buffer.pop();
        }
        
        let end_line = self.l_num;
        let end = self.l_pos;

        if self.has_more_chars() {
            self.advance();
        }

        Ok(Some(Token::new(TokenKind::Comment, &buffer, start_line, start, end_line, end)))
    }

    fn is_line_terminator(x: &char) -> bool {
        // newline | line-separator | paragraph separator
        *x == '\n' || *x == '\u{2028}' || *x == '\u{2029}'
    }

    fn get_string(&mut self) -> Result<Option<Token>, io::Error> {
        if self.curr_char() != '"' { return Ok(None) }
        self.advance();

        let s_line = self.l_num;
        let start = self.l_pos;
        let buffer = self.chop_while(|x| *x != '"' && !Self::is_line_terminator(x));

        match self.curr_char() {
            '"' => self.advance(),
            _ => return Err(self.error(format!("Unclosed string at {}:{}", self.l_num, self.l_pos))),
        }

        if buffer.is_empty() {
            return Err(self.error(format!("Empty string at {}:{}", self.l_num, self.l_pos)))
        }

        Ok(Some(Token::new(TokenKind::String, &buffer, s_line, start, self.l_num, self.l_pos)))
    }

    fn is_reserved_char(x: &char) -> bool {
        matches!(x, '>' | '!' | '~' | ',' | '#' | ':' | ';' | '"')
    }

    fn is_literal_char(x: &char) -> bool {
        !(x.is_whitespace() ||  Self::is_reserved_char(x))
    }

    fn get_literal(&mut self) -> Result<Option<Token>, io::Error> {
        if !Self::is_literal_char(&self.curr_char()) { return Ok(None) }

        let s_line = self.l_num;
        let start = self.l_pos;

        let buffer = self.chop_while(Self::is_literal_char);

        Ok(Some(Token::new(TokenKind::Literal, &buffer, s_line, start, self.l_num, self.l_pos)))
    }

    fn get_next_token(&mut self) -> Result<Token, io::Error> {
        self.trim_whitespace();

        if !self.has_more_chars() { return Ok(Token::new(TokenKind::Eof, "", self.l_num, self.l_pos, self.l_num,self.l_pos+1)) }

        if let Some(spc) = self.get_special()? { return Ok(spc) }
        if let Some(com) = self.get_comment()? { return Ok(com) }
        if let Some(str) = self.get_string()?  { return Ok(str) }
        if let Some(lit) = self.get_literal()? { return Ok(lit) }

        if self.curr_char().is_alphanumeric() {
            Err(self.error(format!("Undelimited string at {}:{}", self.l_num, self.l_pos)))
        } else {
            Err(self.error(format!("Unknown character at {}:{}", self.l_num, self.l_pos)))
        }
    }

    fn error(&self, message: String) -> io::Error {
        io::Error::other(format!("{}: {}", "Config Lex Error".bright_red(), message))
    }

    pub fn tokenise(&mut self) -> io::Result<Vec<Token>> {
        let mut tokens = Vec::new();
        loop {
            let next = self.get_next_token()?;
            if let TokenKind::Eof = next.kind {
                tokens.push(next);
                break
            }
            tokens.push(next);
        }
        Ok(tokens)
    }
}



#[cfg(test)]
mod lexer_tests {
    use super::*;

    #[test]
    fn test_end_line_comments_mult() {
        let test_input= String::from(
            "foo.wsca bar.wsca > beta: # test\n\
                rules1.rsca ! \"Glottal Deletion\"; # test\n\
                rules2.rsca ~ \"Cluster Simplification\", \"Hap(lo)logy\"; # test\n\
            \n\
            foo.wsca bar.wsca > beta: # test\n\
                rules1.rsca ! \"Glottal Deletion\"; # test\n\
                rules2.rsca ~ \"Cluster Simplification\", \"Hap(lo)logy\"; # test\n\
            # test\n\
            foo.wsca bar.wsca > beta: # test\n\
                rules1.rsca ! \"Glottal Deletion\"; # test\n\
                rules2.rsca ~ \"Cluster Simplification\", \"Hap(lo)logy\"; # test\n\
            foo.wsca bar.wsca > beta: # test\n\
                rules1.rsca ! \"Glottal Deletion\"; # test\n\
                rules2.rsca ~ \"Cluster Simplification\", \"Hap(lo)logy\"; # test"
        );

        let expected_result = vec![
            Token::new(TokenKind::Literal,               "foo.wsca", 1, 0,  1,  8),
            Token::new(TokenKind::Literal,               "bar.wsca", 1, 9,  1, 17),
            Token::new(TokenKind::Gt,                           ">", 1, 18, 1, 19),
            Token::new(TokenKind::Literal,                   "beta", 1, 20, 1, 24),
            Token::new(TokenKind::Colon,                        ":", 1, 24, 1, 25),
            Token::new(TokenKind::Comment,                 "# test", 1, 26, 1, 32),
            
            Token::new(TokenKind::Literal,            "rules1.rsca", 2,  0, 2, 11),
            Token::new(TokenKind::Bang,                         "!", 2, 12, 2, 13),
            Token::new(TokenKind::String,        "Glottal Deletion", 2, 15, 2, 32),
            Token::new(TokenKind::Semi,                         ";", 2, 32, 2, 33),
            Token::new(TokenKind::Comment,                 "# test", 2, 34, 2, 40),
            
            Token::new(TokenKind::Literal,            "rules2.rsca", 3,  0, 3, 11),
            Token::new(TokenKind::Tilde,                        "~", 3, 12, 3, 13),
            Token::new(TokenKind::String,  "Cluster Simplification", 3, 15, 3, 38),
            Token::new(TokenKind::Comma,                        ",", 3, 38, 3, 39),
            Token::new(TokenKind::String,             "Hap(lo)logy", 3, 41, 3, 53),
            Token::new(TokenKind::Semi,                         ";", 3, 53, 3, 54),
            Token::new(TokenKind::Comment,                 "# test", 3, 55, 3, 61),

            Token::new(TokenKind::Literal,               "foo.wsca", 5, 0,  5,  8),
            Token::new(TokenKind::Literal,               "bar.wsca", 5, 9,  5, 17),
            Token::new(TokenKind::Gt,                           ">", 5, 18, 5, 19),
            Token::new(TokenKind::Literal,                   "beta", 5, 20, 5, 24),
            Token::new(TokenKind::Colon,                        ":", 5, 24, 5, 25),
            Token::new(TokenKind::Comment,                 "# test", 5, 26, 5, 32),
            
            Token::new(TokenKind::Literal,            "rules1.rsca", 6,  0, 6, 11),
            Token::new(TokenKind::Bang,                         "!", 6, 12, 6, 13),
            Token::new(TokenKind::String,        "Glottal Deletion", 6, 15, 6, 32),
            Token::new(TokenKind::Semi,                         ";", 6, 32, 6, 33),
            Token::new(TokenKind::Comment,                 "# test", 6, 34, 6, 40),
            
            Token::new(TokenKind::Literal,            "rules2.rsca", 7,  0, 7, 11),
            Token::new(TokenKind::Tilde,                        "~", 7, 12, 7, 13),
            Token::new(TokenKind::String,  "Cluster Simplification", 7, 15, 7, 38),
            Token::new(TokenKind::Comma,                        ",", 7, 38, 7, 39),
            Token::new(TokenKind::String,             "Hap(lo)logy", 7, 41, 7, 53),
            Token::new(TokenKind::Semi,                         ";", 7, 53, 7, 54),
            Token::new(TokenKind::Comment,                 "# test", 7, 55, 7, 61),

            Token::new(TokenKind::Comment,                 "# test", 8, 0,  8,  6),

            Token::new(TokenKind::Literal,               "foo.wsca", 9, 0,  9,  8),
            Token::new(TokenKind::Literal,               "bar.wsca", 9, 9,  9, 17),
            Token::new(TokenKind::Gt,                        ">", 9, 18, 9, 19),
            Token::new(TokenKind::Literal,                   "beta", 9, 20, 9, 24),
            Token::new(TokenKind::Colon,                        ":", 9, 24, 9, 25),
            Token::new(TokenKind::Comment,                 "# test", 9, 26, 9, 32),
            
            Token::new(TokenKind::Literal,            "rules1.rsca", 10,  0, 10, 11),
            Token::new(TokenKind::Bang,                         "!", 10, 12, 10, 13),
            Token::new(TokenKind::String,        "Glottal Deletion", 10, 15, 10, 32),
            Token::new(TokenKind::Semi,                         ";", 10, 32, 10, 33),
            Token::new(TokenKind::Comment,                 "# test", 10, 34, 10, 40),
            
            Token::new(TokenKind::Literal,            "rules2.rsca", 11,  0, 11, 11),
            Token::new(TokenKind::Tilde,                        "~", 11, 12, 11, 13),
            Token::new(TokenKind::String,  "Cluster Simplification", 11, 15, 11, 38),
            Token::new(TokenKind::Comma,                        ",", 11, 38, 11, 39),
            Token::new(TokenKind::String,             "Hap(lo)logy", 11, 41, 11, 53),
            Token::new(TokenKind::Semi,                         ";", 11, 53, 11, 54),
            Token::new(TokenKind::Comment,                 "# test", 11, 55, 11, 61),

            Token::new(TokenKind::Literal,               "foo.wsca", 12, 0,  12,  8),
            Token::new(TokenKind::Literal,               "bar.wsca", 12, 9,  12, 17),
            Token::new(TokenKind::Gt,                           ">", 12, 18, 12, 19),
            Token::new(TokenKind::Literal,                   "beta", 12, 20, 12, 24),
            Token::new(TokenKind::Colon,                        ":", 12, 24, 12, 25),
            Token::new(TokenKind::Comment,                 "# test", 12, 26, 12, 32),
            
            Token::new(TokenKind::Literal,            "rules1.rsca", 13,  0, 13, 11),
            Token::new(TokenKind::Bang,                         "!", 13, 12, 13, 13),
            Token::new(TokenKind::String,        "Glottal Deletion", 13, 15, 13, 32),
            Token::new(TokenKind::Semi,                         ";", 13, 32, 13, 33),
            Token::new(TokenKind::Comment,                 "# test", 13, 34, 13, 40),
            
            Token::new(TokenKind::Literal,            "rules2.rsca", 14,  0, 14, 11),
            Token::new(TokenKind::Tilde,                        "~", 14, 12, 14, 13),
            Token::new(TokenKind::String,  "Cluster Simplification", 14, 15, 14, 38),
            Token::new(TokenKind::Comma,                        ",", 14, 38, 14, 39),
            Token::new(TokenKind::String,             "Hap(lo)logy", 14, 41, 14, 53),
            Token::new(TokenKind::Semi,                         ";", 14, 53, 14, 54),
            Token::new(TokenKind::Comment,                 "# test", 14, 55, 14, 61),

            Token::new(TokenKind::Eof,                           "", 14, 61, 14, 62),
        ];
            
        let maybe_result = Lexer::new(&test_input.chars().collect::<Vec<_>>()).tokenise();        
        
        let result = match &maybe_result {
            Ok(r) => r,
            Err(e) => {
                println!("{}", e);
                assert!(false);
                unreachable!()
            },
        };

        assert_eq!(result.len(), expected_result.len());
        
        for i in 0..expected_result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_end_line_comments() {
        let test_input= String::from(
            "foo.wsca bar.wsca > beta: # test\n\
                rules1.rsca ! \"Glottal Deletion\"; # test\n\
                rules2.rsca ~ \"Cluster Simplification\", \"Hap(lo)logy\"; # test"
        );

        let expected_result = vec![
            Token::new(TokenKind::Literal,              "foo.wsca", 1, 0,  1,  8),
            Token::new(TokenKind::Literal,              "bar.wsca", 1, 9,  1, 17),
            Token::new(TokenKind::Gt,                          ">", 1, 18, 1, 19),
            Token::new(TokenKind::Literal,                  "beta", 1, 20, 1, 24),
            Token::new(TokenKind::Colon,                       ":", 1, 24, 1, 25),
            Token::new(TokenKind::Comment,                "# test", 1, 26, 1, 32),
            
            Token::new(TokenKind::Literal,           "rules1.rsca", 2,  0, 2, 11),
            Token::new(TokenKind::Bang,                        "!", 2, 12, 2, 13),
            Token::new(TokenKind::String,       "Glottal Deletion", 2, 15, 2, 32),
            Token::new(TokenKind::Semi,                        ";", 2, 32, 2, 33),
            Token::new(TokenKind::Comment,                "# test", 2, 34, 2, 40),
            
            Token::new(TokenKind::Literal,           "rules2.rsca", 3,  0, 3, 11),
            Token::new(TokenKind::Tilde,                       "~", 3, 12, 3, 13),
            Token::new(TokenKind::String, "Cluster Simplification", 3, 15, 3, 38),
            Token::new(TokenKind::Comma,                       ",", 3, 38, 3, 39),
            Token::new(TokenKind::String,            "Hap(lo)logy", 3, 41, 3, 53),
            Token::new(TokenKind::Semi,                        ";", 3, 53, 3, 54),
            Token::new(TokenKind::Comment,                "# test", 3, 55, 3, 61),

            Token::new(TokenKind::Eof,                          "", 3, 61, 3, 62),
        ];
            
        let maybe_result = Lexer::new(&test_input.chars().collect::<Vec<_>>()).tokenise();        
        
        let result = match &maybe_result {
            Ok(r) => r,
            Err(e) => {
                println!("{}", e);
                assert!(false);
                unreachable!()
            },
        };

        assert_eq!(result.len(), expected_result.len());
  
        for i in 0..expected_result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_1() {
        
        let test_input= String::from(
            "foo.wsca bar.wsca > beta:\n\
                rules1.rsca ! \"Glottal Deletion\";\n\
                rules2.rsca ~ \"Cluster Simplification\", \"Hap(lo)logy\";"
        );

        let expected_result = vec![
            Token::new(TokenKind::Literal,              "foo.wsca", 1, 0,  1,  8),
            Token::new(TokenKind::Literal,              "bar.wsca", 1, 9,  1, 17),
            Token::new(TokenKind::Gt,                          ">", 1, 18, 1, 19),
            Token::new(TokenKind::Literal,                  "beta", 1, 20, 1, 24),
            Token::new(TokenKind::Colon,                       ":", 1, 24, 1, 25),
            
            Token::new(TokenKind::Literal,           "rules1.rsca", 2,  0, 2, 11),
            Token::new(TokenKind::Bang,                        "!", 2, 12, 2, 13),
            Token::new(TokenKind::String,       "Glottal Deletion", 2, 15, 2, 32),
            Token::new(TokenKind::Semi,                        ";", 2, 32, 2, 33),
            
            Token::new(TokenKind::Literal,           "rules2.rsca", 3,  0, 3, 11),
            Token::new(TokenKind::Tilde,                       "~", 3, 12, 3, 13),
            Token::new(TokenKind::String, "Cluster Simplification", 3, 15, 3, 38),
            Token::new(TokenKind::Comma,                       ",", 3, 38, 3, 39),
            Token::new(TokenKind::String,            "Hap(lo)logy", 3, 41, 3, 53),
            Token::new(TokenKind::Semi,                        ";", 3, 53, 3, 54),

            Token::new(TokenKind::Eof,                          "", 3, 54, 3, 55),
        ];
            
        let maybe_result = Lexer::new(&test_input.chars().collect::<Vec<_>>()).tokenise();        
        
        let result = match &maybe_result {
            Ok(r) => r,
            Err(e) => {
                println!("{}", e);
                assert!(false);
                unreachable!()
            },
        };

        assert_eq!(result.len(), expected_result.len());

        for i in 0..expected_result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_simple() {
        let test_input= String::from(
            "beta: rules1.rsca; rules2.rsca;"
        );

        let expected_result = vec![
            Token::new(TokenKind::Literal,                  "beta", 1,  0, 1,  4),
            Token::new(TokenKind::Colon,                       ":", 1,  4, 1,  5),
            Token::new(TokenKind::Literal,           "rules1.rsca", 1,  6, 1, 17),
            Token::new(TokenKind::Semi,                        ";", 1, 17, 1, 18),
            Token::new(TokenKind::Literal,           "rules2.rsca", 1, 19, 1, 30),
            Token::new(TokenKind::Semi,                        ";", 1, 30, 1, 31),
            Token::new(TokenKind::Eof,                          "", 1, 31, 1, 32),
        ];
            
        let maybe_result = Lexer::new(&test_input.chars().collect::<Vec<_>>()).tokenise();        
        
        let result = match &maybe_result {
            Ok(r) => r,
            Err(e) => {
                println!("{}", e);
                assert!(false);
                unreachable!()
            },
        };

        assert_eq!(result.len(), expected_result.len());

        for i in 0..expected_result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_lt_gt() {
        let test_input= String::from(
            "alpha > beta: rules1.rsca < \"asdf\"; rules2.rsca > \"asdf\";"
        );
        let expected_result = vec![
            Token::new(TokenKind::Literal,                 "alpha", 1,  0, 1,  5),
            Token::new(TokenKind::Gt,                          ">", 1,  6, 1,  7),
            Token::new(TokenKind::Literal,                  "beta", 1,  8, 1, 12),
            Token::new(TokenKind::Colon,                       ":", 1, 12, 1, 13),
            Token::new(TokenKind::Literal,           "rules1.rsca", 1, 14, 1, 25),
            Token::new(TokenKind::Lt,                          "<", 1, 26, 1, 27),
            Token::new(TokenKind::String,                   "asdf", 1, 29, 1, 34),
            Token::new(TokenKind::Semi,                        ";", 1, 34, 1, 35),
            Token::new(TokenKind::Literal,           "rules2.rsca", 1, 36, 1, 47),
            Token::new(TokenKind::Gt,                          ">", 1, 48, 1, 49),
            Token::new(TokenKind::String,                   "asdf", 1, 51, 1, 56),
            Token::new(TokenKind::Semi,                        ";", 1, 56, 1, 57),
            Token::new(TokenKind::Eof,                          "", 1, 57, 1, 58),
        ];

        let maybe_result = Lexer::new(&test_input.chars().collect::<Vec<_>>()).tokenise();        
        
        let result = match &maybe_result {
            Ok(r) => r,
            Err(e) => {
                println!("{}", e);
                assert!(false);
                unreachable!()
            },
        };

        assert_eq!(result.len(), expected_result.len());

        for i in 0..expected_result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }

    #[test]
    fn test_lt_eq_gt_eq() {
        let test_input= String::from(
            "alpha > beta: rules1.rsca <= \"asdf\"; rules2.rsca >= \"asdf\";"
        );
        let expected_result = vec![
            Token::new(TokenKind::Literal,                 "alpha", 1,  0, 1,  5),
            Token::new(TokenKind::Gt,                          ">", 1,  6, 1,  7),
            Token::new(TokenKind::Literal,                  "beta", 1,  8, 1, 12),
            Token::new(TokenKind::Colon,                       ":", 1, 12, 1, 13),
            Token::new(TokenKind::Literal,           "rules1.rsca", 1, 14, 1, 25),
            Token::new(TokenKind::LtEq,                       "<=", 1, 26, 1, 28),
            Token::new(TokenKind::String,                   "asdf", 1, 30, 1, 35),
            Token::new(TokenKind::Semi,                        ";", 1, 35, 1, 36),
            Token::new(TokenKind::Literal,           "rules2.rsca", 1, 37, 1, 48),
            Token::new(TokenKind::GtEq,                       ">=", 1, 49, 1, 51),
            Token::new(TokenKind::String,                   "asdf", 1, 53, 1, 58),
            Token::new(TokenKind::Semi,                        ";", 1, 58, 1, 59),
            Token::new(TokenKind::Eof,                          "", 1, 59, 1, 60),
        ];

        let maybe_result = Lexer::new(&test_input.chars().collect::<Vec<_>>()).tokenise();        
        
        let result = match &maybe_result {
            Ok(r) => r,
            Err(e) => {
                println!("{}", e);
                assert!(false);
                unreachable!()
            },
        };

        assert_eq!(result.len(), expected_result.len());

        for i in 0..expected_result.len() {
            assert_eq!(result[i], expected_result[i]);
        }
    }
}