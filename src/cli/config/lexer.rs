use std::{fmt, io, rc::Rc};

use colored::Colorize;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum TokenKind {
    Comma,      // ,
    Bang,       // !
    Tilde,      // !
    Colon,      // : 
    Arrow,      // >
    Literal,   
    // String, 
    Comment,    // '#'.* '\n'
    Eol,        // ('\r')'\n'
    Eof,        // End of file
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Comma    => write!(f, ","),
            TokenKind::Bang     => write!(f, "!"),
            TokenKind::Tilde    => write!(f, "~"),
            TokenKind::Colon    => write!(f, ":"),
            TokenKind::Arrow    => write!(f, ">"),
            TokenKind::Literal  => write!(f, "a literal"),
            TokenKind::Comment  => write!(f, "a comment"),
            TokenKind::Eol      => write!(f, "end of line"),
            TokenKind::Eof      => write!(f, "end of file"),
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
        while self.has_more_chars() && self.source[0].is_whitespace() && self.source[0] != '\n'{
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
        self.source = &self.source[1..];
        self.l_pos += 1;
    }

    fn get_special(&mut self) -> Result<Option<Token>, io::Error> {
        let start = self.l_pos;
        let start_line = self.l_num;
        let tokenkind: TokenKind;

        let value = match self.curr_char() {
            '>' => { tokenkind = TokenKind::Arrow;  self.chop(1) },
            ',' => { tokenkind = TokenKind::Comma;  self.chop(1) },
            ':' => { tokenkind = TokenKind::Colon;  self.chop(1) },
            '~' => { tokenkind = TokenKind::Tilde;  self.chop(1) },
            '!' => { tokenkind = TokenKind::Bang;   self.chop(1) },
             _  => return Ok(None)
        };

        Ok(Some(Token::new(tokenkind, &value, start_line, start, self.l_num, self.l_pos)))
    }

    fn get_comment(&mut self) -> Result<Option<Token>, io::Error> {
        if self.curr_char() != '#' { return Ok(None) }

        let start_line = self.l_num;
        let start = self.l_pos;
        let mut buffer = self.chop_while(|x| *x != '\n');
        self.advance();

        if buffer.ends_with('\r') {
            buffer.pop();
        }
        
        let end_line = self.l_num;
        let end = self.l_pos;

        self.l_num += 1;
        self.l_pos = 0;

        Ok(Some(Token::new(TokenKind::Comment, &buffer, start_line, start, end_line, end)))
    }


    fn is_line_terminator(x: &char) -> bool {
        // newline | line-separator | paragraph separator
        *x == '\n' || *x == '\u{2028}' || *x == '\u{2029}'
    }

    fn get_eol(&mut self) -> Result<Option<Token>, io::Error> {
        if self.curr_char() != '\n' { return Ok(None) }

        let start_line = self.l_num;
        let start = self.l_pos;

        let buffer = self.chop(1);

        let end_line = self.l_num;
        let end = self.l_pos;

        self.l_num += 1;
        self.l_pos = 0;

        Ok(Some(Token::new(TokenKind::Eol, &buffer, start_line, start, end_line, end)))

    }

    fn is_reserved_char(x: &char) -> bool {
        *x == '>' || *x == '!' || *x == '~' || *x == ',' || *x == '#'|| *x == ':'
    }

    // fn is_spaced_string_char(x: &char) -> bool {
    //     !((x.is_whitespace() && *x != ' ' ) ||  Self::is_reserved_char(x))
    // }

    fn is_unspaced_string_char(x: &char) -> bool {
        !(x.is_whitespace() ||  Self::is_reserved_char(x))
    }

    fn get_unspaced_string(&mut self) -> Result<Option<Token>, io::Error> {
        if !Self::is_unspaced_string_char(&self.curr_char()) { return Ok(None) }

        let s_line = self.l_num;
        let start = self.l_pos;

        let buffer = self.chop_while(Self::is_unspaced_string_char);

        Ok(Some(Token::new(TokenKind::Literal, &buffer, s_line, start, self.l_num, self.l_pos)))
    }

    fn get_next_token(&mut self) -> Result<Token, io::Error> {
        self.trim_whitespace();

        if !self.has_more_chars() { return Ok(Token::new(TokenKind::Eof, "", self.l_num, self.l_pos, self.l_num,self.l_pos+1)) }

        if let Some(com) = self.get_eol()? { return Ok(com) }
        if let Some(com) = self.get_comment()? { return Ok(com) }
        if let Some(spc) = self.get_special()? { return Ok(spc) }
        if let Some(str) = self.get_unspaced_string()?  { return Ok(str) }

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

    // #[test]
    // fn test_2() {
    //     let test_input= String::from(
    //         "foo.wsca bar.wsca > beta:\n \
    //             rules1.rsca ! Glottal Deletion\n \
    //             rules2.rsca ~ Cluster Simplification, Hap(lo)logy
    //         foo.wsca bar.wsca > alpha:\n \
    //             rules1.rsca ! Glottal Deletion\n \
    //             "
    //         );
    //         println!("{}", test_input);

    //         // from alpha 
    //         // with aliases.alias 
    //         // foo.wsca bar.wsca > beta:
    //         //     with aliases.alias
    //         //     with rules1.rsca ! "Glottal Deletion"
    //         //     with rules2.rsca ~ "Cluster Simplification", "Hap(lo)logy"
    //     let expected_result = vec![
    //         Token::new(TokenKind::Literal,       "foo.wsca", 1, 0,  1,  8),
    //         Token::new(TokenKind::Literal,       "bar.wsca", 1, 9,  1, 17),
    //         Token::new(TokenKind::Arrow,                ">", 1, 18, 1, 19),
    //         Token::new(TokenKind::Literal,           "beta", 1, 20, 1, 24),
    //         Token::new(TokenKind::Colon,                ":", 1, 24, 1, 25),
    //         Token::new(TokenKind::Eol,                 "\n", 1, 25, 1, 26),

    //         Token::new(TokenKind::Literal,    "rules1.rsca", 2,  1, 2, 12),
    //         Token::new(TokenKind::Bang,                 "!", 2, 13, 2, 14),
    //         Token::new(TokenKind::Literal,        "Glottal", 2, 15, 2, 22),
    //         Token::new(TokenKind::Literal,       "Deletion", 2, 23, 2, 31),
    //         Token::new(TokenKind::Eol,                 "\n", 2, 31, 2, 32),
   
    //         Token::new(TokenKind::Literal,    "rules2.rsca", 3,  1, 3, 12),
    //         Token::new(TokenKind::Tilde,                "~", 3, 13, 3, 14),
    //         Token::new(TokenKind::Literal,        "Cluster", 3, 15, 3, 22),
    //         Token::new(TokenKind::Literal, "Simplification", 3, 23, 3, 37),
    //         Token::new(TokenKind::Comma,                ",", 3, 37, 3, 38),
    //         Token::new(TokenKind::Literal,    "Hap(lo)logy", 3, 39, 3, 50),
    //         Token::new(TokenKind::Eof,                   "", 3, 50, 3, 51),
    //         ];
            
    //     let result = Lexer::new(&test_input.chars().collect::<Vec<_>>()).tokenise().unwrap();

    //     for res in result {
    //         println!("{:?}", res);
    //     }
    // }

    #[test]
    fn test_1() {
        
        let test_input= String::from(
            "foo.wsca bar.wsca > beta:\n \
                rules1.rsca ! Glottal Deletion\n \
                rules2.rsca ~ Cluster Simplification, Hap(lo)logy"
            );
            println!("{}", test_input);

            // from alpha 
            // with aliases.alias 
            // foo.wsca bar.wsca > beta:
            //     | aliases.alias
            //     | rules1.rsca ! "Glottal Deletion"
            //     | rules2.rsca ~ "Cluster Simplification", "Hap(lo)logy"
        let expected_result = vec![
            Token::new(TokenKind::Literal,       "foo.wsca", 1, 0,  1,  8),
            Token::new(TokenKind::Literal,       "bar.wsca", 1, 9,  1, 17),
            Token::new(TokenKind::Arrow,                ">", 1, 18, 1, 19),
            Token::new(TokenKind::Literal,           "beta", 1, 20, 1, 24),
            Token::new(TokenKind::Colon,                ":", 1, 24, 1, 25),
            Token::new(TokenKind::Eol,                 "\n", 1, 25, 1, 26),

            Token::new(TokenKind::Literal,    "rules1.rsca", 2,  1, 2, 12),
            Token::new(TokenKind::Bang,                 "!", 2, 13, 2, 14),
            Token::new(TokenKind::Literal,        "Glottal", 2, 15, 2, 22),
            Token::new(TokenKind::Literal,       "Deletion", 2, 23, 2, 31),
            Token::new(TokenKind::Eol,                 "\n", 2, 31, 2, 32),
   
            Token::new(TokenKind::Literal,    "rules2.rsca", 3,  1, 3, 12),
            Token::new(TokenKind::Tilde,                "~", 3, 13, 3, 14),
            Token::new(TokenKind::Literal,        "Cluster", 3, 15, 3, 22),
            Token::new(TokenKind::Literal, "Simplification", 3, 23, 3, 37),
            Token::new(TokenKind::Comma,                ",", 3, 37, 3, 38),
            Token::new(TokenKind::Literal,    "Hap(lo)logy", 3, 39, 3, 50),
            Token::new(TokenKind::Eof,                   "", 3, 50, 3, 51),
            ];
            
        let result = Lexer::new(&test_input.chars().collect::<Vec<_>>()).tokenise().unwrap();        
        
        assert_eq!(result.len(), expected_result.len());

        println!("{:#?}", result);
        
        for i in 0..expected_result.len() {
            assert_eq!(result[i], expected_result[i]);
        }

        // @proto-germanic %setup:
        //     "pgmc/pre",
        //     "pgmc/early",
        //     "pgmc/late",

        // setup > proto-germanic with aliases.alias:
        //     pgmc/pre.rsca
        //     pgmc/early.rsca
        //     pgmc/late.rsca

    }
}