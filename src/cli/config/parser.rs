use std::{collections::HashSet, io, path::{Path, PathBuf}, rc::Rc};

use asca::rule::RuleGroup;
use colored::Colorize;

use super::super::{parse::parse_rsca, seq::{ASCAConfig, Entry, RuleFilter}, util};
use super::lexer::{Position, Token, TokenKind};

pub(crate) struct Parser<'a> {
    token_list: Vec<Token>,
    pos: usize,
    curr_tkn: Token,
    path: &'a Path,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(lst: Vec<Token>, path: &'a Path) -> Self {
        let mut s = Self { 
            token_list: lst, 
            pos: 0, 
            curr_tkn: Token { kind: TokenKind::Eof, value: Rc::default(), position: Position::new(0, 0, 0, 1 ) },
            path,
        };
        s.curr_tkn = s.token_list[s.pos].clone();

        s
    }

    fn has_more_tokens(&self) -> bool { self.pos < self.token_list.len() }

    fn advance(&mut self) {
        self.pos += 1;
        self.curr_tkn = if self.has_more_tokens() {
            self.token_list[self.pos].clone()
        } else {
            let last_pos = self.token_list.last().unwrap().position;
            Token { kind: TokenKind::Eof, value: Rc::default(), position: Position::new(last_pos.s_line, last_pos.s_pos, last_pos.e_line, last_pos.e_line+1) }
        };

        if self.curr_tkn.kind == TokenKind::Comment {
            self.advance();
        }
    }

    fn peek(&self, knd: TokenKind) -> bool { self.curr_tkn.kind == knd }

    #[allow(dead_code)]
    fn peek_next(&self, knd: TokenKind) -> bool { self.token_list[self.pos+1].kind == knd }

    fn expect(&mut self, knd: TokenKind) -> bool {
        if self.curr_tkn.kind == knd {
            self.advance();
            true
        } else {
            false
        }
    }

    fn eat(&mut self) -> Token {
        let token = self.curr_tkn.clone();
        self.advance();
        token
    }

    fn eat_expect(&mut self, knd: TokenKind) -> Option<Token> {
        if self.peek(knd) {
            Some(self.eat())
        } else {
            None
        }
    }

    fn skip_comments(&mut self) {
        // self.advance is recursive so we only have to check once
        if self.peek(TokenKind::Comment) {
            self.advance();
        }
    }

    fn error(&self, message: String) -> io::Error {
        io::Error::other(format!("{}: {}", "Config Parse Error".bright_red(), message))
    }

    fn line_lookahead(&self, knd: TokenKind) -> bool {
        let mut pos = self.pos;
        while pos < self.token_list.len() && self.token_list[pos].kind != TokenKind::Eol && self.token_list[pos].kind != TokenKind::Eof {
            if self.token_list[pos].kind == knd {
                return true
            }
            pos += 1;
        }
        false
    }

    fn get_ident(&mut self) -> io::Result<(Vec<Rc<str>>, Token)> {
        let mut has_arrow = false;

        let mut token_list = Vec::new();
        let mut len = 0;

        loop {
            if let Some(colon) = self.eat_expect(TokenKind::Colon) {
                if token_list.is_empty() {
                    return Err(self.error(format!("Expected tag, but received ':' at '{:?}'", colon.position)))
                }
                break;
            }
            if self.eat_expect(TokenKind::Arrow).is_some() {
                if has_arrow {
                    // TODO: Better errors
                    return Err(self.error("Can't have multiple arrows".to_string()))
                }
                has_arrow = true;
                len = token_list.len();
                continue;
            }
            if let Some(lit) = self.eat_expect(TokenKind::Literal) {
                token_list.push(lit);
                continue;
            }

            if token_list.is_empty() {
                return Err(self.error(format!("Expected tag, but received '{}' at '{:?}'", self.curr_tkn.value, self.curr_tkn.position)))
            } else {
                return Err(self.error(format!("Unexpected token '{}' at '{:?}'", self.curr_tkn.value, self.curr_tkn.position)))
            }
        }

        if !has_arrow {
            match token_list.len().cmp(&1) {
                std::cmp::Ordering::Equal => return Ok((vec![], token_list[0].clone())),
                std::cmp::Ordering::Greater => return Err(self.error("Too many tags".to_string())),
                std::cmp::Ordering::Less => unreachable!(),
            }
        }

        if token_list.len() > len + 1 {
            return Err(self.error("Too many tags".to_string()))
        }

        let tag = token_list[len].clone();

        token_list.pop();

        let words = token_list.into_iter().map(|tkn| {
            tkn.value
        }).collect();
        
        Ok((words, tag))

    }

    fn get_entries(&mut self) -> io::Result<Vec<Entry>>  {
        let mut entries = Vec::new();

        if let Some(e) =  self.get_entry()? {
            entries.push(e);

            while self.has_more_tokens() {
                match self.get_entry()? {
                    Some(e) => entries.push(e),
                    None =>  break,
                }
            }
        }

        Ok(entries)
    }

    fn get_verbatim(&self, rule: &Token, filter: &Option<RuleFilter>) -> String {
        let mut rule_str = rule.value.to_string();

        if let Some(f) = filter {
            match f {
                // ~
                RuleFilter::Only(s) => {
                    rule_str.push_str(" ~ ");
                    rule_str.push_str(s);
                },
                RuleFilter::OnlyMult(items) => {
                    rule_str.push_str(" ~ ");
                    rule_str.push_str(&items[0]);

                    for s in items.iter().skip(1) {
                        rule_str.push_str(", ");
                        rule_str.push_str(s);
                    }
                },
                // !
                RuleFilter::Without(s) => {
                    rule_str.push_str(" ! ");
                    rule_str.push_str(s);
                },
                RuleFilter::WithoutMult(items) => {
                    rule_str.push_str(" ! ");
                    rule_str.push_str(&items[0]);

                    for s in items.iter().skip(1) {
                        rule_str.push_str(", ");
                        rule_str.push_str(s);
                    }
                },
            }
        }
        rule_str
    }

    fn get_entry(&mut self) -> io::Result<Option<Entry>> {
        if !self.expect(TokenKind::Eol) {
            return Ok(None)
        }
        if self.line_lookahead(TokenKind::Colon) {
            return Ok(None)
        }
        let Some(rule) = self.eat_expect(TokenKind::Literal) else {
            return Ok(None)
        };

        let filter = self.get_filter()?;

        let verbatim = self.get_verbatim(&rule, &filter);

        let mut file_path = self.path.to_path_buf();
        let rule_file = rule.value.trim();
        file_path.set_file_name(rule_file);

        if file_path.is_file(){
            let entry_rules = parse_rsca(&file_path)?;
            match filter {
                Some(rf) => Ok(Some(self.parse_entry(entry_rules, rf, verbatim, file_path, PathBuf::from(rule_file))?)),
                None => Ok(Some(Entry::from(file_path, verbatim, entry_rules))),
            }
        } else {
            Err(self.error(format!("Cannot find {file_path:?}. {}:{}", rule.position.s_line, rule.position.s_pos)))
        } 
    }

    fn get_filter(&mut self) -> io::Result<Option<RuleFilter>> {
        match self.curr_tkn.kind {
            TokenKind::Bang => {
                self.advance();
                let pos = self.curr_tkn.position;
                let list = self.get_filter_list()?;
                match list.len().cmp(&1) {
                    std::cmp::Ordering::Greater => Ok(Some(RuleFilter::WithoutMult(list))),
                    std::cmp::Ordering::Equal => Ok(Some(RuleFilter::Without(list[0].clone()))),
                    std::cmp::Ordering::Less => Err(self.error(format!("Empty filter list at {}:{}", pos.s_line, pos.s_pos))),
                }
            },
            TokenKind::Tilde => {
                self.advance();
                let pos = self.curr_tkn.position;
                let list = self.get_filter_list()?;
                match list.len().cmp(&1) {
                    std::cmp::Ordering::Greater => Ok(Some(RuleFilter::OnlyMult(list))),
                    std::cmp::Ordering::Equal => Ok(Some(RuleFilter::Only(list[0].clone()))),
                    std::cmp::Ordering::Less => Err(self.error(format!("Empty filter list at {}:{}", pos.s_line, pos.s_pos))),
                }
            },
            _ => Ok(None)
        }
    }

    fn get_filter_list(&mut self) -> io::Result<Vec<String>>{
        let mut filters = Vec::new();
        let mut phrase = Vec::new();

        if let Some(f) =  self.eat_expect(TokenKind::Literal) {
            phrase.push(f.value.to_lowercase());

            loop {
                if self.peek(TokenKind::Eol) {
                    let x = phrase.join(" ");
                    filters.push(x);
                    break;
                }
                if self.expect(TokenKind::Comma) {
                    let x = phrase.join(" ");
                    filters.push(x);
                    phrase.clear();
                }

                match self.eat_expect(TokenKind::Literal) {
                    Some(f) => phrase.push(f.value.to_lowercase()),
                    None => if self.peek(TokenKind::Eol) || self.peek(TokenKind::Eof) { 
                        let x = phrase.join(" ");
                        filters.push(x);
                        break;
                    } else {
                        return Err(self.error(format!("Expected a rule name, found {} at {}:{}", self.curr_tkn.kind, self.curr_tkn.position.s_line, self.curr_tkn.position.s_pos)))
                    },
                }
            }
        }

        if filters.is_empty() {
            return Err(self.error(format!("Expected a rule name, found {} at {}:{}", self.curr_tkn.kind, self.curr_tkn.position.s_line, self.curr_tkn.position.s_pos)))
        }
        Ok(filters)
    }

    fn parse_entry(&mut self, entry_rules: Vec<RuleGroup>, filter: RuleFilter, verbatim: String, file_path: PathBuf, rule_file: PathBuf) -> io::Result<Entry> {
        let mut file_path = file_path.clone();
        let file_stem = rule_file.file_stem().expect("File exists").to_str().unwrap();
        match filter {
            RuleFilter::Only(rule_str) => {
                match entry_rules.iter().find(|r| r.name.to_lowercase() == rule_str.to_lowercase()).cloned() {
                    Some(rule) => {
                        file_path.set_file_name(format!("{}_o_{}", file_stem, util::sanitise_str(&rule_str)));
                        Ok(Entry::from(file_path, verbatim, vec![rule]))
                    },
                    None => Err(self.error(format!("Could not find rule '{}' in '{}'.\nMake sure the rule name matches exactly!", rule_str, rule_file.to_str().unwrap()))),
                }
            },
            RuleFilter::Without(rule_str) => {
                let before_len = entry_rules.len();
                let entries = entry_rules.iter().filter(|r| r.name.to_lowercase() != rule_str.to_lowercase()).cloned().collect::<Vec<_>>();
                if entries.len() == before_len {
                    return Err(self.error(format!("Could not find rule '{}' in '{}'.\nMake sure the rule name matches exactly!", rule_str, rule_file.to_str().unwrap())))
                }
                file_path.set_file_name(format!("{}_x_{}", file_stem, util::sanitise_str(&rule_str)));
                Ok(Entry::from(file_path, verbatim, entries))
            },
            RuleFilter::OnlyMult(filters) => {
                let mut entries = Vec::new();
                for filter in &filters {
                    match entry_rules.iter().find(|r| r.name.to_lowercase() == filter.to_lowercase()) {
                        Some(entry) => entries.push(entry.clone()),
                        None => return Err(self.error(format!("Could not find rule '{}' in '{}'.\nMake sure the rule name matches exactly!", filter, rule_file.to_str().unwrap()))),
                    }
                }
                let abv_filt = filters.iter().filter_map(|s| s.chars().next()).collect::<String>();
                file_path.set_file_name(format!("{}_om_{}", file_stem, util::sanitise_str(&abv_filt)));
                Ok(Entry::from(file_path, verbatim, entries))
            },
            RuleFilter::WithoutMult(filters) => {
                let before_len = entry_rules.len();
                let entries = entry_rules.iter().filter(|r| !filters.contains(&r.name.to_lowercase())).cloned().collect::<Vec<_>>();
                if entries.len() == before_len {
                    return Err(self.error(format!("Could not find any of the excluded rules in '{}'.\nMake sure the rule names match exactly!", rule_file.to_str().unwrap())))
                }
                let abv_filt = filters.iter().filter_map(|s| s.chars().next()).collect::<String>();
                file_path.set_file_name(format!("{}_xm_{}", file_stem, util::sanitise_str(&abv_filt)));
                Ok(Entry::from(file_path, verbatim, entries))
            },
        }
    }

    fn get_seq(&mut self) -> io::Result<ASCAConfig> {
        self.skip_comments();

        while self.eat_expect(TokenKind::Eol).is_some() { }

        let (words, tag_token) = self.get_ident()?;

        let tag = tag_token.value;
        let entries = self.get_entries()?;

        Ok(ASCAConfig { tag, from: None, alias: None, words, entries })
    }

    pub(crate) fn parse(&mut self) -> io::Result<Vec<ASCAConfig>> {
        let mut tag_set = HashSet::new();
        let mut conf = Vec::new();

        while self.curr_tkn.kind != TokenKind::Eof {
            let seq = self.get_seq()?;

            if !tag_set.insert(seq.tag.clone()) {
                return Err(self.error(format!("tag '{}' declared more than once in config", seq.tag)))
            }

            conf.push(seq);
        }

        // Validate config tags

        // Check all pipeline tags exist
        for c in &conf {
            if let Some(from_tag) = &c.from {
                if !tag_set.contains(from_tag) {
                    return Err(self.error(format!("tag '{}' does not exist", from_tag)))
                }
            }
        }
        // Check for loops
        for pipe in conf.iter().filter(|c| c.from.is_some()).collect::<Vec<_>>() {
            if self.detect_tag_loop(&conf, pipe) {
                return Err(self.error(format!("infinite pipeline loop detected in tag '{}'", pipe.tag)))
            }
        }

        Ok(conf)
    }

    fn detect_tag_loop(&self, conf: &[ASCAConfig], head: &ASCAConfig) -> bool {
        let mut set: HashSet<String> = HashSet::new();
        let mut head = head;

        while let Some(from) = &head.from {
            if !set.insert(from.to_string()) {
                return true
            }
            head = conf.iter().find(|c| c.tag == *from).unwrap()
        }
        false
    }
}

#[cfg(test)]
mod parser_tests {

    use super::*;
    use super::super::lexer::{Lexer, Token};

    fn setup(test_str: &str) -> Vec<Token> {
        match Lexer::new(&String::from(test_str).chars().collect::<Vec<_>>()).tokenise() {
            Ok(tkns) => tkns,
            Err(e) => {
                println!("{}", e.to_string());
                assert!(false);
                unreachable!()
            },
        }
    }

    #[test]
    fn test_with_words() {
        let test_input= String::from(
            "foo.wsca bar.wsca > beta:\n \
                examples/indo-european/germanic/pgmc/pre.rsca ! Laryngeal colouring\n \
                examples/indo-european/germanic/pgmc/pre.rsca ~ Dental Cluster Simplification, Cowgill's Law"
            );
        let maybe_result = Parser:: new(setup(&test_input), Path::new("")).parse();

        assert!(maybe_result.is_ok());

        let result = maybe_result.unwrap();

        assert_eq!(result[0].tag, "beta".into());
        assert_eq!(result[0].from, None);
        assert_eq!(result[0].alias, None);
        assert_eq!(result[0].words, ["foo.wsca".into(), "bar.wsca".into()]);
        assert_eq!(result[0].entries[0].name, PathBuf::from("examples/indo-european/germanic/pgmc/pre_x_laryngeal-colouring"));
        assert_eq!(result[0].entries[1].name, PathBuf::from("examples/indo-european/germanic/pgmc/pre_om_dc"));
    }

    #[test]
    fn test_without_words() {
        let test_input= String::from(
            "beta:\n \
                examples/indo-european/germanic/pgmc/pre.rsca ! Laryngeal colouring\n \
                examples/indo-european/germanic/pgmc/pre.rsca ~ Dental Cluster Simplification, Cowgill's Law"
            );
        let maybe_result = Parser:: new(setup(&test_input), Path::new("")).parse();

        assert!(maybe_result.is_ok());

        let result = maybe_result.unwrap();

        assert_eq!(result[0].tag, "beta".into());
        assert_eq!(result[0].from, None);
        assert_eq!(result[0].alias, None);
        assert_eq!(result[0].words, []);
        assert_eq!(result[0].entries[0].name, PathBuf::from("examples/indo-european/germanic/pgmc/pre_x_laryngeal-colouring"));
        assert_eq!(result[0].entries[1].name, PathBuf::from("examples/indo-european/germanic/pgmc/pre_om_dc"));

    }

    #[test]
    fn test_mult() {
        let test_input= String::from(
            "alpha:\n \
                examples/indo-european/germanic/pgmc/pre.rsca ! Laryngeal colouring\n \
                examples/indo-european/germanic/pgmc/pre.rsca ~ Dental Cluster Simplification, Cowgill's Law

            foo.wsca bar.wsca > beta:\n \
                examples/indo-european/germanic/pgmc/pre.rsca ! Laryngeal colouring\n \
                examples/indo-european/germanic/pgmc/pre.rsca ~ Cowgill's Law"
            );
        let maybe_result = Parser:: new(setup(&test_input), Path::new("")).parse();

        assert!(maybe_result.is_ok());

        let result = maybe_result.unwrap();

        assert_eq!(result[0].tag, "alpha".into());
        assert_eq!(result[0].from, None);
        assert_eq!(result[0].alias, None);
        assert_eq!(result[0].words, []);
        assert_eq!(result[0].entries[0].name, PathBuf::from("examples/indo-european/germanic/pgmc/pre_x_laryngeal-colouring"));
        assert_eq!(result[0].entries[1].name, PathBuf::from("examples/indo-european/germanic/pgmc/pre_om_dc"));

        assert_eq!(result[1].tag, "beta".into());
        assert_eq!(result[1].from, None);
        assert_eq!(result[1].alias, None);
        assert_eq!(result[1].words, ["foo.wsca".into(), "bar.wsca".into()]);
        assert_eq!(result[1].entries[0].name, PathBuf::from("examples/indo-european/germanic/pgmc/pre_x_laryngeal-colouring"));
        assert_eq!(result[1].entries[1].name, PathBuf::from("examples/indo-european/germanic/pgmc/pre_o_cowgills-law"));
    }
}