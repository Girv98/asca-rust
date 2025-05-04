use std::{collections::HashSet, io, path::{Path, PathBuf}, rc::Rc};

use asca::rule::RuleGroup;
use colored::Colorize;

use super::super::util;
use super::super::seq::{ASCAConfig, Entry, InputKind, RuleFilter};
use super::super::parse::{parse_alias, parse_rsca, parse_wsca};
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
        while pos < self.token_list.len() && self.token_list[pos].kind != TokenKind::Eol && self.token_list[pos].kind != TokenKind::Eof && self.token_list[pos].kind != TokenKind::Comment {
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
                    return Err(self.error(format!("Expected tag, but received ':' at line {}:{}", colon.position.s_line, colon.position.s_pos)))
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
                return Err(self.error(format!("Expected tag, but received '{}' at line {}:{}", self.curr_tkn.value, self.curr_tkn.position.s_line, self.curr_tkn.position.s_pos)))
            } else if self.curr_tkn.kind == TokenKind::Eof {
                return Err(self.error(format!("Unexpected end of file at line {}:{}", self.curr_tkn.position.s_line, self.curr_tkn.position.s_pos)))
            } else {
                return Err(self.error(format!("Unexpected token '{}' at line {}:{}", self.curr_tkn.kind, self.curr_tkn.position.s_line, self.curr_tkn.position.s_pos)))
            }
        }

        if !has_arrow {
            match token_list.len().cmp(&1) {
                std::cmp::Ordering::Equal => return Ok((vec![], token_list[0].clone())),
                std::cmp::Ordering::Greater => return Err(self.error(format!(
                    "More than one tag found on line '{}:{}'", 
                    self.token_list.last().unwrap().position.s_line,
                    self.token_list.last().unwrap().position.s_pos
                ))),
                std::cmp::Ordering::Less => unreachable!(),
            }
        }

        if token_list.len() > len + 1 {
            return Err(self.error(format!(
                "More than one tag found on line '{}:{}'", 
                self.token_list.last().unwrap().position.s_line,
                self.token_list.last().unwrap().position.s_pos
            )))
        }

        let tag = token_list[len].clone();

        token_list.pop();

        let inputs = token_list.into_iter().map(|tkn| {
            tkn.value
        }).collect();

        self.skip_comments();
        while self.eat_expect(TokenKind::Eol).is_some() { }
        
        Ok((inputs, tag))

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

    fn get_entry(&mut self) -> io::Result<Option<Entry>> {
        if self.line_lookahead(TokenKind::Colon) {
            return Ok(None)
        }
        let Some(rule) = self.eat_expect(TokenKind::Literal) else {
            return Ok(None)
        };

        let filter = self.get_filter()?;

        self.skip_comments();
        while self.eat_expect(TokenKind::Eol).is_some() { }

        let mut file_path = self.path.to_path_buf();
        let rule_file = rule.value.trim();
        file_path.set_file_name(rule_file);

        if file_path.is_file(){
            let entry_rules = parse_rsca(&file_path)?;
            match filter {
                Some(rf) => Ok(Some(self.parse_entry(entry_rules, rf, file_path, PathBuf::from(rule_file))?)),
                None => Ok(Some(Entry::from(file_path, entry_rules))),
            }
        } else {
            Err(self.error(format!("Cannot find {file_path:?} at line {}:{}", rule.position.s_line, rule.position.s_pos)))
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
                    std::cmp::Ordering::Less => Err(self.error(format!("Empty filter list at line {}:{}", pos.s_line, pos.s_pos))),
                }
            },
            TokenKind::Tilde => {
                self.advance();
                let pos = self.curr_tkn.position;
                let list = self.get_filter_list()?;
                match list.len().cmp(&1) {
                    std::cmp::Ordering::Greater => Ok(Some(RuleFilter::OnlyMult(list))),
                    std::cmp::Ordering::Equal => Ok(Some(RuleFilter::Only(list[0].clone()))),
                    std::cmp::Ordering::Less => Err(self.error(format!("Empty filter list at line {}:{}", pos.s_line, pos.s_pos))),
                }
            },
            _ => Ok(None)
        }
    }

    fn get_filter_list(&mut self) -> io::Result<Vec<String>>{
        let mut filters = Vec::new();

        if let Some(f) =  self.eat_expect(TokenKind::String) {
            filters.push(f.value.to_string());

            loop {
                if self.peek(TokenKind::Eol) || self.peek(TokenKind::Eof) { break; }
                if !self.expect(TokenKind::Comma) {
                    let pos = self.curr_tkn.position;
                    return Err(self.error(format!("Expected comma, found {} at {}:{}", self.curr_tkn.kind, pos.s_line, pos.s_pos)))
                }

                match self.eat_expect(TokenKind::String) {
                    Some(f) => filters.push(f.value.to_string()),
                    None => if self.peek(TokenKind::Eol) || self.peek(TokenKind::Eof) { 
                        break;
                    } else {
                        return Err(self.error(format!("Expected a rule name, found {} at line {}:{}", self.curr_tkn.kind, self.curr_tkn.position.s_line, self.curr_tkn.position.s_pos)))
                    },
                }
            }
        }

        if filters.is_empty() {
            return Err(self.error(format!("Expected a rule name, found {} at line {}:{}", self.curr_tkn.kind, self.curr_tkn.position.s_line, self.curr_tkn.position.s_pos)))
        }
        Ok(filters)
    }

    fn parse_entry(&mut self, entry_rules: Vec<RuleGroup>, filter: RuleFilter, file_path: PathBuf, rule_file: PathBuf) -> io::Result<Entry> {
        let mut file_path = file_path.clone();
        let file_stem = rule_file.file_stem().expect("Caller insures file exists").to_str().unwrap();
        file_path.set_file_name(format!("{}{}", file_stem, filter.as_file_string()));

        match filter {
            RuleFilter::Only(rule_str) => {
                match entry_rules.iter().find(|r| r.name.to_lowercase() == rule_str.to_lowercase()).cloned() {
                    Some(rule) => Ok(Entry::from(file_path, vec![rule])),
                    None => Err(self.error(format!("Could not find rule '{}' in '{}'. Did you mean {}?", rule_str, rule_file.to_str().unwrap(), self.lev(entry_rules, &rule_str).yellow())))
                }
            },
            RuleFilter::Without(rule_str) => {
                let entries = entry_rules.iter().filter(|r| r.name.to_lowercase() != rule_str.to_lowercase()).cloned().collect::<Vec<_>>();
                if entries.len() == entry_rules.len() {
                    return Err(self.error(format!("Could not find rule '{}' in '{}'. Did you mean {}?", rule_str, rule_file.to_str().unwrap(), self.lev(entry_rules, &rule_str).yellow())))
                }
                Ok(Entry::from(file_path, entries))
            },
            RuleFilter::OnlyMult(filters) => {
                let entries = filters.iter().map(|filter| {
                    match entry_rules.iter().find(|r| r.name.to_lowercase() == filter.to_lowercase()) {
                        Some(entry) => Ok(entry.clone()),
                        None => Err(self.error(format!("Could not find rule '{}' in '{}'. Did you mean {}?", filter, rule_file.to_str().unwrap(), self.lev(entry_rules.clone(), filter).yellow())))
                    }
                }).collect::<io::Result<Vec<RuleGroup>>>()?;

                Ok(Entry::from(file_path, entries))
            },
            RuleFilter::WithoutMult(mut filters) => {
                // TODO: which rules were not found
                filters = filters.iter().map(|f| f.to_lowercase()).collect();
                let entries = entry_rules.iter().filter(|r| !filters.contains(&r.name.to_lowercase())).cloned().collect::<Vec<_>>();
                if entries.len() == entry_rules.len() {
                    return Err(self.error(format!("Could not find any of the excluded rules in '{}'.\nMake sure the rule names match exactly!", rule_file.to_str().unwrap())))
                } else if entries.len() != entry_rules.len() - filters.len() {
                    return Err(self.error(format!("Could not find one or more of the excluded rules in '{}'.\nMake sure the rule names match exactly!", rule_file.to_str().unwrap())))
                }
                Ok(Entry::from(file_path, entries))
            },
        }
    }

    fn lev(&self, entry_rules: Vec<RuleGroup>, filter: &str) -> String {
        let mut best_lev = usize::MAX;
        let mut best_match= "";
        let names = entry_rules.iter().map(|e| &e.name);

        for name in names {
            let len = util::lev(name, filter);
            if len < best_lev {
                best_match = name;
                best_lev = len;
            }
        }
        
        best_match.to_string()
    }

    fn parse_inputs(&self, inputs: &[Rc<str>], tag: &Rc<str>) -> io::Result<(Option<Rc<str>>, Vec<InputKind>)> {
        let mut alias = None;
        let mut parsed_input = Vec::new();

        for item in inputs {
            let mut file_path = self.path.to_path_buf();
            let item_file = item.trim();
            file_path.set_file_name(item_file);

            if file_path.is_file() {
                if let Some(ext) = file_path.extension() {
                    match ext.to_str() {
                        Some("wsca") => { parsed_input.push(InputKind::WordFile(item.clone())); continue; },
                        Some("alias") => {
                            if alias.is_some() {
                                return Err(self.error(format!("A sequence can only have one alias file. seq '{}' @ '{}'", tag.yellow(), item.yellow())))
                            }
                            alias = Some(item.clone());
                            continue;
                        },
                        _ => {}
                    }
                } 
                // determine if wsca or alias by trying to parse as both
                // TODO: atm, it is possible for a none word file to parse without erroring, but yield no correct input
                // we are also throwing away the parsed result which isn't ideal
                let maybe_alias = parse_alias(&file_path);
                
                if maybe_alias.is_ok() {
                    if alias.is_some() {
                        return Err(self.error(format!("A sequence can only have one alias file. seq '{}' @ '{}'", tag.yellow(), item.yellow())))
                    }
                    alias = Some(item.clone());
                    continue;
                }

                match parse_wsca(&file_path) {
                    Ok(_) => parsed_input.push(InputKind::WordFile(item.clone())),
                    Err(e) => {
                        eprintln!("{} could not confirm that '{}' as an input to the sequence '{}' was to be a word or alias file due to one or more errors while parsing:", "asca:".bright_red(), item_file.yellow(), tag.yellow());
                        return Err(e)
                    },
                }
            } else if file_path.extension().is_some() {
                return Err(self.error(format!("File {} could not be found. seq '{}' @ '{}'", format!("{:?}", file_path).yellow(), tag.yellow(), item.yellow())))
            } else {
                // assume it is a piped tag
                parsed_input.push(InputKind::FromTag(item.clone()));
            } 
        }

        Ok((alias, parsed_input))
    }

    fn get_seq(&mut self) -> io::Result<Option<ASCAConfig>> {
        self.skip_comments();

        while self.eat_expect(TokenKind::Eol).is_some() { }

        if self.curr_tkn.kind == TokenKind::Eof {
            return Ok(None)
        }

        let (input, tag_token) = self.get_ident()?;

        let tag = tag_token.value;
        let (alias, input) = self.parse_inputs(&input, &tag)?;
        
        let entries = self.get_entries()?;

        Ok(Some(ASCAConfig { tag, alias, input, entries }))
    }

    pub(crate) fn parse(&mut self) -> io::Result<Vec<ASCAConfig>> {
        let mut tag_set = HashSet::new();
        let mut conf = Vec::new();

        while self.curr_tkn.kind != TokenKind::Eof {
            let Some(seq ) = self.get_seq()? else { break };

            if !tag_set.insert(seq.tag.clone()) {
                return Err(self.error(format!("tag '{}' declared more than once in config", seq.tag)))
            }

            conf.push(seq);
        }

        // Validate config tags

        // Check all pipeline tags exist
        for c in &conf {
            for from in c.get_from_tags() {
                if !tag_set.contains(&from) {
                    return Err(self.error(format!("piped tag '{}' found in sequence '{}' does not exist", from.yellow(), c.tag.yellow())))
                }
            }
        }
        // Check for loops
        for pipe in conf.iter().filter(|c| !c.get_from_tags().is_empty()).collect::<Vec<_>>() {
            if self.detect_tag_loop(&conf, pipe) {
                return Err(self.error(format!("infinite pipeline loop detected in tag '{}'", pipe.tag)))
            }
        }

        Ok(conf)
    }

    // Inefficient but works
    fn has_loop_rec(conf: &[ASCAConfig], set: HashSet<Rc<str>>, from: Rc<str>) -> bool {
        let mut set = set;
        let head = conf.iter().find(|c| c.tag == from).unwrap();

        if !set.insert(from.clone()) {
            return true
        }

        for from in head.get_from_tags() {
            if from == head.tag || Self::has_loop_rec(conf, set.clone(), from.clone()) {
                return true
            }
        }

        false
    }

    fn detect_tag_loop(&self, conf: &[ASCAConfig], head: &ASCAConfig) -> bool {
        for from in head.get_from_tags() {
            if from == head.tag || Self::has_loop_rec(conf, HashSet::new(), from.clone()) {
                return true
            }
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
    fn test_tag_loop() {
        let test_input= String::from(
            "beta > alpha:\n \
                examples/indo-european/germanic/pgmc/pre.rsca

            alpha > beta:\n \
                examples/indo-european/germanic/pgmc/pre.rsca ! Laryngeal colouring\n \
                examples/indo-european/germanic/pgmc/pre.rsca ~ Cowgill's Law"
            );
        let maybe_result = Parser:: new(setup(&test_input), Path::new("")).parse();
        // println!("{:?}", maybe_result);
        assert!(maybe_result.is_err());
    }

    #[test]
    fn test_from_tag_does_not_exist() {
        let test_input= String::from(
            "beta > alpha:\n \
                examples/indo-european/germanic/pgmc/pre.rsca

            gamma > beta:\n \
                examples/indo-european/germanic/pgmc/pre.rsca ! Laryngeal colouring\n \
                examples/indo-european/germanic/pgmc/pre.rsca ~ Cowgill's Law"
            );
        let maybe_result = Parser:: new(setup(&test_input), Path::new("")).parse();
        // println!("{:?}", maybe_result);
        assert!(maybe_result.is_err());
    }

    #[test]
    fn test_with_words() {
        let test_input= String::from(
            "examples/indo-european/pie-uvular-common.wsca examples/indo-european/pie-pronouns.wsca > beta:\n \
                examples/indo-european/germanic/pgmc/pre.rsca ! Laryngeal colouring\n \
                examples/indo-european/germanic/pgmc/pre.rsca ~ Dental Cluster Simplification, Cowgill's Law"
            );
        let maybe_result = Parser:: new(setup(&test_input), Path::new("")).parse();

        assert!(maybe_result.is_ok());

        let result = maybe_result.unwrap();

        assert_eq!(result[0].tag, "beta".into());
        assert_eq!(result[0].alias, None);
        assert_eq!(result[0].input, [
            InputKind::WordFile("examples/indo-european/pie-uvular-common.wsca".into()), 
            InputKind::WordFile("examples/indo-european/pie-pronouns.wsca".into())
        ]);
        assert_eq!(result[0].entries[0].name, PathBuf::from("examples/indo-european/germanic/pgmc/pre_excl_laryngeal-colouring"));
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
        assert_eq!(result[0].alias, None);
        assert_eq!(result[0].input, []);
        assert_eq!(result[0].entries[0].name, PathBuf::from("examples/indo-european/germanic/pgmc/pre_excl_laryngeal-colouring"));
        assert_eq!(result[0].entries[1].name, PathBuf::from("examples/indo-european/germanic/pgmc/pre_om_dc"));

    }

    #[test]
    fn test_mult() {
        let test_input= String::from(
            "alpha:\n \
                examples/indo-european/germanic/pgmc/pre.rsca ! Laryngeal colouring\n \
                examples/indo-european/germanic/pgmc/pre.rsca ~ Dental Cluster Simplification, Cowgill's Law

            examples/indo-european/pie-uvular-common.wsca examples/indo-european/pie-pronouns.wsca > beta:\n \
                examples/indo-european/germanic/pgmc/pre.rsca ! Laryngeal colouring\n \
                examples/indo-european/germanic/pgmc/pre.rsca ~ Cowgill's Law"
            );
        let maybe_result = Parser:: new(setup(&test_input), Path::new("")).parse();

        assert!(maybe_result.is_ok());

        let result = maybe_result.unwrap();

        assert_eq!(result[0].tag, "alpha".into());
        assert_eq!(result[0].alias, None);
        assert_eq!(result[0].input, []);
        assert_eq!(result[0].entries[0].name, PathBuf::from("examples/indo-european/germanic/pgmc/pre_excl_laryngeal-colouring"));
        assert_eq!(result[0].entries[1].name, PathBuf::from("examples/indo-european/germanic/pgmc/pre_om_dc"));

        assert_eq!(result[1].tag, "beta".into());
        assert_eq!(result[1].alias, None);
        assert_eq!(result[1].input, [
            InputKind::WordFile("examples/indo-european/pie-uvular-common.wsca".into()), 
            InputKind::WordFile("examples/indo-european/pie-pronouns.wsca".into())
        ]);
        assert_eq!(result[1].entries[0].name, PathBuf::from("examples/indo-european/germanic/pgmc/pre_excl_laryngeal-colouring"));
        assert_eq!(result[1].entries[1].name, PathBuf::from("examples/indo-european/germanic/pgmc/pre_only_cowgills-law"));
    }
}