use std::{collections::HashSet, io, path::{Path, PathBuf}, rc::Rc};

use asca::rule::RuleGroup;
use colored::Colorize;

use super::super::{parse::parse_rsca, seq::{ASCAConfig, Entry, RuleFilter}, util::{self, RULE_FILE_EXT}};
use super::lexer_old::{Position, Token, TokenKind};

pub(crate) struct OldParser<'a> {
    token_list: Vec<Token>,
    pos: usize,
    curr_tkn: Token,
    path: &'a Path
}

impl<'a> OldParser<'a> {
    pub(crate) fn new(lst: Vec<Token>, path: &'a Path) -> Self {
        let mut s = Self { 
            token_list: lst, 
            pos: 0, 
            curr_tkn: Token { kind: TokenKind::EoF, value: Rc::default(), position: Position::new(0, 0, 0, 1 ) },
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
            Token { kind: TokenKind::EoF, value: Rc::default(), position: Position::new(last_pos.s_line, last_pos.s_pos, last_pos.e_line, last_pos.e_line+1) }
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
        let message = format!("{}: {}", "Config Error".bright_red(), message);
        
        io::Error::other(message)
    }

    fn get_verbatim(&self, rule: &Token, filter: &Option<RuleFilter>) -> String {
        let mut rule_path = PathBuf::from(&rule.value.to_string());

        if rule_path.extension().is_none() {
            rule_path.set_extension(RULE_FILE_EXT);
        }

        let mut rule_str = rule_path.to_str().expect("File string is valid UTF-8").to_string();

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

    fn get_filter_list(&mut self) -> io::Result<Vec<String>> {
        if !self.expect(TokenKind::LeftCurly) {
            return Err(self.error(format!("Expected '{{', found {} at {}:{}", self.curr_tkn.kind, self.curr_tkn.position.s_line, self.curr_tkn.position.s_pos)))
        }

        let mut filters = Vec::new();

        if let Some(f) =  self.eat_expect(TokenKind::String) {
            filters.push(f.value.to_string());

            loop {
                if self.expect(TokenKind::RightCurly) { break; }
                if !self.expect(TokenKind::Comma)     { 
                    let pos = self.curr_tkn.position;
                    return Err(self.error(format!("Expected comma, found {} at {}:{}", self.curr_tkn.kind, pos.s_line, pos.s_pos)))
                }

                match self.eat_expect(TokenKind::String) {
                    Some(f) => filters.push(f.value.to_string()),
                    None => if self.expect(TokenKind::RightCurly) { 
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

    fn parse_entry(&mut self, entry_rules: Vec<RuleGroup>, filter: RuleFilter, verbatim: String, file_path: PathBuf, rule_file: PathBuf) -> io::Result<Entry> {
        let mut file_path = file_path.clone();
        let file_stem = rule_file.file_stem().expect("Caller insures file exists").to_str().unwrap();
        file_path.set_file_name(format!("{}{}", file_stem, filter.as_file_string()));

        match filter {
            RuleFilter::Only(rule_str) => {
                match entry_rules.iter().find(|r| r.name.to_lowercase() == rule_str.to_lowercase()).cloned() {
                    Some(rule) => Ok(Entry::from(file_path, verbatim, vec![rule])),
                    None => Err(self.error(format!("Could not find rule '{}' in '{}'. Did you mean {}?", rule_str, rule_file.to_str().unwrap(), self.lev(entry_rules, &rule_str).yellow())))
                }
            },
            RuleFilter::Without(rule_str) => {
                let entries = entry_rules.iter().filter(|r| r.name.to_lowercase() != rule_str.to_lowercase()).cloned().collect::<Vec<_>>();
                if entries.len() == entry_rules.len() {
                    return Err(self.error(format!("Could not find rule '{}' in '{}'. Did you mean {}?", rule_str, rule_file.to_str().unwrap(), self.lev(entry_rules, &rule_str).yellow())))
                }
                Ok(Entry::from(file_path, verbatim, entries))
            },
            RuleFilter::OnlyMult(filters) => {
                let entries = filters.iter().map(|filter| {
                    match entry_rules.iter().find(|r| r.name.to_lowercase() == filter.to_lowercase()) {
                        Some(entry) => Ok(entry.clone()),
                        None => Err(self.error(format!("Could not find rule '{}' in '{}'. Did you mean {}?", filter, rule_file.to_str().unwrap(), self.lev(entry_rules.clone(), filter).yellow())))
                    }
                }).collect::<io::Result<Vec<RuleGroup>>>()?;

                Ok(Entry::from(file_path, verbatim, entries))
            },
            RuleFilter::WithoutMult(mut filters) => {
                // TODO: which rules was not found
                filters = filters.iter().map(|f| f.to_lowercase()).collect();
                let entries = entry_rules.iter().filter(|r| !filters.contains(&r.name.to_lowercase())).cloned().collect::<Vec<_>>();
                if entries.len() == entry_rules.len() {
                    return Err(self.error(format!("Could not find any of the excluded rules in '{}'.\nMake sure the rule names match exactly!", rule_file.to_str().unwrap())))
                } else if entries.len() != entry_rules.len() - filters.len() {
                    return Err(self.error(format!("Could not find one or more of the excluded rules in '{}'.\nMake sure the rule names match exactly!", rule_file.to_str().unwrap())))
                }
                Ok(Entry::from(file_path, verbatim, entries))
            },
        }
    }

    fn get_entry(&mut self) -> io::Result<Option<Entry>> {
        let Some(rule) = self.eat_expect(TokenKind::String) else {
            return Ok(None)
        };

        let rule_file = rule.value.trim();
        let mut file_path = self.path.to_path_buf();
        file_path.set_file_name(rule_file);
        file_path.set_extension(RULE_FILE_EXT);
        let filter = self.get_filter()?;

        let verbatim = self.get_verbatim(&rule, &filter);

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

    fn get_entries(&mut self) -> io::Result<Vec<Entry>> {
        let mut entries = Vec::new();

        if let Some(e) =  self.get_entry()? {
            entries.push(e);
            
            while self.has_more_tokens() {
                if !self.expect(TokenKind::Comma) { break; }
                match self.get_entry()? {
                    Some(e) => entries.push(e),
                    None => break,
                }
            }
        }

        Ok(entries)
    }

    fn get_word_paths(&mut self) -> io::Result<Vec<Rc<str>>> {
        if !self.expect(TokenKind::LeftSquare) {
            return Ok(Vec::new())
        }

        let mut word_files = vec![];

        match self.eat_expect(TokenKind::String) {
            Some(w) => word_files.push(w.value),
            None => return Err(self.error(format!("Expected a file path, found {} at {}:{}", self.curr_tkn.kind, self.curr_tkn.position.s_line, self.curr_tkn.position.s_pos))),
        }

        while self.has_more_tokens() {
            if self.expect(TokenKind::RightSquare) { break; }
            if !self.expect(TokenKind::Comma)      { 
                let pos = self.curr_tkn.position;
                return Err(self.error(format!("Expected comma, found {} at {}:{}", self.curr_tkn.kind, pos.s_line, pos.s_pos)))
            }
            match self.eat_expect(TokenKind::String) {
                Some(w) => word_files.push(w.value),
                None => if self.expect(TokenKind::RightSquare) {
                    break;
                } else {
                    return Err(self.error(format!("Expected a file path, found {} at {}:{}", self.curr_tkn.kind, self.curr_tkn.position.s_line, self.curr_tkn.position.s_pos)))
                },
            }
        }

        Ok(word_files)
    }

    // fn get_from(&mut self) -> Option<Token> {
    //     self.eat_expect(TokenKind::From)
    // }

    fn get_tag(&mut self) -> io::Result<Token> {
        match self.eat_expect(TokenKind::Tag) {
            Some(tag) => Ok(tag),
            None => Err(self.error(format!("Expected a tag, found {} at {}:{}", self.curr_tkn.kind, self.curr_tkn.position.s_line, self.curr_tkn.position.s_pos))),
        }
    }

    fn get_seq(&mut self) -> io::Result<ASCAConfig> {

        self.skip_comments();

        let tag_token = self.get_tag()?;

        let tag = tag_token.value;

        let mut from = None;
        let mut alias = None;

        match self.curr_tkn.kind {
            TokenKind::From => from = Some(self.eat().value),
            TokenKind::Alias => alias = Some(self.eat().value),
            _ => {}
        }

        match self.curr_tkn.kind {
            TokenKind::From => if from.is_none() {
                 from = Some(self.eat().value)
            } else {
                return Err(self.error(format!("A sequence can only have one from tag {}:{}", self.curr_tkn.position.s_line, self.curr_tkn.position.s_pos)))
            },
            TokenKind::Alias => if alias.is_none() {
                alias = Some(self.eat().value)
            } else {
                return Err(self.error(format!("A sequence can only have one alias tag {}:{}", self.curr_tkn.position.s_line, self.curr_tkn.position.s_pos)))
            },
            _ => {}
        }

        let words = self.get_word_paths()?;

        if !self.expect(TokenKind::Colon) {
            if words.is_empty() {
                return Err(self.error(format!("Expected colon after tag at {}:{}", tag_token.position.e_line, tag_token.position.e_pos)))
            } else {
                let pos = self.token_list[self.pos-1].position;
                return Err(self.error(format!("Expected colon after words list at {}:{}", pos.e_line, pos.e_pos)))
            }
        }

        let entries = self.get_entries()?;

        Ok(ASCAConfig { tag, from, alias, words, entries })
    }

    pub(crate) fn parse(&mut self) -> io::Result<Vec<ASCAConfig>> {
        let mut tag_set = HashSet::new();
        let mut conf = Vec::new();

        while self.curr_tkn.kind != TokenKind::EoF {
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