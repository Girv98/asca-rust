use std::{collections::HashSet, io, path::{Path, PathBuf}};

use asca::RuleGroup;
use super::util::{self, RULE_FILE_ENDING};

#[derive(Debug)]
pub struct ASCAConfig {
    pub tag: String,
    pub words: Option<String>,
    pub entries: Vec<Entry>
}

impl ASCAConfig {
    fn new() -> Self {
        Self { tag: String::new(), words: None, entries: Vec::new() }
    }
}

#[derive(Debug)]
pub struct Entry {
    pub name: PathBuf,
    pub rules: Vec<RuleGroup>
}

impl Entry {
    pub fn from(name: PathBuf, rules: Vec<RuleGroup>) -> Self {
        Self { name, rules }
    }
}

pub fn parse_config(path: &Path) -> io::Result<Vec<ASCAConfig>> {
    let mut result: Vec<ASCAConfig> = vec![];
    let mut ac = ASCAConfig::new();
    let mut tag_map = HashSet::new();

    for line in util::file_read(path)?.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#')  {
            continue;
        }

        if line.starts_with('[') {
            if !ac.entries.is_empty() {
                if ac.tag.is_empty() {
                    println!("Warning: {path:?} Entries without a tag will be ignored");
                } else {
                    result.push(ac);
                }
            }
            ac = ASCAConfig::new();

            let mut chars = line.chars();
            chars.next();

            let mut tag = String::new();

            loop {
                match chars.next() {
                    Some(c) => match c {
                        ']' => break,
                        c => tag.push(c),
                    },
                    None => todo!(),
                }
            }

            tag = tag.trim().to_string();

            if !tag_map.insert(tag.clone()) {
                return Err(io::Error::other(format!("Parse Error: tag '{}' declared more than once in config", tag)))
            }
            ac.tag = tag;

            let rest = chars.as_str().trim().to_string();
            if !rest.is_empty() {
                ac.words = Some(rest)
            }
            continue;
        }

        let mut file_path = path.to_path_buf();
        file_path.set_file_name(line);
        file_path.set_extension(RULE_FILE_ENDING);

        if file_path.is_file() {
            let entry_rules = parse_rsca_file(&file_path)?;
            ac.entries.push(Entry::from(file_path, entry_rules));
        } else {
            return Err(io::Error::other(format!("Error: Cannot find {file_path:?}")))
        }
    }
    if ac.tag.is_empty() {
        println!("Warning: {path:?} Entries without a tag will be ignored");
    } else {
        result.push(ac);
    }

    Ok(result)
}

// TODO: We can do better
pub fn parse_rsca_file(rule_file_path: &Path) -> io::Result<Vec<RuleGroup>> {
    let mut rules = Vec::new();
    let mut r = RuleGroup::new();
    for line in util::file_read(rule_file_path)?.lines() {
        let line = line.trim();
        if line.starts_with('@') {
            if !r.is_empty() {
                rules.push(r);
            }
            r = RuleGroup::new();

            let mut chars = line.chars();
            chars.next();
            r.name = chars.as_str().trim().to_string();

            continue;
        }
        if line.starts_with('#') {
            let mut chars = line.chars();
            chars.next();
            if !r.description.is_empty() {
                r.description.push('\n');
            }
            r.description += chars.as_str().trim();
            continue;
        }

        if line.is_empty() {
            if !r.is_empty() && !r.description.is_empty() {
                rules.push(r);
                r = RuleGroup::new();
            }
            continue;
        }

        if r.description.is_empty() {
            r.rule.push(line.to_string());
            continue;
        } 

        rules.push(r);
        r = RuleGroup::new();
        r.rule.push(line.to_string());
    }
    rules.push(r);
    Ok(rules)
}


pub fn parse_wsca_file(path: &Path) -> io::Result<Vec<String>> {
    Ok(util::file_read(path)?.lines().map(|s| s.to_owned()).collect::<Vec<String>>())
}