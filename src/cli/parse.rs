use std::{io, path::Path};

use asca::rule::RuleGroup;
use colored::Colorize;

use super::util;

// TODO: We can do better
pub fn parse_rsca(path: &Path) -> io::Result<Vec<RuleGroup>> {
    let mut rules = Vec::new();
    let mut r = RuleGroup::new();
    for line in util::file_read(path)?.lines() {
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
        if line.starts_with('#') && !line.starts_with("##") {
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

// TODO: We can do better
pub fn parse_alias(path: &Path) -> io::Result<(Vec<String>, Vec<String>)> {
    let mut into = vec![];
    let mut from = vec![];

    // Some(true) = into, Some(false) = from
    let mut state: Option<bool> = None;

    for (ln, line) in util::file_read(path)?.lines().enumerate() {
        let line = line.trim();

        if line.starts_with("@into") {
            state = Some(true);
            continue;
        }
        if line.starts_with("@from") {
            state = Some(false);
            continue;
        }
        // comments
        if line.starts_with("#") {
            continue;
        }

        match state {
            Some(state_into) => if state_into {
                into.push(line.to_string());
            } else {
                from.push(line.to_string());
            },
            None => return Err(io::Error::other(format!("{} Potentially unescaped comment at line {}", "Alias Parse Error:".bright_red(), ln+1))),
        }
    }

    if into.is_empty() && from.is_empty() {
        return Err(io::Error::other(format!(
            "{} Could not parse {} as an alias file:\n Could not find an {} or {}", 
            "Alias Parse Error: ".bright_red(), format!("{path:?}").yellow(), "@into".yellow(), "@from".yellow()
    )))
    }

    Ok((into, from))
}


pub fn parse_wsca(path: &Path) -> io::Result<(Vec<String>, Vec<String>)> {
    Ok(util::file_read(path)?.lines().map(|line| {
        let mut line_iter = line.trim().split('#');

        let word = line_iter.next().unwrap().trim().to_owned();
        let comment = line_iter.collect::<String>().trim().to_owned();
        (word, comment)
    }).collect::<(Vec<String>, Vec<String>)>())
}