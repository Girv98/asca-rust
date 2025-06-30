use std::{io, path::PathBuf};

use colored::Colorize;

use super::AscaJson;
use super::parse;
use super::seq::{get_all_rules, get_config, get_old_config, get_orig_alias_into, get_orig_words, get_words};
use super::util::{self, ALIAS_FILE_EXT, CONF_FILE_EXT, JSON_FILE_EXT, RULE_FILE_EXT, WORD_FILE_EXT};

/// Convert rsca/wsca/alias files into an asca-web json file
pub fn from_asca(words: Option<PathBuf>, rules: Option<PathBuf>, alias: Option<PathBuf>, output: Option<PathBuf>) -> io::Result<()> {
    let (words, _) = parse::parse_wsca(&util::validate_or_get_path(words.as_deref(), &[WORD_FILE_EXT, "txt"], "word")?)?;
    let rules = parse::parse_rsca(&util::validate_or_get_path(rules.as_deref(), &[RULE_FILE_EXT, "txt"], "rule")?)?;

    let (into, from) = if let Some(al) = alias {
        parse::parse_alias(util::as_file(&al)?)?
    } else {
        (Vec::new(), Vec::new())
    };

    let json = serde_json::to_string_pretty(&(AscaJson { into, from, words, rules }))?;

    if let Some(path) = &output {
        util::write_to_file(path, json, JSON_FILE_EXT, None)
    } else {
        util::dir_create_file(&format!("out.{JSON_FILE_EXT}"), json, None)
    }
}

/// Split an asca-web json file into separate rsca, wsca, and alias files
pub fn from_json(path: Option<PathBuf>, words_path: Option<PathBuf>, rules_path: Option<PathBuf>, alias_path: Option<PathBuf>) -> io::Result<()> {

    let file = util::file_open(&util::validate_or_get_path(path.as_deref(), &["json"], "json")?)?;
    let json: AscaJson = serde_json::from_reader(file)?;

    let words = json.words.join("\n");
    let rules = util::to_rsca_format(json.rules);

    if let Some(path) = words_path {
        util::write_to_file(&path, words, WORD_FILE_EXT, None)?;
    } else {
        util::dir_create_file(&format!("out.{WORD_FILE_EXT}"), words, None)?;
    }

    if let Some(path) = rules_path {
        util::write_to_file(&path, rules, RULE_FILE_EXT, None)?;
    } else {
        util::dir_create_file(&format!("out.{RULE_FILE_EXT}"), rules, None)?;
    }


    if !json.into.is_empty() || !json.from.is_empty() {
        let alias = util::to_alias(json.into, json.from);
        
        if let Some(path) = alias_path {
            util::write_to_file(&path, alias, ALIAS_FILE_EXT, None)?;
        } else {
            util::dir_create_file(&format!("out.{ALIAS_FILE_EXT}"), alias, None)?;
        }
    }

    Ok(())
}

pub fn update_conf_format(config_dir: Option<PathBuf>) -> io::Result<()> {
    let (mut path, is_dir) = util::validate_file_or_dir(config_dir)?;
    let conf = get_old_config(&path, is_dir)?;

    let new_conf = util::to_new_config_format(conf);

    if !is_dir {
        path.pop();
    }

    path.push(format!("config-new.{CONF_FILE_EXT}"));

    util::dir_create_file(&path, new_conf, None)
    
}

/// Convert a tag within a config file to an asca-web json file
pub fn from_seq(config_dir: Option<PathBuf>, tag: String, output: Option<PathBuf>, recurse: bool) -> io::Result<()> {
    let (mut path, is_dir) = util::validate_file_or_dir(config_dir)?;
    let conf = get_config(&path, is_dir)?;

    if !is_dir { path.pop(); }

    let Some(seq) = conf.iter().find(|c| c.tag.as_ref() == tag) else {
        let possible_tags = conf.iter().map(|c| c.tag.clone()).collect::<Vec<_>>().join("\n- ");
        return Err(io::Error::other(format!("{} Could not find tag '{}' in config.\nAvailable tags are:\n- {}", "Config Error:".bright_red(), tag.yellow(), possible_tags)))
    };

    let json = if !seq.get_from_tags().is_empty() && recurse {
        // get original words list and entire rule history
        let words = get_orig_words(&conf, &path, seq)?;
        let rules = get_all_rules(&conf, seq)?;

        let into = get_orig_alias_into(&conf, &path, seq)?;

        let (_ , from) = if let Some(alias) = &seq.alias {
            let mut a_path = path.to_path_buf();
            a_path.push(alias.as_ref());
            // a_path.set_extension(ALIAS_FILE_EXT);
            parse::parse_alias(util::as_file(&a_path)?)?
        } else {
            (Vec::new(), Vec::new())
        };

        serde_json::to_string_pretty(&(AscaJson { into, from, words, rules }))?
    } else {
        let words = get_words(&conf, &path, &vec![], seq, &mut std::collections::HashMap::new())?;

        let mut rules = vec![];
        for entry in &seq.entries {
            rules.extend_from_slice(&entry.rules);
        }

        let (into, from) = if let Some(alias) = &seq.alias {
            let mut a_path = path.to_path_buf();
            a_path.push(alias.as_ref());
            // a_path.set_extension(ALIAS_FILE_EXT);
            parse::parse_alias(util::as_file(&a_path)?)?
        } else {
            (Vec::new(), Vec::new())
        };

        serde_json::to_string_pretty(&(AscaJson { into, from, words, rules }))?
    };

    if let Some(path) = &output {
        util::write_to_file(path, json, JSON_FILE_EXT, None)
    } else {
        util::dir_create_file(&format!("out.{JSON_FILE_EXT}"), json, None)
    }
}