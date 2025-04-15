use std::{collections::HashMap, io, path::{Path, PathBuf}, rc::Rc};
use colored::Colorize;

use asca::RuleGroup;
use super::{config::{lexer::Lexer, parser::Parser}, parse::{self, parse_wsca}, util::{self, ALIAS_FILE_EXT, CONF_FILE_EXT, WORD_FILE_EXT}};


#[derive(Debug, Clone)]
pub struct ASCAConfig {
    pub tag: Rc<str>,
    pub from: Option<Rc<str>>,
    pub alias: Option<Rc<str>>,
    pub words: Vec<Rc<str>>,
    pub entries: Vec<Entry>
}

impl ASCAConfig {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self { tag: Rc::default(), from: None, alias: None,  words: vec![], entries: Vec::new() }
    }
}

#[derive(Debug, Clone)]
pub struct Entry {
    pub name: PathBuf,
    pub rules: Vec<RuleGroup>,
}

impl Entry {
    pub fn from(name: PathBuf, rules: Vec<RuleGroup>) -> Self {
        Self { name, rules }
    }
}

pub enum RuleFilter {
    Only(String),
    Without(String),
    OnlyMult(Vec<String>),
    WithoutMult(Vec<String>),
}

struct SeqFlags {
    output: bool,
    overwrite: Option<bool>,
    output_all: bool,
    all_steps: bool,
}

type SeqTrace = Vec<Vec<String>>;


/// Read config file and return result
pub(super) fn get_config(dir: &Path) -> io::Result<Vec<ASCAConfig>> {
    let maybe_conf = util::get_dir_files(dir.to_str().unwrap(), &[CONF_FILE_EXT])?;

    if maybe_conf.is_empty() {
        return Err(io::Error::other(format!("{} No config file found in directory {dir:?}", "Error:".bright_red())))
    } else if maybe_conf.len() > 1 {
        return Err(io::Error::other(format!("{} Multiple config files found in directory {dir:?}", "Error:".bright_red())))
    }

    let tokens = Lexer::new(&util::file_read(maybe_conf[0].as_path())?.chars().collect::<Vec<_>>()).tokenise()?;

    Parser::new(tokens, maybe_conf[0].as_path()).parse()
}

pub(super) fn get_all_rules(rule_seqs: &[ASCAConfig], conf: &ASCAConfig) -> io::Result<Vec<RuleGroup>> {
    if let Some(from_tag) = &conf.from {
        let Some(seq) = rule_seqs.iter().find(|c| c.tag == *from_tag) else {
            let possible_tags = rule_seqs.iter().map(|c| c.tag.clone()).collect::<Vec<_>>().join("\n- ");
            return Err(io::Error::other(format!("{} Could not find tag '{}' in config.\nAvailable tags are:\n- {}", "Config Error:".bright_red(), from_tag.yellow(), possible_tags)))
        };
        let mut rules = get_all_rules(rule_seqs, seq)?;
        for entry in &conf.entries {
            rules.extend_from_slice(&entry.rules);
        }
        Ok(rules)

    } else {
        let mut rules = vec![];
        for entry in &conf.entries {
            rules.extend_from_slice(&entry.rules);
        }
        Ok(rules)
    }
}

pub(super) fn get_orig_alias_into(rule_seqs: &[ASCAConfig], dir: &Path,  conf: &ASCAConfig) -> io::Result<Vec<String>> {
    if let Some(from_tag) = &conf.from {
        let Some(seq) = rule_seqs.iter().find(|c| c.tag == *from_tag) else {
            let possible_tags = rule_seqs.iter().map(|c| c.tag.clone()).collect::<Vec<_>>().join("\n- ");
            return Err(io::Error::other(format!("{} Could not find tag '{}' in config.\nAvailable tags are:\n- {}", "Config Error:".bright_red(), from_tag.yellow(), possible_tags)))
        };
        get_orig_alias_into(rule_seqs, dir,seq)
    } else if let Some(al_path) = &conf.alias {
        let mut path = dir.to_path_buf();
        path.push(al_path.as_ref());
        path.set_extension(ALIAS_FILE_EXT);
        let (into, _) = parse::parse_alias(&util::validate(&path, &[ALIAS_FILE_EXT, "txt"])?)?;
        Ok(into)           
    } else {
        Ok(Vec::new())
    }
    
}

pub(super) fn get_orig_words(rule_seqs: &[ASCAConfig], dir: &Path,  conf: &ASCAConfig) -> io::Result<Vec<String>> {
    if let Some(from_tag) = &conf.from {
        let Some(seq) = rule_seqs.iter().find(|c| c.tag == *from_tag) else {
            let possible_tags = rule_seqs.iter().map(|c| c.tag.clone()).collect::<Vec<_>>().join("\n- ");
            return Err(io::Error::other(format!("{} Could not find tag '{}' in config.\nAvailable tags are:\n- {}", "Config Error:".bright_red(), from_tag.yellow(), possible_tags)))
        };
        get_orig_words(rule_seqs, dir,seq)
    } else {
        let mut words = Vec::new();
        for w_str in &conf.words {
            let mut w_path = dir.to_path_buf();
            w_path.push(w_str.as_ref());
            w_path.set_extension(WORD_FILE_EXT);
            let (mut w_file, _) = parse_wsca(&util::validate_or_get_path(Some(&w_path), &[WORD_FILE_EXT, "txt"], "word")?)?;
            if !words.is_empty() {
                words.push("".to_string());
            }
            words.append(&mut w_file);
        }
        Ok(words)
    }
}

/// Determine which word file to use, validate, and parse it into a vec of word strings
pub(super) fn get_words(rule_seqs: &[ASCAConfig], dir: &Path, words_path: &Option<PathBuf>, conf: &ASCAConfig, seq_cache: &mut HashMap<Rc<str>, Vec<String>>) -> io::Result<Vec<String>> {
    let mut words = if let Some(from_tag) = &conf.from {
        if let Some(x) = seq_cache.get(from_tag) {
            x.clone()
        } else {
            let Some(seq) = rule_seqs.iter().find(|c| c.tag == *from_tag) else {
                let possible_tags = rule_seqs.iter().map(|c| c.tag.clone()).collect::<Vec<_>>().join("\n- ");
                return Err(io::Error::other(format!("{} Could not find tag '{}' in config.\nAvailable tags are:\n- {}", "Config Error:".bright_red(), from_tag.yellow(), possible_tags)))
            };
            if let Some((t, _)) = run_sequence(rule_seqs, dir, words_path, seq, seq_cache)? {
                let w = t.last().unwrap().clone();
                seq_cache.insert(seq.tag.clone(), w.clone());
                w
            } else {
                return Ok(vec![])
            }
        }
    } else { 
        vec![] 
    };

    if let Some(wp) = words_path {
        let (mut w, _) = parse_wsca(&util::validate_or_get_path(Some(wp), &[WORD_FILE_EXT, "txt"], "word")?)?;
        words.append(&mut w);
    } else if !conf.words.is_empty() {
        for ws in &conf.words {
            let mut wp = dir.to_path_buf();
            wp.push(ws.as_ref());
            wp.set_extension(WORD_FILE_EXT);
            let (mut w_file, _) = parse_wsca(&util::validate_or_get_path(Some(&wp), &[WORD_FILE_EXT, "txt"], "word")?)?;
            if !words.is_empty() {
                words.push("".to_string());
            }
            words.append(&mut w_file);
        }
    } 

    if words.is_empty() {
        return Err(io::Error::other(format!("{} No input words defined for config '{}'.\nYou can specify a word file at runtime with the -w option", "Config Error:".bright_red(), conf.tag)))
    }

    Ok(words)
}

/// Handle writing the result to file
fn output_result(dir: &Path, tag: &str, trace: &[Vec<String>], seq_names: &[PathBuf], overwrite: Option<bool>, output_all: bool) -> io::Result<()> {
    // Create <out/tag> subfolder within <dir> if doesn't exist
    // Create files for each seq, <seq_name>.wsca, and write to each
    let mut path = dir.to_path_buf();
    path.push("out");
    path.push(tag);
    
    if !path.exists() {
        util::dir_create_all(&path)?;
    }

    if output_all {
        for (seq, name) in seq_names.iter().enumerate() {
            let content = trace[seq+1].join("\n");
            let mut p = path.clone();
            let name = format!("{}_{}", seq+1, name.file_name().unwrap().to_os_string().into_string().unwrap());
            p.push(name);
            p.set_extension(WORD_FILE_EXT);
            util::write_to_file(&p, content, WORD_FILE_EXT, overwrite)?;
        }
    } else {
        let name = seq_names.last().unwrap().file_name().unwrap();
        let content = trace.last().unwrap().join("\n");
        let mut p = path.clone();
        p.push(name);
        p.set_extension(WORD_FILE_EXT);
        util::write_to_file(&p, content, WORD_FILE_EXT, overwrite)?;
    }

    Ok(())
}

fn calc_padding(trace: &[Vec<String>]) -> Vec<usize> {
    let mut arr  = Vec::with_capacity(trace.len());

    for col in trace {
        let mut len = 0;
        for word in col {
            let spacing_chars_num = word.chars().count() - util::fix_combining_char_pad(word);
            len = std::cmp::max(len, spacing_chars_num)
        }
        arr.push(len+1);
    }

    arr
}

// Handle printing result to the terminal
fn print_result(trace: &[Vec<String>], tag: &str, all_steps: bool) {
    debug_assert!(!trace.is_empty());
    println!("\nOUTPUT - {}", tag);
    let arr = "=>".bright_red().bold();

    let num_steps = trace.len();
    let num_words = trace[0].len();
    let base_pad = calc_padding(trace);

    for word in 0..num_words {
        if trace[0][word].is_empty() {
            println!();
            continue;
        }
        // Concat start word
        let pad = base_pad[0] + util::fix_combining_char_pad(&trace[0][word]);
        let mut str = format!("{:<pad$}", &trace[0][word].bright_blue().bold());
        if all_steps {
            // Concat intermediate steps
            for (s, step) in trace.iter().take(num_steps-1).skip(1).enumerate() {
                let pad = base_pad[s] + util::fix_combining_char_pad(&step[word]);
                str = format!("{str}{arr} {:<pad$}", &step[word]);
            }
        }
        // Concat final result
        let pad = base_pad[num_steps-1] + util::fix_combining_char_pad(&trace[num_steps-1][word]);
        str = format!("{str} {arr} {:<pad$}", &trace[num_steps-1][word].bright_green().bold());
        println!("{}", str)
    }
    // println!();
}

/// Pass a sequence to ASCA and return a trace and the name of each file that was used
pub fn run_sequence(config: &[ASCAConfig], dir: &Path, words_path: &Option<PathBuf>, seq: &ASCAConfig, seq_cache: &mut HashMap<Rc<str>, Vec<String>>) -> io::Result<Option<(SeqTrace, Vec<PathBuf>)>> {
    let mut files = Vec::new();
    let mut trace = Vec::new();

    let words = get_words(config, dir, words_path, seq, seq_cache)?;
    let (into , from) = if let Some(alias) = &seq.alias {
        let mut a_path = dir.to_path_buf();
        a_path.push(alias.as_ref());
        a_path.set_extension(ALIAS_FILE_EXT);
        parse::parse_alias(&util::validate(&a_path, &[ALIAS_FILE_EXT, "txt"])?)?
    } else {
        (Vec::new(), Vec::new())
    };
    if words.is_empty() { return Ok(None) }
    trace.push(words.clone());
    for (i, entry) in seq.entries.iter().enumerate() {
        files.push(entry.name.clone());
        match asca::run(&entry.rules, &trace[i], &into, &from) {
            Ok(res) => trace.push(res),
            Err(err) => {
                util::print_asca_errors(err, &words, &entry.rules, &into, &from);
                return Ok(None)
            },
        }
    }
    Ok(Some((trace, files)))
}

/// Run a given sequence, then deal with output if necessary
fn handle_sequence(config: &[ASCAConfig], seq_cache: &mut HashMap<Rc<str>, Vec<String>>, dir_path: &Path, words_path: &Option<PathBuf>, seq: &ASCAConfig, flags: &SeqFlags) -> io::Result<()> {   
    if let Some((trace, files)) = run_sequence(config, dir_path, words_path, seq, seq_cache)? {
        if trace.is_empty() {
            return Err(io::Error::other(format!("{} No words in input for tag '{}'", "Config Error:".bright_red(), seq.tag)))
        }
        seq_cache.insert(seq.tag.clone(), trace.last().unwrap().clone());

        print_result(&trace, &seq.tag, flags.all_steps);
        if flags.output {
            return output_result(dir_path, &seq.tag, &trace, &files, flags.overwrite, flags.output_all)
        } 
    }
    Ok(())
}

pub(crate) fn run(maybe_dir_path: Option<PathBuf>, words_path: Option<PathBuf>, maybe_tag: Option<String>, output: bool, overwrite: Option<bool>, output_all: bool, all_steps: bool) -> io::Result<()> {
    let dir_path = util::validate_directory(maybe_dir_path)?;
    let config = get_config(&dir_path)?;

    let mut seq_cache = HashMap::new();

    let flags = SeqFlags { output, overwrite, output_all, all_steps };

    if let Some(tag) = maybe_tag {
        // Would be better if this was a hashmap
        // but even if the file was unreasonably long as to cause slowdowns, we'd have other issues
        let Some(seq) = config.iter().find(|c| c.tag.as_ref() == tag) else {
            let possible_tags = config.iter().map(|c| c.tag.clone()).collect::<Vec<_>>().join("\n- ");
            return Err(io::Error::other(format!("{} Could not find tag '{}' in config.\nAvailable tags are:\n- {}", "Config Error:".bright_red(), tag.yellow(), possible_tags)))
        };
        handle_sequence(&config, &mut seq_cache, &dir_path, &words_path, seq, &flags)
    } else {
        // Run all sequences
        for seq in &config {
            handle_sequence(&config, &mut seq_cache, &dir_path, &words_path, seq, &flags)?;
        }
        Ok(())
    }
}