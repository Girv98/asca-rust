use std::{collections::HashMap, io, path::{Path, PathBuf}, rc::Rc};
use colored::Colorize;

use asca::rule::RuleGroup;
use super  :: {
    config :: { lexer::Lexer, lexer_old::OldLexer, parser::Parser, parser_old::OldParser }, 
    parse  :: { self, parse_wsca }, 
    util   :: { self, CONF_FILE_EXT, WORD_FILE_EXT }
};


#[derive(Debug, Clone)]
pub struct OldConfig {
    pub tag: Rc<str>,
    pub from: Option<Rc<str>>,
    pub alias: Option<Rc<str>>,
    pub words: Vec<Rc<str>>,
    pub entries: Vec<OldEntry>
}

impl OldConfig {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self { tag: Rc::default(), from: None, alias: None,  words: vec![], entries: Vec::new() }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum InputKind {
    WordFile(Rc<str>),
    FromTag(Rc<str>),
}

impl InputKind {
    pub(crate) fn as_word_file(&self) -> Option<&Rc<str>> {
        if let Self::WordFile(v) = self {
            Some(v)
        } else {
            None
        }
    }

}

#[derive(Debug, Clone)]
pub struct ASCAConfig {
    pub tag: Rc<str>,
    pub alias: Option<Rc<str>>,
    pub input: Vec<InputKind>,
    pub entries: Vec<Entry>
}

impl ASCAConfig {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self { tag: Rc::default(), alias: None, input: vec![], entries: Vec::new() }
    }

    pub fn get_from_tags(&self) -> Vec<Rc<str>> {
        let mut tags = Vec::new();

        for input in &self.input {
            if let InputKind::FromTag(f) = input {
                tags.push(f.clone());
            }
        }

        tags
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

#[derive(Debug, Clone)]
pub struct OldEntry {
    pub verbatim: String, 
}

impl OldEntry {
    pub fn from(verbatim: String) -> Self {
        Self { verbatim }
    }
}

pub enum RuleFilter {
    Only(String),
    Without(String),
    OnlyMult(Vec<String>),
    WithoutMult(Vec<String>),
}

impl RuleFilter {
    pub fn as_file_string(&self) -> String {
        match self {
            RuleFilter::Only(rule_str) => {
                format!("_only_{}", util::sanitise_str(rule_str))
            },
            RuleFilter::Without(rule_str) => {
                format!("_excl_{}", util::sanitise_str(rule_str))
            },
            RuleFilter::OnlyMult(filters) => {
                let abv_filt =  filters.iter().map(|s| s.split(" ").filter_map(|x| x.chars().next()).collect::<String>()).collect::<Vec<String>>().join("-");
                format!("_om_{}", util::sanitise_str(&abv_filt))
            },
            RuleFilter::WithoutMult(filters) => {
                let abv_filt =  filters.iter().map(|s| s.split(" ").filter_map(|x| x.chars().next()).collect::<String>()).collect::<Vec<String>>().join("-");
                format!("_xm_{}", util::sanitise_str(&abv_filt))
                
            },
        }
    }
}

struct SeqFlags {
    output: bool,
    overwrite: Option<bool>,
    output_all: bool,
    all_steps: bool,
}

type SeqTrace = Vec<Vec<String>>;

fn confirm_config_path(dir: &Path, is_dir: bool) -> io::Result<PathBuf> {
    if is_dir {
        let maybe_conf = util::get_dir_files(dir.to_str().unwrap(), &[CONF_FILE_EXT])?;

        if maybe_conf.is_empty() {
            return Err(io::Error::other(format!("{} No config file found in directory {}", "Error:".bright_red(), format!("{dir:?}").yellow())))
        } else if maybe_conf.len() > 1 {
            return Err(io::Error::other(format!("{} Multiple config files found in directory {}. Please specify.", "Error:".bright_red(), format!("{dir:?}").yellow())))
        }
        Ok(maybe_conf[0].to_path_buf())
    } else {
        Ok(dir.to_path_buf())
    }
}

/// Read config file and return result
pub(super) fn get_config(dir: &Path, is_dir: bool) -> io::Result<Vec<ASCAConfig>> {
    let conf = confirm_config_path(dir, is_dir)?;

    let tokens = Lexer::new(&util::file_read(conf.as_path())?.chars().collect::<Vec<_>>()).tokenise()?;

    Parser::new(tokens, conf.as_path()).parse()
}

/// Read config file and return result
pub(super) fn get_old_config(dir: &Path, is_dir: bool) -> io::Result<Vec<OldConfig>> {
    let conf = confirm_config_path(dir, is_dir)?;

    let tokens = OldLexer::new(&util::file_read(conf.as_path())?.chars().collect::<Vec<_>>()).tokenise()?;

    OldParser::new(tokens).parse()
}

pub(super) fn get_all_rules(rule_seqs: &[ASCAConfig], conf: &ASCAConfig) -> io::Result<Vec<RuleGroup>> {

    let from_tags = conf.get_from_tags();

    if from_tags.len() > 1 {
        return Err(io::Error::other(format!("{} Not possible to recursively convert tags with multiple pipes", "Conversion Error:".bright_red())))
    }

    if from_tags.is_empty() {
        let mut rules = vec![];
        for entry in &conf.entries {
            rules.extend_from_slice(&entry.rules);
        }
        return Ok(rules)
    }

    let Some(seq) = rule_seqs.iter().find(|c| c.tag == from_tags[0]) else {
        let possible_tags = rule_seqs.iter().map(|c| c.tag.clone()).collect::<Vec<_>>().join("\n- ");
        return Err(io::Error::other(format!("{} Could not find tag '{}' in config.\nAvailable tags are:\n- {}", "Config Error:".bright_red(), from_tags[0].yellow(), possible_tags)))
    };
    
    let mut rules = get_all_rules(rule_seqs, seq)?;
    
    for entry in &conf.entries {
        rules.extend_from_slice(&entry.rules);
    }

    Ok(rules)

}

pub(super) fn get_orig_alias_into(rule_seqs: &[ASCAConfig], dir: &Path,  conf: &ASCAConfig) -> io::Result<Vec<String>> {

    let from_tags = conf.get_from_tags();

    if from_tags.len() > 1 {
        return Err(io::Error::other(format!("{} Not possible to recursively convert tags with multiple pipes", "Conversion Error:".bright_red())))
    }
    
    if from_tags.is_empty() {
        if let Some(al_path) = &conf.alias {
            let mut path = dir.to_path_buf();
            path.push(al_path.as_ref());
            let (into, _) = parse::parse_alias(util::as_file(&path)?)?;
            return Ok(into)    
        } else {
            return Ok(Vec::new())
        }
    }

    let Some(seq) = rule_seqs.iter().find(|c| c.tag == from_tags[0]) else {
        let possible_tags = rule_seqs.iter().map(|c| c.tag.clone()).collect::<Vec<_>>().join("\n- ");
        return Err(io::Error::other(format!("{} Could not find tag '{}' in config.\nAvailable tags are:\n- {}", "Config Error:".bright_red(), from_tags[0].yellow(), possible_tags)))
    };

    get_orig_alias_into(rule_seqs, dir,seq)
}

pub(super) fn get_orig_words(rule_seqs: &[ASCAConfig], dir: &Path, conf: &ASCAConfig) -> io::Result<Vec<String>> {

    let from_tags = conf.get_from_tags();

    if from_tags.len() > 1 {
        return Err(io::Error::other(format!("{} Not possible to recursively convert tags with multiple pipes", "Conversion Error:".bright_red())))
    }

    if from_tags.is_empty() {
        let mut words = Vec::new();
        for w_str in &conf.input {
            let mut w_path = dir.to_path_buf();
            w_path.push(w_str.as_word_file().expect("from_tags is empty").as_ref());
            let (mut w_file, _) = parse_wsca(&util::validate_or_get_path(Some(&w_path), &[WORD_FILE_EXT, "txt"], "word")?)?;
            if !words.is_empty() {
                words.push("".to_string());
            }
            words.append(&mut w_file);
        }
        return Ok(words)
    }

    let Some(seq) = rule_seqs.iter().find(|c| c.tag == from_tags[0]) else {
        let possible_tags = rule_seqs.iter().map(|c| c.tag.clone()).collect::<Vec<_>>().join("\n- ");
        return Err(io::Error::other(format!("{} Could not find tag '{}' in config.\nAvailable tags are:\n- {}", "Config Error:".bright_red(), from_tags[0].yellow(), possible_tags)))
    };

    get_orig_words(rule_seqs, dir,seq)
}

fn get_pipe(rule_seqs: &[ASCAConfig], dir: &Path, words_path: &Vec<PathBuf>, from_tag: &Rc<str>, seq_cache: &mut HashMap<Rc<str>, Vec<String>>) -> io::Result<Vec<String>> {
    // retrieve if cached
    if let Some(x) = seq_cache.get(from_tag) {
        return Ok(x.clone())
    }
    // else retrieve tag config
    let Some(seq) = rule_seqs.iter().find(|c| c.tag == *from_tag) else {
        let possible_tags = rule_seqs.iter().map(|c| c.tag.clone()).collect::<Vec<_>>().join("\n- ");
        return Err(io::Error::other(format!("{} Could not find tag '{}' in config.\nAvailable tags are:\n- {}", "Config Error:".bright_red(), from_tag.yellow(), possible_tags)))
    };
    // run tag and cache output
    if let Some((t, _, _)) = run_sequence(rule_seqs, dir, words_path, seq, seq_cache)? {
        let w = t.last().unwrap().clone();
        seq_cache.insert(seq.tag.clone(), w.clone());
        return Ok(w)
    }
    Ok(vec![])
    
}

/// Determine which word file to use, validate, and parse it into a vec of word strings
pub(super) fn get_words(rule_seqs: &[ASCAConfig], dir: &Path, words_path: &Vec<PathBuf>, conf: &ASCAConfig, seq_cache: &mut HashMap<Rc<str>, Vec<String>>) -> io::Result<Vec<String>> {   
    let mut words = Vec::new();

    if !words_path.is_empty() {
        for wp in words_path {
            let (mut w, _) = parse_wsca(util::as_file(wp)?)?;
            words.append(&mut w);
        }
    } else {
        for input in &conf.input {
            match input {
                InputKind::WordFile(wf) => {
                    let mut wp = dir.to_path_buf();
                    wp.push(wf.as_ref());
                    // wp.set_extension(WORD_FILE_EXT);
                    let (mut w_file, _) = parse_wsca(util::as_file(&wp)?)?;
                    if !words.is_empty() {
                        words.push("".to_string());
                    }
                    words.append(&mut w_file);
                },
                InputKind::FromTag(ft) => {
                    let pipe_words = get_pipe(rule_seqs, dir, words_path, ft, seq_cache)?;
                    if !words.is_empty() {
                        words.push("".to_string());
                    }
                    words.extend_from_slice(&pipe_words);
                },
            }
        }
    }

    if words.is_empty() {
        return Err(io::Error::other(format!("{} No input words defined for config '{}'.\nYou can specify a word file at runtime with the -w option", "Config Error:".bright_red(), conf.tag)))
    }

    Ok(words)
}

/// Handle writing the result to file
fn output_result(dir: &Path, tag: &str, trace: &[Vec<String>], seq_names: &[PathBuf], overwrite: Option<bool>, output_all: bool, is_romanised: bool) -> io::Result<()> {
    // Create <out/tag> subfolder within <dir> if doesn't exist
    // Create files for each seq, <seq_name>.wsca, and write to each
    let mut path = dir.to_path_buf();
    path.push("out");
    path.push(tag);
    
    if !path.exists() {
        util::dir_create_all(&path)?;
    }

    if output_all {
        let num_names = seq_names.len();
        let mut num = 1;
        for (seq, name) in seq_names.iter().enumerate() {
            if is_romanised && seq == num_names - 2 {
                continue;
            }
            let content = trace[seq+1].join("\n");
            let mut p = path.clone();
            let name = format!("{}_{}", num, name.file_name().unwrap().to_os_string().into_string().unwrap());
            p.push(name);
            p.set_extension(WORD_FILE_EXT);
            util::write_to_file(&p, content, WORD_FILE_EXT, overwrite)?;
            num += 1;
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
}

fn get_aliases(dir: &Path, seq: &ASCAConfig) -> io::Result<(Vec<String>, Vec<String>)> {
    if let Some(alias) = &seq.alias {
        let mut a_path = dir.to_path_buf();
        a_path.push(alias.as_ref());
        // a_path.set_extension(ALIAS_FILE_EXT);
        parse::parse_alias(util::as_file(&a_path)?)
    } else {
        Ok((Vec::new(), Vec::new()))
    }
}

#[inline]
fn run_once(trace: &mut Vec<Vec<String>>, files: &mut Vec<PathBuf>, entry: &Entry, index: usize, into: &[String], from: &[String]) -> Option<()> {
    files.push(entry.name.clone());
    match asca::run_unparsed(&entry.rules, &trace[index], into, from) {
        Ok(res) => {
            trace.push(res);
            Some(())
        },
        Err(err) => {
            util::print_asca_errors(err, &entry.rules, into, from);
            None
        },
    }
}

/// Pass a sequence to ASCA and return a trace and the name of each file that was used
pub fn run_sequence(config: &[ASCAConfig], dir: &Path, words_path: &Vec<PathBuf>, seq: &ASCAConfig, seq_cache: &mut HashMap<Rc<str>, Vec<String>>) -> io::Result<Option<(SeqTrace, Vec<PathBuf>, bool)>> {
    let mut files = Vec::new();
    let mut trace = Vec::new();

    let words = get_words(config, dir, words_path, seq, seq_cache)?;
    let (into, from) = get_aliases(dir, seq)?;

    if words.is_empty() { return Ok(None) }
    trace.push(words.clone());

    let num_steps = seq.entries.len();
    let last_step = num_steps - 1;
    
    if num_steps == 0 {
        return Ok(Some((trace, files, false)))
    } else if num_steps == 1 {
        if run_once(&mut trace, &mut files, &seq.entries[0], 0, &into, &from).is_none() { return Ok(None) }
        return Ok(Some((trace, files, !from.is_empty())))
    }

    // Head Iteration
    if run_once(&mut trace, &mut files, &seq.entries[0], 0, &into, &[]).is_none() { return Ok(None) }
    // Tail Iterations
    for (i, entry) in seq.entries.iter().enumerate().skip(1) {
        if run_once(&mut trace, &mut files, entry, i, &[], &[]).is_none() { return Ok(None) }
    }
    // Hack to apply romanisation as a separate step
    // TODO: What to do with the this in output
    if !from.is_empty() {
        let name = seq.entries[last_step].name.clone();
        // y.set_extension("");
        // let x = y.file_name().unwrap().to_str().unwrap().to_owned() + "-romanised";
        // y.set_file_name(x);
        let empty_entry = Entry { name, rules: Vec::new() };
        let _ = run_once(&mut trace, &mut files, &empty_entry, num_steps, &[], &from).is_none();
    }
    
    Ok(Some((trace, files,!from.is_empty())))
}

/// Run a given sequence, then deal with output if necessary
fn handle_sequence(config: &[ASCAConfig], seq_cache: &mut HashMap<Rc<str>, Vec<String>>, dir_path: &Path, words_path: &Vec<PathBuf>, seq: &ASCAConfig, flags: &SeqFlags) -> io::Result<()> {   
    if let Some((trace, files, is_romanised)) = run_sequence(config, dir_path, words_path, seq, seq_cache)? {
        if trace.is_empty() {
            return Err(io::Error::other(format!("{} No words in input for tag '{}'", "Config Error:".bright_red(), seq.tag)))
        }
        seq_cache.insert(seq.tag.clone(), trace.last().unwrap().clone());

        print_result(&trace, &seq.tag, flags.all_steps);
        if flags.output {
            return output_result(dir_path, &seq.tag, &trace, &files, flags.overwrite, flags.output_all, is_romanised)
        } 
    }
    Ok(())
}

pub(crate) fn run(maybe_dir_path: Option<PathBuf>, words_path: Vec<PathBuf>, maybe_tag: Option<String>, output: bool, overwrite: Option<bool>, output_all: bool, all_steps: bool) -> io::Result<()> {
    let (mut path, is_dir) = util::validate_file_or_dir(maybe_dir_path)?;
    let config = get_config(&path, is_dir)?;

    if !is_dir {
        path.pop();
    }

    let mut seq_cache = HashMap::new();

    let flags = SeqFlags { output, overwrite, output_all, all_steps };

    if let Some(tag) = maybe_tag {
        // Would be better if this was a hashmap
        // but even if the file was unreasonably long as to cause slowdowns, we'd have other issues
        let Some(seq) = config.iter().find(|c| c.tag.as_ref() == tag) else {
            let possible_tags = config.iter().map(|c| c.tag.clone()).collect::<Vec<_>>().join("\n- ");
            return Err(io::Error::other(format!("{} Could not find tag '{}' in config.\nAvailable tags are:\n- {}", "Config Error:".bright_red(), tag.yellow(), possible_tags)))
        };
        handle_sequence(&config, &mut seq_cache, &path, &words_path, seq, &flags)
    } else {
        // Run all sequences
        for seq in &config {
            handle_sequence(&config, &mut seq_cache, &path, &words_path, seq, &flags)?;
        }
        Ok(())
    }
}