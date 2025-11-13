use std::{io, path::{Path, PathBuf}};
use colored::Colorize;

use asca::rule::RuleGroup;
use super::{args::InGroup, parse, util::{self, LINE_ENDING, RULE_FILE_EXT, WORD_FILE_EXT}, AscaJson};

// Handle comparing the output to the contents of a wsca file.
fn print_comparison(result: &[String], comp_path: &Path) -> io::Result<()> {
    let (comparison, _) = parse::parse_wsca(&util::validate_or_get_path(Some(comp_path), &[WORD_FILE_EXT, "txt"], "word")?)?;
    let sep = "|".bright_red().bold();
    
    let mut cmp_len = 0;
    for comp in &comparison {
        let cmp_sp = comp.chars().count() - util::fix_combining_char_pad(comp);
        cmp_len = std::cmp::max(cmp_len, cmp_sp);
    }

    println!("{} {} {}\n", comp_path.to_str().expect("File path has been validated").bright_blue().bold(), sep, "OUTPUT".bright_green().bold());
    for (comp, res) in comparison.iter().zip(result) {
        let pad = cmp_len + util::fix_combining_char_pad(comp);
        if comp.is_empty() && res.is_empty() {
            println!()
        } else if res != comp {
            println!("{:<pad$} {} {}", comp.bright_blue().bold(), sep, res.bright_green().bold());
        } else {
            println!("{comp:<pad$} {sep} {res}");
        }
    }
    // In edge cases where one file is longer then the other
    match result.len().cmp(&comparison.len()) {
        std::cmp::Ordering::Equal => {},
        std::cmp::Ordering::Less => for comp in comparison.iter().skip(result.len()) {
            let pad = cmp_len + util::fix_combining_char_pad(comp);
            println!("{:<pad$} {}", comp.bright_blue().bold(), sep);
        },
        std::cmp::Ordering::Greater => for i in result.iter().skip(comparison.len()) {
            println!("{:<cmp_len$} {} {}", "", sep, i.bright_green().bold());
        },
    }
    Ok(())
}

// Handle printing result to the terminal
fn print_result(result: &[String], start_words: &[String], maybe_compare: Option<PathBuf>) -> io::Result<()> {
    if let Some(comp_path) = maybe_compare {
        print_comparison(result, &comp_path)
    } else {
        println!("OUTPUT");
        let arr = "=>".bright_red().bold();

        let mut bef_len = 0;
        for bef in start_words {
            let bef_sp = bef.chars().count() - util::fix_combining_char_pad(bef);
            bef_len = std::cmp::max(bef_len, bef_sp);
        }

        let mut output = String::with_capacity(start_words.len() * 64); // assuming at least 64 bytes per line

        for (bef, aft) in start_words.iter().zip(result) {
            if bef.is_empty() && aft.is_empty() {
                output += "\n";
            } else {
                let pad = bef_len + util::fix_combining_char_pad(bef);
                output += &format!("\n{:<pad$} {} {}", bef.bright_blue().bold(), arr, aft.bright_green().bold());
            }
        }

        println!("{}", output.trim());
        Ok(())
    }
}


type Words = Vec<String>;
type IntoAliases = Vec<String>;
type FromAliases = Vec<String>;
/// Gets input file. 
/// If -j,validate and parse words and rules from it. If -w, use those words instead.
/// Else, validate and parse -r and -w.
fn get_input(i_group: InGroup, input: Vec<PathBuf>, alias: Option<PathBuf>) -> io::Result<(Words, Vec<RuleGroup>, IntoAliases, FromAliases)> {
    let InGroup { from_json, rules } = i_group;
    if let Some(json) = from_json {
        let json_file_path = util::validate_or_get_path(Some(&json), &["json"], "json")?;

        let file = util::file_open(&json_file_path)?;
        let json: AscaJson = serde_json::from_reader(file)?;

        let words = if !input.is_empty() {
            let mut w = Vec::new();
            for wf in input {
                w.extend(parse::parse_wsca(util::as_file(&wf)?)?.0);
            }
            w
        } else {
            json.words
        };

        if words.is_empty() {
            return Err(io::Error::other(format!("{} no words provided", "asca:".bright_blue())))
        }

        let (into, from) = if let Some(af) = alias {
            parse::parse_alias(util::as_file(&af)?)?
        } else {
            (json.into, json.from)
        };

        Ok((words, json.rules, into, from))

    } else {
        let mut words = Vec::new();
        for wf in input {
                words.extend(parse::parse_wsca(util::as_file(&wf)?)?.0);
        }
        
        if words.is_empty() {
            return Err(io::Error::other(format!("{} no words provided", "asca:".bright_blue())))
        }

        let rules = parse::parse_rsca(&util::validate_or_get_path(rules.as_deref(), &[RULE_FILE_EXT, "txt"], "rule")?)?;

        let (into, from) = if let Some(af) = alias {
            parse::parse_alias(util::as_file(&af)?)?
        } else {
            (Vec::new(), Vec::new())
        };

        Ok((words, rules, into, from))
    }
}

/// Handle writing the result to file
fn output_result(output: Option<PathBuf>, res: &[String]) -> io::Result<()> {
    if let Some(path) = &output {
        util::write_to_file(path, res.join(LINE_ENDING), WORD_FILE_EXT, None)?;
    }
    Ok(())
}

pub(crate) fn run(in_group: InGroup, maybe_words: Vec<PathBuf>, maybe_alias: Option<PathBuf>, maybe_output: Option<PathBuf>, maybe_compare: Option<PathBuf>) -> io::Result<()> {
    let (words, rules, into, from) = get_input(in_group, maybe_words, maybe_alias)?;

    match asca::par_run_unparsed(&rules, &words, &into, &from) {
        Ok(res) => {
            print_result(&res, &words, maybe_compare)?;
            output_result(maybe_output, &res)
        },
        Err(err) => { 
            Err(util::get_asca_errors(err, &rules, &into, &from))
        },
    }
}