mod cli; 
use cli::args::{CliArgs, AscaCommand, Conv};
use clap_stdin::MaybeStdin;
use colored::Colorize;

use std::{io, path::PathBuf, process};
use clap::{CommandFactory, Parser};


fn run() -> io::Result<()> {

    let args = CliArgs::parse();

    if let Some(generator) = args.generator {
        let mut cmd = CliArgs::command();
        eprintln!("Generating completion file for {generator:?}...");
        cli::args::print_completions(generator, &mut cmd);
        return Ok(())
    }

    match args.cmd {
        Some(AscaCommand::Run { i_group, words, alias, compare, output }) => cli::run::run(i_group, get_word_input(words)?, alias, output, compare),
        Some(AscaCommand::Conv(conv)) => match conv {
            Conv::Asca { words, rules, alias, output } => cli::convert::from_asca(words, rules, alias, output),
            Conv::Json { path, words, alias, rules }   => cli::convert::from_json(path, words, alias, rules),
            Conv::Tag  { path, tag, recurse, output }  => cli::convert::from_seq(path, tag, output, recurse),
            Conv::Config { path } => cli::convert::update_conf_format(path),
        },
        Some(AscaCommand::Seq { path, tag, words, all_steps, output, overwrite , no_overwrite, output_all }) => {
            let ow = match (overwrite, no_overwrite) {
                (true, false) => Some(true),
                (false, true) => Some(false),
                _ => None
            };
            cli::seq::run(path, get_word_input(words)?, tag, output, ow, output_all, all_steps)
        },
        // Some(AscaCommand::Tui) => println!("tui coming soon..."),
        None => Ok(()),
    }
}

fn get_word_input(maybe_words: Vec<MaybeStdin<PathBuf>>) -> io::Result<Vec<PathBuf>> {
    if !maybe_words.is_empty() && maybe_words[0].is_stdin() {
        let Some(w) = maybe_words[0].to_str() else { 
            return Err(io::Error::other(format!("{} could not parse input word list, contains invalid UTF-8 characters", "asca:".bright_red())));  
        };
        Ok(w.split(' ').map(PathBuf::from).collect())
    } else {
        Ok(maybe_words.iter().map(|x| x.to_path_buf()).collect())
    }
}

fn main() {
    if let Err(e) = run() {
        eprintln!("{e}");
        process::exit(1);
    }
}