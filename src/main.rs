mod cli; 
use cli::args::{CliArgs, AscaCommand, Conv};

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
        Some(AscaCommand::Run { i_group, words, alias, compare, output }) => {
            let words: Vec<PathBuf>  = if !words.is_empty() && words[0].is_stdin() {
                words[0].to_str().expect("Is valid unicode").split(' ').map(|x| PathBuf::from(x)).collect()
            } else {
                words.iter().map(|x| x.to_path_buf()).collect()
            };
            cli::run::run(i_group, words, alias, output, compare)
        },
        Some(AscaCommand::Conv(conv)) => match conv {
            Conv::Asca { words, rules, alias, output } => cli::convert::from_asca(words, rules, alias, output),
            Conv::Json { path, words, alias, rules }   => cli::convert::from_json(path, words, alias, rules),
            Conv::Tag  { path, tag, recurse, output }  => cli::convert::from_seq(path, tag, output, recurse),
        },
        // Some(AscaCommand::Mult { rules, words, compare, output }) => todo!(),
        Some(AscaCommand::Seq { path, tag, words, all_steps, output, overwrite , no_overwrite, output_all }) => {
            let ow = match (overwrite, no_overwrite) {
                (true, false) => Some(true),
                (false, true) => Some(false),
                _ => None
            };
            cli::seq::run(path, words, tag, output, ow, output_all, all_steps)
        },
        // Some(AscaCommand::Tui) => println!("tui coming soon..."),
        None => Ok(()),
    }
}

fn main() {
    if let Err(e) = run() {
        eprintln!("{e}");
        process::exit(1);
    }
}