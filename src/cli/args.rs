use std::path::PathBuf;

use clap :: {
    Args, Command, Parser, Subcommand, ValueHint
};
use clap_complete::{generate, Generator, Shell};
use clap_stdin::MaybeStdin;

#[derive(Debug, Parser)]
#[clap(author, version, about)]
#[command(name = "asca")]
pub struct CliArgs {
    #[arg(long = "generate", value_enum)]
    /// Generate a completion script for a given shell
    pub(crate) shell: Option<Shell>,
    #[clap(subcommand)]
    pub cmd: Option<AscaCommand>,
}

#[derive(Debug, Subcommand)]
#[command(arg_required_else_help = true, visible_alias = "hint")]
pub enum AscaCommand {
    /// Run basic cli.
    Run {
        #[clap(flatten)]
        i_group: InGroup,

        /// Paths to wsca files containing the input words.
        /// If -j is provided, these words with be used instead of the words listed in the json file
        #[clap(verbatim_doc_comment, value_hint=ValueHint::FilePath)]
        words: Vec<MaybeStdin<PathBuf>>,
        /// Path to an alias file containing romanisations to and from.
        #[arg(short='l', long, verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
        alias: Option<PathBuf>,

        /// Path of a wsca file to compare with the result.
        #[arg(short, long, verbatim_doc_comment, value_hint=ValueHint::FilePath)]
        compare: Option<PathBuf>,

        /// Desired path of output file.
        /// - If a directory is provided, asca will create an out.wsca file in that directory.
        /// - If not provided, asca will not write to any file.
        #[arg(short, long, verbatim_doc_comment, value_hint=ValueHint::FilePath)]
        output: Option<PathBuf>,
    },
    /// Run an asca config.
    Seq {
        /// Path to the config file or the directory it is within.
        #[arg(value_hint=clap::ValueHint::DirPath)]
        path: Option<PathBuf>,
        
        /// Run a given tag in the config file.
        /// - If not provided, all tags in the config will be run.
        #[arg(short='t', long, verbatim_doc_comment, value_hint=clap::ValueHint::Other)]
        tag: Option<String>,

        /// Paths to wsca files.
        /// - If provided, these will be used instead of the word files defined in the config.
        #[arg(short, long, verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
        words: Vec<MaybeStdin<PathBuf>>,

        /// Print all intermediate steps in a sequence.
        #[arg(short, long, action, verbatim_doc_comment)]
        all_steps: bool,

        /// When given, asca will create an out folder within the [PATH] directory.
        #[arg(short, long, action, verbatim_doc_comment)]
        output: bool,

        /// Accept cases where an output file would be overwritten.
        /// - Requires --output
        #[arg(short='y', long, action, requires="output", conflicts_with="no_overwrite", verbatim_doc_comment)]
        overwrite: bool,

        /// Reject cases where an output file would be overwritten.
        /// - Requires --output
        #[arg(short='n', long, action, requires="output", conflicts_with="overwrite", verbatim_doc_comment)]
        no_overwrite: bool,

        /// Output all intermediate steps in a sequence.
        /// - Requires --output
        #[arg(short='i', long, action, requires="output", verbatim_doc_comment)]
        output_all: bool,
    },
    /// Convert between different formats, such as the asca-web json file and the cli wsca/rsca format.
    #[clap(subcommand)]
    Conv(Conv),
    // /// Enter tui.
    // Tui
}

#[derive(Debug, Subcommand)]
pub enum Conv {
    /// Convert a word file and rule file into an asca-web json file.
    Asca {
        /// Path to the word file to convert.
        /// - If not provided, asca will look for a file in the current directory.
        #[arg(short, long, verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
        words: Option<PathBuf>,
        /// Path to the rule file to convert.
        /// - If not provided, asca will look for a file in the current directory.
        #[arg(short, long, verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
        rules: Option<PathBuf>,
        /// Path to an optional alias file to convert.
        #[arg(short, long, verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
        alias: Option<PathBuf>,
        /// The desired path of the output json file.
        /// - If not provided, asca will create a file in the current directory.
        #[arg(short, long, verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
        output: Option<PathBuf>,
    },
    /// Convert a json file into separate word and rule files
    Json {
        /// Path to the Json file to convert.
        /// - If not provided, asca will look for a file in the current directory.
        #[arg(short, long, verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
        path: Option<PathBuf>,
        /// The desired path to the output word file.
        /// - If not provided, asca will create a file in the current directory.
        #[arg(short, long, verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
        words: Option<PathBuf>,
        /// The desired path to the output alias file, if applicable.
        /// - If not provided, asca will create a file in the current directory.
        #[arg(short, long, verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
        alias: Option<PathBuf>,
        /// The desired path to the output rule file.
        /// - If not provided, asca will create a file in the current directory.
        #[arg(short, long, verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
        rules: Option<PathBuf>,
    },
    /// Convert a tag within a config file into an asca-web json file
    Tag {
        /// Path to the config file or the directory it is within.
        /// - If not provided, asca will look for a config in the current directory.
        #[arg(short, long, verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
        path: Option<PathBuf>,
        /// The tag within the config file to be converted.
        #[arg(verbatim_doc_comment, value_hint=clap::ValueHint::Other)]
        tag: String,
        /// Follow a pipeline back to its root tag and generate a full linear rule history
        /// - Additional words added after the start of the pipeline will not be included
        /// - Note: Tags cannot have multiple piped inputs as there would be a split history.
        #[arg(short, long, action, verbatim_doc_comment)]
        recurse: bool,
        /// The desired path of the output rule file.
        /// - If not provided, asca will create a file in the current directory.
        #[arg(short, long, verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
        output: Option<PathBuf>,
    },
    /// Convert an old config file into the new format
    Config {
        /// Path to the config file or the directory it is within.
        /// - If not provided, asca will look for a config in the current directory.
        #[clap(verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
        path: Option<PathBuf>
    }
}

#[derive(Debug, Args)]
#[group(multiple = false)]
pub struct InGroup {
    /// Path to an asca-web json file, mutually exclusive with -r.
    /// - If -w is supplied, those words will be used instead of those defined in the json.
    #[arg(short='j', long, verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
    pub from_json: Option<PathBuf>,

    /// Path to a rsca file containing the rules to be applied, mutually exclusive with -j.
    /// - If neither -j nor -r is provided, asca will look for a file in the current directory.
    #[arg(short, long, verbatim_doc_comment, value_hint=clap::ValueHint::FilePath)]
    pub rules: Option<PathBuf>,
}

pub(crate) fn print_completions<G: Generator>(r#gen: G, cmd: &mut Command) {
    generate(r#gen, cmd, cmd.get_name().to_string(), &mut std::io::stdout());
}