pub mod util;
pub mod args;
pub mod parse;
pub mod config;
pub mod seq;
pub mod run;
pub mod convert;

use asca::RuleGroup;

#[derive(Debug, serde::Deserialize, serde::Serialize)]
struct AscaJson {
    pub words: Vec<String>,
    pub rules: Vec<RuleGroup>,
}