pub mod util;
pub mod args;
pub mod parse;
pub mod config;
pub mod seq;
pub mod run;
pub mod convert;

use asca::rule::RuleGroup;

#[derive(Debug, serde::Deserialize, serde::Serialize)]
struct AscaJson {
    #[serde(default)]
    pub into: Vec<String>,
    #[serde(default)]
    pub from: Vec<String>,
    pub words: Vec<String>,
    pub rules: Vec<RuleGroup>,
}