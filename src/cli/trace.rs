use colored::Colorize;

use asca::{error::ASCAError, rule::{RuleGroup, trace::Change}, word::Phrase};

use crate::cli::{parse, util::{self, RULE_FILE_EXT}};



pub(crate) fn run(rules: Option<std::path::PathBuf>, word: String, alias: Option<std::path::PathBuf>) -> Result<(), std::io::Error> {
    
    let rules = parse::parse_rsca(&util::validate_or_get_path(rules.as_deref(), &[RULE_FILE_EXT, "txt"], "rule")?)?;

    let (into, _) = if let Some(af) = alias {
        parse::parse_alias(util::as_file(&af)?)?
    } else {
        (Vec::new(), Vec::new())
    };

    // To handle potential comments
    let phrase = word.split('#').next().unwrap_or_default().trim();

    match get_string(&rules, phrase, &into) {
        Ok(sv) => for s in sv {
            println!("{}", s);
        },
        Err(_) => todo!(),
    }


    Ok(())
}


pub fn get_string(unparsed_rules: &[RuleGroup], unparsed_phrase: &str, alias_into: &[String]) -> Result<Vec<String>, ASCAError> {
    let phrase = Phrase::try_from(unparsed_phrase, alias_into)?;
    let rules = asca::rule::ParsedRules::try_from(unparsed_rules)?;
    let changes = rules.trace(&phrase)?;

    Ok(to_string(&phrase, changes, unparsed_rules))
}


fn to_string(original: &Phrase, changes: Vec<Change>, rules: &[RuleGroup]) -> Vec<String> {
    let mut res = Vec::with_capacity(changes.len());
    let mut last = original.iter().fold(String::new(), |acc, w| {
        acc + &w.render() + " "
    });
    for change in changes {
        res.push(format!("{}:", rules[change.rule_index].name).bright_green().to_string());
        let mut st = String::new();

        st.push_str("    ");
        st.push_str(&last);
        st.push_str(&"=> ".bright_blue().to_string());

        let mut word = String::new();
        for aw in change.after.iter() {
            word.push_str(&aw.render());
            word.push(' ');
        }
        last = word;
        st.push_str(&last);

        res.push(st);
    }
    res
}