use crate :: { error::ASCAError, word::Phrase };
use super :: RuleGroup;

pub struct Change {
    /// The index of the [RuleGroup] or [ParsedRules](super::ParsedRules) that applies the change
    pub rule_index: usize,
    /// The phrase as the result of this change
    pub after: Phrase,
}


pub fn get_changes(unparsed_rules: &[RuleGroup], unparsed_phrase: &str, alias_into: &[String]) -> Result<Vec<Change>, ASCAError> {
    let phrase = Phrase::try_from(unparsed_phrase, alias_into)?;
    let rules = crate::parse_rule_groups(unparsed_rules)?;

    crate::apply_rules_trace(&rules, &phrase)
}

pub fn get_string(unparsed_rules: &[RuleGroup], unparsed_phrase: &str, alias_into: &[String]) -> Result<Vec<String>, ASCAError> {
    let phrase = Phrase::try_from(unparsed_phrase, alias_into)?;
    let rules = crate::parse_rule_groups(unparsed_rules)?;
    let res = crate::apply_rules_trace(&rules, &phrase)?;

    Ok(to_string(&phrase, res, unparsed_rules))
}

pub(crate) fn to_string(original: &Phrase, changes: Vec<Change>, rules: &[RuleGroup]) -> Vec<String> {
    let mut res = Vec::with_capacity(changes.len());
    let mut last = original.iter().fold(String::new(), |acc, w| {
        acc + &w.render(&[]).0 + " "
    });
    for change in changes {
        res.push(format!("Applied \"{}\":", rules[change.rule_index].name));
        let mut st = String::new();

        st.push_str(&last);
        st.push_str("=> ");

        let mut word = String::new();
        for aw in change.after.iter() {
            word.push_str(&aw.render(&[]).0);
            word.push(' ');
        }
        last = word;
        st.push_str(&last);

        res.push(st);
    }
    res
}

pub(crate) fn to_string_wasm(original: &Phrase, changes: Vec<Change>, rules: &[RuleGroup]) -> (Vec<String>, Vec<String>) {
    let mut res = Vec::with_capacity(changes.len());
    let mut unk = Vec::new();
    let mut last = String::new();
    
    for word in original.iter() {
        let (w, u) = word.render(&[]);
        unk.extend(u);
        last.push_str(&w);
        last.push(' ');
    }

    let mut last_unk = unk.clone();

    for change in changes {
        res.push(format!("Applied \"{}\":", rules[change.rule_index].name));
        let mut st = String::new();

        st.push_str(&last);
        st.push_str("=> ");

        unk.append(&mut last_unk);
        let mut word = String::new();
        for aw in change.after.iter() {
            let (w, u) = aw.render(&[]);
            last_unk.extend(u);
            word.push_str(&w);
            word.push(' ');
        }
        last = word;
        unk.extend_from_slice(&last_unk);
        st.push_str(&last);

        res.push(st);
    }
    (res, unk)
}