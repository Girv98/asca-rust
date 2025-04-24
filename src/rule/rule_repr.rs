use crate :: {
    error :: {ASCAError, RuleSyntaxError}, 
    word  :: Phrase
};
use super::{trace::Change, Rule};

/// The unparsed ASCA Rule Group
#[derive(Default, Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct RuleGroup {
    pub name: String,
    pub rule: Vec<String>,
    pub description: String, 
}

impl RuleGroup {
    pub fn new() -> Self {
        Self { name: String::new(), rule: Vec::new(), description: String::new() }
    }

    pub fn is_empty(&self) -> bool {
        self.name.is_empty() && self.rule.is_empty() && self.description.is_empty()
    }
}


#[derive(Default, Debug)]
pub struct RuleGroupBuilder {
    name: String,
    rule: Vec<String>,
    description: String, 
}

impl RuleGroupBuilder {
    pub fn new() -> Self {
        Self { name: String::new(), rule: Vec::new(), description: String::new() }
    }

    pub fn name<T: Into<String>>(mut self, name: T) -> Self {
        self.name = name.into();
        self
    }

    pub fn desc<T: Into<String>>(mut self, desc: T) -> Self {
        self.description = desc.into();
        self
    }

    pub fn rules<T: Into<String>>(mut self, rule: Vec<T>) -> Self {
        rule.into_iter().for_each(| r| {
            self.rule.push(r.into());
        });
        self
    }

    pub fn rule<T: Into<String>>(mut self, rule: T) -> Self {
        self.rule.push(rule.into());
        self
    }

    pub fn build(self) -> RuleGroup {
        RuleGroup { name: self.name, rule: self.rule, description: self.description }
    }
}

/// SOA of the parsed ASCA Rule Group
#[derive(Default, Debug, Clone)]
pub struct ParsedRules {
    pub names: Vec<String>,
    pub rules: Vec<Vec<Rule>>,
    pub descs: Vec<String>,
}

impl TryFrom<Vec<RuleGroup>> for ParsedRules {    
    type Error = RuleSyntaxError;
    
    fn try_from(rgs: Vec<RuleGroup>) -> Result<Self, Self::Error> {
        let names: Vec<String> = rgs.iter().map(|v| v.name.clone()).collect();
        let rules = crate::parse_rule_groups(&rgs)?;
        let descs: Vec<String> = rgs.iter().map(|v| v.description.clone()).collect();

        Ok(Self { names, rules, descs })
    }
}

impl<'a>  IntoIterator for &'a ParsedRules {
    type Item = (&'a str, &'a [Rule], &'a str);

    type IntoIter = ParsedRulesIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        ParsedRulesIter {
            pr: self,
            index: 0,
        }
    }
}

impl ParsedRules {

    pub fn new() -> Self {
        Self { names: Vec::new(), rules: Vec::new(), descs: Vec::new() }
    }

    /// Applies self to the input phrases
    pub fn apply(&self, phrases: &[Phrase]) -> Result<Vec<Phrase>, ASCAError> {        
        crate::apply_rule_groups(&self.rules, phrases)
    }

    /// Returns the changes that affect the input phrase
    pub fn trace(&self, phrase: &Phrase) -> Result<Vec<Change>, ASCAError> {        
        crate::apply_rules_trace(&self.rules, phrase)
    }

    /// Returns an array of references to the traced rules or None if out of bounds.
    pub fn get_traced_rules(&self, changes: &[Change]) -> Option<Vec<(&String, &Vec<Rule>, &String)>> {
        changes.iter().map(|Change { rule_index: i, .. }| {
            match (self.names.get(*i), self.rules.get(*i), self.descs.get(*i)) {
                (Some(n), Some(r), Some(d)) => Some((n, r, d)),
                _ => None,
            }
        }).collect()
    }
}

pub struct ParsedRulesIter<'a> {
    pr: &'a ParsedRules,
    index: usize
}

impl<'a> Iterator for ParsedRulesIter<'a> {
    type Item = (&'a str, &'a [Rule], &'a str);

    fn next(&mut self) -> Option<Self::Item> {

        let n = self.pr.names.get(self.index);
        let r = self.pr.rules.get(self.index);
        let d = self.pr.descs.get(self.index);

        if n.is_some() && r.is_some() && d.is_some() {
            self.index += 1;
            Some((
                unsafe { n.unwrap_unchecked() }, 
                unsafe { r.unwrap_unchecked() }, 
                unsafe { d.unwrap_unchecked() }
            ))
        } else {
            None
        }

    }
}