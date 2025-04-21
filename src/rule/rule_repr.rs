use crate :: {
    error :: {ASCAError, RuleSyntaxError}, 
    word  :: Phrase
};
use super::{trace::Change, Rule};

/// The unparsed ASCA Rule Group
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
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

impl Default for RuleGroup {
    fn default() -> Self {
        Self::new()
    }
}

pub struct RuleGroupBuilder {
    pub name: String,
    pub rule: Vec<String>,
    pub description: String, 
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

impl Default for RuleGroupBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
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

impl IntoIterator for ParsedRules {
    type Item = (String, Vec<Rule>, String);

    type IntoIter = ParsedRulesIter;

    fn into_iter(self) -> Self::IntoIter {
        ParsedRulesIter {
            names: self.names,
            rules: self.rules,
            descs: self.descs,
            index: 0,
        }
    }
}

impl ParsedRules {

    pub fn new() -> Self {
        Self { names: Vec::new(), rules: Vec::new(), descs: Vec::new() }
    }

    pub fn run(&self, phrases: &[Phrase]) -> Result<Vec<Phrase>, ASCAError> {        
        crate::apply_rule_groups(&self.rules, phrases)
    }

    pub fn trace(&self, phrase: &Phrase) -> Result<Vec<Change>, ASCAError> {        
        crate::apply_rules_trace(&self.rules, phrase)
    }

    /// Returns an array of references to the trace rules or None if out of bounds.
    pub fn get_traced_rules(&self, changes: &[Change]) -> Option<Vec<(&String, &Vec<Rule>, &String)>> {
        changes.iter().map(|Change { rule_index: i, .. }| {
            match (self.names.get(*i), self.rules.get(*i), self.descs.get(*i)) {
                (Some(n), Some(r), Some(d)) => Some((n, r, d)),
                _ => None,
            }
        }).collect()
    }
}

impl Default for ParsedRules {
    fn default() -> Self {
        Self::new()
    }
}

pub struct ParsedRulesIter {
    names: Vec<String>,
    rules: Vec<Vec<Rule>>,
    descs: Vec<String>,
    index: usize
}

impl Iterator for ParsedRulesIter {
    type Item = (String, Vec<Rule>, String);

    fn next(&mut self) -> Option<Self::Item> {

        let n = self.names.get(self.index).cloned();
        let r = self.rules.get(self.index).cloned();
        let d = self.descs.get(self.index).cloned();

        if n.is_none() && r.is_none() && d.is_none() {
            None
        } else {
            self.index += 1;
            Some((n.unwrap_or_default(), r.unwrap_or_default(), d.unwrap_or_default()))
        }

    }
}