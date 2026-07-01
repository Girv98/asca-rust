mod lexer;
mod parser;
mod rule_repr;
mod subrule;
pub mod trace;
mod modifier;

pub(crate) use modifier::*;
pub use rule_repr::*;
use subrule::*;
pub use lexer::*;
pub use parser::*;


use std ::{
    cell::RefCell, 
    cmp ::max, 
    collections::HashMap, 
    fmt 
};

use crate :: {
    error :: { ASCAError, RuleSyntaxError }, 
    word  :: { NodeKind, Phrase, Word}, 
};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum RuleType {
    Substitution,
    Metathesis,
    #[allow(unused)] // Coming 0.10.1+
    MetaOrdered,
    Deletion,
    Insertion,
}

#[derive(Debug, Clone)]
pub(crate) struct PlaceMod {
    pub(crate) lab: Option<u8>,
    pub(crate) cor: Option<u8>,
    pub(crate) dor: Option<u8>,
    pub(crate) phr: Option<u8>,
}

impl PlaceMod {
    pub(crate) fn new(lab: Option<u8>, cor: Option<u8>, dor: Option<u8>, phr: Option<u8>) -> Self {
        Self { lab, cor, dor, phr }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Alpha {
    Node(NodeKind, Option<u8>),
    Place(PlaceMod),
    Feature(bool),
    Supra(bool),
    Grouped(bool, bool),
}

impl Alpha {
    pub(crate) fn as_node(&self) -> Option<(NodeKind, Option<u8>)> {
        if let Self::Node(n, m) = self {
            Some((*n, *m))
        } else {
            None
        }
    }

    pub(crate) fn as_place(&self) -> Option<&PlaceMod> {
        if let Self::Place(place) = self {
            Some(place)
        } else {
            None
        }
    }

    pub(crate) fn as_grouped(&self) -> Option<(bool, bool)> {
        if let Self::Grouped(f, s) = self {
            Some((*f, *s))
        } else {
            None
        }
    }

    pub(crate) fn as_binary(&self) -> bool {
        match self {
            Alpha::Feature(pos) | Alpha::Supra(pos) => *pos,
            Alpha::Node(_, node_mod) => node_mod.is_some(),
            Alpha::Place(pm) => pm.lab.is_some() || pm.cor.is_some() || pm.dor.is_some() || pm.phr.is_some(),
            Alpha::Grouped(f, _) => *f,
        }
    }
}

#[derive(Clone)]
pub struct Rule {
    pub(crate) input:   Vec<Vec<ParseItem>>, // to support multirules
    pub(crate) output:  Vec<Vec<ParseItem>>, // these need to be Vec<Vec<Item>>
    pub(crate) context: Vec<EnvItem>,
    pub(crate) except:  Vec<EnvItem>,
    pub(crate) prop_rev: bool,
    pub(crate) cross_bound_inp: bool,
    pub(crate) cross_bound_env: bool,
}

impl Rule {
    pub(crate) fn new(i: Vec<Vec<ParseItem>>, o: Vec<Vec<ParseItem>>, context: Vec<EnvItem>, except: Vec<EnvItem>, prop_rev: bool, cross_bound_inp: bool, cross_bound_env: bool) -> Self {
        Self { input: i, output: o, context, except, prop_rev, cross_bound_inp, cross_bound_env }
    }

    pub(crate) fn split_into_subrules(&self) -> Result<Vec<SubRule>, RuleSyntaxError> {
        // check that input, output, context, except are the same length
        // and if any are not, that they are length == 1
        // context and except can be length == 0
        let max = max(self.input.len(), max(self.output.len(), max(self.context.len(), self.except.len())));

        if self.input.len()   != max && self.input.len()   != 1 { return Err(RuleSyntaxError::UnbalancedRuleIO(self.input.clone()))  }
        if self.output.len()  != max && self.output.len()  != 1 { return Err(RuleSyntaxError::UnbalancedRuleIO(self.output.clone())) }
        if self.context.len() != max && self.context.len() != 1 && !self.context.is_empty() { return Err(RuleSyntaxError::UnbalancedRuleEnv(self.context.clone())) }
        if self.except.len()  != max && self.except.len()  != 1 && !self.except.is_empty()  { return Err(RuleSyntaxError::UnbalancedRuleEnv(self.except.clone()))  }

        // populate subrules, if one's length == 1 then it's value is duplicated to rest of subrules
        let mut sub_vec = Vec::with_capacity(max);
        for i in 0..max {
            let mut input   = if self.input.len()  == 1 {  self.input[0].clone() } else {  self.input[i].clone() };
            let mut output  = if self.output.len() == 1 { self.output[0].clone() } else { self.output[i].clone() };
            let mut context = if self.context.is_empty() { None } else if self.context.len() == 1 { Some(self.context[0].clone()) } else { Some(self.context[i].clone()) };
            let mut except  = if self.except.is_empty()  { None } else if self.except.len()  == 1 { Some( self.except[0].clone()) } else { Some( self.except[i].clone()) };
            let rule_type = {
                match (&input[0].kind, &output[0].kind) {
                    (ParseElement::EmptySet, ParseElement::EmptySet) => return Err(RuleSyntaxError::InsertDelete(input[0].position.group, input[0].position.line, input[0].position.start, output[0].position.start)),
                    (ParseElement::EmptySet, ParseElement::Metathesis) => return Err(RuleSyntaxError::InsertMetath(input[0].position.group, input[0].position.line, input[0].position.start, output[0].position.start)),
                    (ParseElement::EmptySet, ParseElement::MetaOrdered) => return Err(RuleSyntaxError::InsertMetath(input[0].position.group, input[0].position.line, input[0].position.start, output[0].position.start)),
                    (ParseElement::EmptySet, _) => RuleType::Insertion,
                    (_, ParseElement::EmptySet) => RuleType::Deletion,
                    (_, ParseElement::Metathesis) => RuleType::Metathesis,
                    (_, ParseElement::MetaOrdered) => RuleType::MetaOrdered,
                    (..) => RuleType::Substitution  
                }
            };

            if self.prop_rev {
                for i in &mut input  { i.reverse(); }
                input.reverse();
                for o in &mut output { o.reverse(); }
                output.reverse();
                if let Some(cont) = &mut context { cont.reverse(); }
                if let Some(expt) = &mut except  { expt.reverse(); }
            }


            // Ascertain if this subrule has `##` in either its input or environments
            let inp_x_bound = if self.cross_bound_inp {
                if self.input.len() == 1 {
                    true
                } else {
                    let mut x = false;
                    for item in &input { if item.kind == ParseElement::ExtlBound { x = true; break } }
                    x
                }
            } else { false };
            
            let env_x_bound = if self.cross_bound_env { 
                if self.context.len() == 1 && self.except.len() == 1 {
                    true
                } else {
                    self.sub_rule_envs_contain_extl_bound(&context, &except)
                }
            } else { false };
            
            sub_vec.push(
                SubRule {
                    input, 
                    output, 
                    context, 
                    except, 
                    rule_type, 
                    references: RefCell::new(HashMap::new()), 
                    alphas: RefCell::new(HashMap::new()), 
                    is_reversed: self.prop_rev,
                    inp_x_bound,
                    env_x_bound,
                }
            );
        }

        Ok(sub_vec)
    }

    fn sub_rule_envs_contain_extl_bound(&self, context: &Option<EnvItem>, exception: &Option<EnvItem>) -> bool {
        if let Some(envs) = context {
            for env in &envs.envs {
                if env.contains_external() {
                    return true
                }
            }
        }

        if let Some(envs) = exception {
            for env in &envs.envs {
                if env.contains_external() {
                    return true
                }
            }
        }

        false
    }

    pub(crate) fn apply(&self, phrase: Phrase, /*, trace: bool*/) -> Result<Phrase, ASCAError> {
        let sub_rules = self.split_into_subrules()?;
        
        let mut res_phrase = phrase; 
        for i in sub_rules {
            res_phrase = i.apply(res_phrase)?;
        }
        Ok(res_phrase)
    }

    #[allow(unused)]
    pub(crate) fn apply_word(&self, word: Word, /*, trace: bool*/) -> Result<Word, ASCAError> {
        let sub_rules = self.split_into_subrules()?;
        
        let mut res_phrase = Phrase::with_capacity(1);
        res_phrase.push(word); 
        for i in sub_rules {
            res_phrase = i.apply(res_phrase)?;
        }
        Ok(res_phrase[0].clone())
    }
}

impl fmt::Debug for Rule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Rule ->")?;
        writeln!(f, "    Input = [")?;
        for i in self.input.iter() {
            writeln!(f, "        {i:?}")?;
        }
        writeln!(f, "    ]")?;
        writeln!(f, "    Output = [")?;
        for o in self.output.iter() {
            writeln!(f, "        {o:?}")?;
        }
        writeln!(f, "    ]")?;
        writeln!(f, "    Context = [")?;
        for c in self.context.iter() {
            writeln!(f, "        {c}")?;
        }
        writeln!(f, "    ]")?;
        writeln!(f, "    Exception = [")?;
        for e in self.except.iter() {
            writeln!(f, "        {e}")?;
        }
        writeln!(f, "    ]")?;

        Ok(())
    }
}

#[cfg(test)]
mod tests;
