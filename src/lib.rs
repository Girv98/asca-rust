mod trie;
pub mod word;
pub mod rule;
pub mod error;
mod alias;

use indexmap::IndexMap;
use serde::Deserialize;
use std::collections::HashMap;
use lazy_static::lazy_static;
use wasm_bindgen::prelude::*;

use alias :: Transformation;
use trie  :: *;
use word  :: { DiaMods, Diacritic, * };
use error :: { ASCAError,  * };
use rule  :: { trace::Change, BinMod, ModKind, Rule, RuleGroup };

const CARDINALS_FILE: &str = include_str!("cardinals.json");
const DIACRITIC_FILE: &str = include_str!("diacritics.json");
lazy_static! {
    static ref CARDINALS_MAP: IndexMap<String, Segment> = serde_json::from_str(CARDINALS_FILE).unwrap();
    static ref DIACRITS: Vec<Diacritic> = {
        // this seems very unnecessary, but I don't know enough about serde
        // at least it works
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Deserialize, Hash)]
        pub enum DiaFeatType {
            Root, Manner, Laryngeal, Place, Labial, Coronal, Dorsal, Pharyngeal, 
            /*RUT*/ Consonantal, Sonorant, Syllabic,      
            /*MAN*/ Continuant, Approximant, Lateral, Nasal, DelayedRelease, Strident, Rhotic, Click,          
            /*LAR*/ Voice, SpreadGlottis, ConstrGlottis,   
            /*LAB*/ Labiodental, Round,          
            /*COR*/ Anterior, Distributed,     
            /*DOR*/ Front, Back, High, Low, Tense, Reduced,        
            /*PHR*/ AdvancedTongueRoot, RetractedTongueRoot, 
        }
        
        #[derive(Deserialize)]
        struct DT {
            pub name: String,
            pub diacrit: char,
            pub prereqs: Option<HashMap<DiaFeatType, bool>>,
            pub payload: Option<HashMap<DiaFeatType, bool>>,
        }

        impl DT {
            pub fn hm_to_mod(&self, hm: &Option<HashMap<DiaFeatType, bool>>) -> DiaMods {
                let mut args = DiaMods::new();
                // if hm.is_none() {return args};
                let Some(s) = hm else {return args};
                for (key, value) in s.iter() {
                    let x = *key as usize;
                    match value {
                        true =>{
                            if x > 7 { args.feats[x - 8] = Some(ModKind::Binary(BinMod::Positive)) }
                            else { args.nodes[x] = Some(ModKind::Binary(BinMod::Positive)) };
                        },
                        false => {
                            if x > 7 { args.feats[x - 8] = Some(ModKind::Binary(BinMod::Negative)) } 
                            else { args.nodes[x] = Some(ModKind::Binary(BinMod::Negative)) };
                        }
                    }
                }
                args
            }

            pub fn to_diacritic(&self) ->  Diacritic {
                Diacritic { 
                    name: self.name.clone(), 
                    diacrit: self.diacrit, 
                    prereqs: self.hm_to_mod(&self.prereqs), 
                    payload: self.hm_to_mod(&self.payload)
                }
            }
        }

        let dt: Vec<DT> = serde_json::from_str(DIACRITIC_FILE).unwrap();

        dt.iter().map(|x| x.to_diacritic()).collect()
    };
    static ref CARDINALS_VEC: Vec<String> = CARDINALS_MAP.keys().cloned().collect();
    static ref CARDINALS_TRIE: Trie = {
        let mut m = Trie::new();
        CARDINALS_MAP.keys().for_each(|k| m.insert(k.as_str()));
        m
    };    
}

fn apply_rule_groups(rules: &[Vec<Rule>], phrases: &[Phrase]) -> Result<Vec<Phrase>, ASCAError> {
    let mut transformed_phrases: Vec<Phrase> = Vec::with_capacity(phrases.len());

    for phrase in phrases {
        transformed_phrases.push(phrase.apply_all(rules)?);
    }

    Ok(transformed_phrases)
}

fn apply_rules_trace(rules: &[Vec<Rule>], phrase: &Phrase) -> Result<Vec<Change>, ASCAError> {   
    let mut changes: Vec<Change> = Vec::new();

    let mut res_phrase = phrase.clone();
    for (i, rule_group) in rules.iter().enumerate() {
        let res_step = res_phrase.clone();
        for rule in rule_group {
            res_phrase = rule.apply(res_phrase)?;
        }
        if res_phrase != res_step {
            changes.push(Change { rule_index: i, after: res_phrase.clone() });
        }
    }

    Ok(changes)
}

fn phrases_to_string(phrases: Vec<Phrase>, alias_from: Vec<Transformation>) -> (Vec<String>, Vec<String>) {
    let mut res = Vec::with_capacity(phrases.len());
    let mut unknowns = Vec::new();

    for phrase in phrases {
        let mut phr_res = String::new();
        for word in phrase.iter() {
            let (w, u) = word.render(&alias_from);
            phr_res.push(' ');
            phr_res.push_str(&w);
            unknowns.extend(u);
        }
        res.push(phr_res.trim().to_string());
    }

    (res, unknowns)
}

fn input_phrases_to_string(phrases: &[Phrase]) -> Vec<String> {
    let mut res = Vec::with_capacity(phrases.len());

    for phrase in phrases {
        let mut phr_res = String::new();
        for word in phrase.iter() {
            let (w, _) = word.render(&[]);
            phr_res.push(' ');
            phr_res.push_str(&w);
        }
        res.push(phr_res.trim().to_string());
    }

    res
}

fn parse_phrases(unparsed_phrases: &[String], alias_into: &[Transformation]) -> Result<Vec<Phrase>, ASCAError> {
    unparsed_phrases.iter().map(|phrase| -> Result<Phrase, ASCAError> {
        phrase.split(' ')
        .map(|w| Word::new(w, alias_into))
        .collect()
    }).collect()
}

fn parse_rule_groups(unparsed_rule_groups: &[RuleGroup]) -> Result<Vec<Vec<Rule>>, RuleSyntaxError> {
    let mut rule_groups = Vec::with_capacity(unparsed_rule_groups.len());

    for (rgi, rg) in unparsed_rule_groups.iter().enumerate() {
        let mut rule_group = Vec::with_capacity(rg.rule.len());
        for (ri, r) in rg.rule.iter().enumerate() {
            if let Some(asdf) = rule::Parser::new(rule::Lexer::new(&r.chars().collect::<Vec<_>>(), rgi, ri).get_line()?, rgi, ri).parse()? {
                rule_group.push(asdf);
            }
        }
        rule_groups.push(rule_group);
    }

    Ok(rule_groups)
}

pub fn run_unparsed(unparsed_rules: &[RuleGroup], unparsed_phrases: &[String], alias_into: &[String], alias_from: &[String]) -> Result<Vec<String>, ASCAError> {
    let phrases = parse_phrases(unparsed_phrases, &alias::parse_into(alias_into)?)?;
    let rules = parse_rule_groups(unparsed_rules)?;
    let res = apply_rule_groups(&rules, &phrases)?;

    let (res, _) = phrases_to_string(res, alias::parse_from(alias_from)?);

    Ok(res)
}

#[allow(clippy::type_complexity)]
pub fn run_unparsed_debug(unparsed_rules: &[RuleGroup], unparsed_phrases: &[String], alias_into: &[String], alias_from: &[String]) -> Result<(Vec<String>, Vec<String>, Vec<String>), ASCAError> {
    let phrases = parse_phrases(unparsed_phrases, &alias::parse_into(alias_into)?)?;
    let rules = parse_rule_groups(unparsed_rules)?;
    let res = apply_rule_groups(&rules, &phrases)?;
    
    let (output, unknowns) = phrases_to_string(res, alias::parse_from(alias_from)?);

    Ok((input_phrases_to_string(&phrases), output, unknowns))
}

// For interop with WebASCA

#[doc(hidden)]
#[wasm_bindgen]
pub struct WasmResult {
    input: Vec<String>,         // parsed input
    output: Vec<String>,
    unknowns: Vec<String>,      // Any segments that were unable to be parsed
    trace_rules: Vec<usize>,    // Indices of rules which were applied
    was_ok: bool                // Did we error, or succeed?
}

#[wasm_bindgen]
impl WasmResult {
    pub fn get_input(&self) -> Vec<String> {
        self.input.clone()
    }

    pub fn get_output(&self) -> Vec<String> {
        self.output.clone()
    }

    pub fn get_unknowns(&self) -> Vec<String> {
        self.unknowns.clone()
    }

    pub fn get_traces(&self) -> Vec<usize> {
        self.trace_rules.clone()
    }

    pub fn was_ok(&self) -> bool {
        self.was_ok
    }
}


#[wasm_bindgen]
pub fn run_wasm(val: JsValue, unparsed_phrases: Vec<String>, unparsed_into: Vec<String>, unparsed_from: Vec<String>, trace: Option<usize>) -> WasmResult {
    let unparsed_rules: Vec<RuleGroup> = serde_wasm_bindgen::from_value(val).expect("Rules are in valid JSObject format");
    
    match trace {
        Some(t) => parse_result_web(run_trace_wasm(&unparsed_rules, &unparsed_phrases, &unparsed_into, t), &unparsed_rules, &unparsed_into, &unparsed_from, &unparsed_phrases),
        None => match run_unparsed_debug(&unparsed_rules, &unparsed_phrases, &unparsed_into, &unparsed_from) {
            Ok((inp, res, unk)) => parse_result_web(Ok((inp, res, unk, vec![])), &unparsed_rules, &unparsed_into, &unparsed_from, &unparsed_phrases),
            Err(e) => parse_result_web(Err(e), &unparsed_rules, &unparsed_into, &unparsed_from, &unparsed_phrases),
        }
    }
}

fn get_trace_phrase(unparsed_phrases: &[String], alias_into: &[String], trace_index: usize) -> Result<Option<Phrase>, ASCAError> {
    match unparsed_phrases.get(trace_index) {
        Some(phrase) => Ok(Some(Phrase::try_from(phrase, alias_into)?)),
        None => Ok(None),
    }
}

#[allow(clippy::type_complexity)]
fn run_trace_wasm(unparsed_rules: &[RuleGroup], unparsed_phrase: &[String], alias_into: &[String], trace_index: usize) -> Result<(Vec<String>, Vec<String>, Vec<String>, Vec<usize>), ASCAError> {
    let phrase = get_trace_phrase(unparsed_phrase, alias_into, trace_index)?.unwrap_or_default();
    let rules = parse_rule_groups(unparsed_rules)?;
    let res = apply_rules_trace(&rules, &phrase)?;

    let (out, unk, trc) = rule::trace::to_string_wasm(&phrase, res, unparsed_rules);

    Ok((input_phrases_to_string(&[phrase]), out, unk, trc))
}

#[allow(clippy::type_complexity)]
fn parse_result_web(result: Result<(Vec<String>, Vec<String>, Vec<String>, Vec<usize>), ASCAError>, rules: &[RuleGroup], unparsed_into: &[String], unparsed_from: &[String], unparsed_phrases: &[String]) -> WasmResult {
    match result {
        Ok((input, output, unknowns, trace_rules)) => {
            WasmResult { input, output, unknowns, trace_rules, was_ok: true}
        },
        Err(err) => {
            let output = match err {
                ASCAError::WordSyn(e) => e.format(),
                ASCAError::AliasSyn(e) => e.format(unparsed_into, unparsed_from),
                ASCAError::AliasRun(e) => e.format(unparsed_into, unparsed_from),
                ASCAError::RuleSyn(e) => e.format(rules),
                ASCAError::RuleRun(e) => e.format(rules),
            };
            WasmResult { input: unparsed_phrases.to_vec(), output: vec![output], unknowns: vec![], trace_rules: vec![], was_ok: false }
        },
    }    
}
