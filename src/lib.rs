mod lexer;
mod trie;
mod parser;
mod word;
mod syll;
mod seg;
mod rule;
mod subrule;
mod error;
mod alias;

pub use error::*;

use serde::Deserialize;
use std::collections::HashMap;
use lazy_static::lazy_static;
use wasm_bindgen::prelude::*;

use alias ::{lexer::AliasLexer, parser::AliasParser, AliasKind, Transformation};
use lexer ::*;
use parser::*;
use trie  ::*;
use word  ::*;
use seg   ::*;
use rule  ::*;

const CARDINALS_FILE: &str = include_str!("cardinals.json");
const DIACRITIC_FILE: &str = include_str!("diacritics.json");
lazy_static! {
    static ref CARDINALS_MAP: HashMap<String, Segment> = serde_json::from_str(CARDINALS_FILE).unwrap();
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
    static ref CARDINALS_VEC: Vec<String> = {
        let mut m = Vec::new();
        CARDINALS_MAP.iter().for_each(|(k,_)| {
            m.push(k.clone());
        });

        m
    };
    static ref CARDINALS_TRIE: Trie = {
        let mut m = Trie::new();
        CARDINALS_MAP.iter().for_each(|(k,_)| {
            m.insert(k.as_str());
        });

        m
    };    
}


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

    pub fn from<T: Into<String>>(name: T, rule: Vec<String>, description: T) -> Self {
        Self { name: name.into(), rule, description: description.into() }
    }

    pub fn from_rules(rule: Vec<String>) -> Self {
        Self { name: String::new(), rule, description: String::new() }
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


// We are currently only normalising a few characters as most would be invalid anyway.
pub(crate) fn normalise(s: &str) -> String {
    let mut output = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            'ã' => output.push_str("ã"),
            'ẽ' => output.push_str("ẽ"),
            'ĩ' => output.push_str("ĩ"),
            'õ' => output.push_str("õ"),
            'ũ' => output.push_str("ũ"),
            'ỹ' => output.push_str("ỹ"),
            'ℇ' => output.push_str("ɛ"),
            'ꭤ' => output.push_str("ɑ"),
            'ǝ' => output.push_str("ə"),
            'ɚ' => output.push_str("ə˞"),
            'ɝ' => output.push_str("ɜ˞"),
            'ℎ' => output.push_str("h"),
            'ℏ' => output.push_str("ħ"),
            // 'ﬁ' => output.push_str("fi"),
            // 'ﬂ' => output.push_str("fl"),
            // 'ĳ' => output.push_str("ij"),
            // 'ǌ' => output.push_str("nj"),
            // 'ǉ' => output.push_str("lj"),
            _ => {
                output.push(ch);
            }
        }
    }

    output
}

fn apply_rule_groups(rules: &[Vec<Rule>], words: &[Word]) -> Result<Vec<Word>, Error> {
    let mut transformed_words: Vec<Word> = Vec::with_capacity(words.len());

    for word in words {
        let mut res_word = word.clone();

        for rule_group in rules {
            for rule in rule_group {
                res_word = rule.apply(res_word)?;
            }
        }
        transformed_words.push(res_word);
    }

    Ok(transformed_words)
}

fn words_to_string(words: Vec<Word>, alias_from: Vec<Transformation>) -> Result<Vec<String>, WordRuntimeError> {
    let mut wrds_str: Vec<String> = Vec::with_capacity(words.len());
    for w in words {
        wrds_str.push(w.render(&alias_from));
    }
    Ok(wrds_str)
}

fn parse_words(unparsed_words: &[String], alias_into: &[Transformation]) -> Result<Vec<Word>, Error> {
    let mut words: Vec<Word> = Vec::with_capacity(unparsed_words.len());
    for w in unparsed_words {
        words.push(Word::new(normalise(w), alias_into)?);
    }
    Ok(words)
}

fn parse_rule_groups(unparsed_rule_groups: &[RuleGroup]) -> Result<Vec<Vec<Rule>>, RuleSyntaxError> {
    let mut rule_groups = Vec::with_capacity(unparsed_rule_groups.len());

    for (rgi, rg) in unparsed_rule_groups.iter().enumerate() {
        let mut rule_group = Vec::with_capacity(rg.rule.len());
        for (ri, r) in rg.rule.iter().enumerate() {
            if let Some(asdf) = Parser::new(Lexer::new(&r.chars().collect::<Vec<_>>(), rgi, ri).get_line()?, rgi, ri).parse()? {
                rule_group.push(asdf);
            }
        }
        rule_groups.push(rule_group);
    }

    Ok(rule_groups)
}

fn parse_aliases(into: &[String], from: &[String]) -> Result<(Vec<Transformation>, Vec<Transformation>), Error> {
    let mut into_parsed = Vec::with_capacity(into.len());
    for (line, alias) in into.iter().enumerate() {
        into_parsed.extend(AliasParser::new(AliasKind::Deromaniser, AliasLexer::new(AliasKind::Deromaniser, &alias.chars().collect::<Vec<_>>(), line).get_line()?, line).parse()?);
    }
    
    let mut from_parsed = Vec::with_capacity(from.len());
    for (line, alias) in from.iter().enumerate() {
        from_parsed.extend(AliasParser::new(AliasKind::Romaniser, AliasLexer::new(AliasKind::Romaniser, &alias.chars().collect::<Vec<_>>(), line).get_line()?, line).parse()?);
    }

    Ok((into_parsed, from_parsed))
}

pub fn run(unparsed_rules: &[RuleGroup], unparsed_words: &[String], alias_into: &[String], alias_from: &[String]) -> Result<Vec<String>, Error> {
    let (alias_into, alias_from) = parse_aliases(alias_into, alias_from)?;
    
    let words = parse_words(unparsed_words, &alias_into)?;
    let rules = parse_rule_groups(unparsed_rules)?;
    let res = apply_rule_groups(&rules, &words)?;

    Ok(words_to_string(res, alias_from)?)
}


#[wasm_bindgen]
pub fn run_wasm(val: JsValue, unparsed_words: Vec<String>, unparsed_into: Vec<String>, unparsed_from: Vec<String>) -> Vec<String> {
    let unparsed_rules: Vec<RuleGroup> = serde_wasm_bindgen::from_value(val).expect("Rules are in valid JSObject format");

    parse_result_web(run(&unparsed_rules, &unparsed_words, &unparsed_into, &unparsed_from), &unparsed_rules, &unparsed_words, &unparsed_into, &unparsed_from)
}

fn parse_result_web(unparsed_result: Result<Vec<String>, Error>, rules: &[RuleGroup], words: &[String], unparsed_into: &[String], unparsed_from: &[String]) -> Vec<String> {
    let mut res = Vec::new();
    match unparsed_result {
        Ok(output) => {
            for o in output {
                res.push(o);
            }
        },
        Err(err) => match err {
            Error::WordSyn(e) => res.push(e.format_word_error(words)),
            Error::WordRun(e) => res.push(e.format_word_error(words)),
            Error::AliasSyn(e) => res.push(e.format_alias_error(unparsed_into, unparsed_from)),
            Error::AliasRun(e) => res.push(e.format_alias_error(unparsed_into, unparsed_from)),
            Error::RuleSyn(e) => res.push(e.format_rule_error(rules)),
            Error::RuleRun(e) => res.push(e.format_rule_error(rules)),
        },
    }

    res
}
