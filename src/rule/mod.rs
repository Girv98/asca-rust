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
mod rule_tests {
    use crate::rule::{Lexer, Parser, RuleGroup};

    use super::*;
    
    fn run(test_rule: &str, test_input: &str, expt_output: &str) -> bool {
        let rule = setup_rule(test_rule);
        let phrase = setup_phrase(test_input);

        match rule.apply(phrase) {
            Ok(out) => if out.render() == expt_output { true } else {
                println!("-----------------------");
                println!("expt: {}", expt_output);
                println!("actl: {}", out.render());
                println!("-----------------------");
                false
            },
            Err(e) => {
                let rg = RuleGroup { name: String::new(), rule: vec![test_rule.to_owned()], description: String::new() };
                println!("{}", e.format_rule_error(&vec![rg]));
                false
            },
        }
    }

    fn setup_rule(test_str: &str) -> Rule {
        let maybe_lex = Lexer::new(&test_str.chars().collect::<Vec<_>>(),0 ,0).get_line();
        match maybe_lex {
            Ok(lexed) => {
                match Parser::new(lexed, 0, 0).parse() {
                    Ok(rule) => return rule.unwrap(),
                    Err(e) => {
                        let rg = RuleGroup { name: String::new(), rule: vec![test_str.to_owned()], description: String::new() };
                        println!("{}", e.format(&vec![rg]));
                        assert!(false);
                    },
                }
            },
            Err(e) => {
                let rg = RuleGroup { name: String::new(), rule: vec![test_str.to_owned()], description: String::new() };
                println!("{}", e.format(&vec![rg]));
                assert!(false);
            },
        } 
        unreachable!()
    }

    fn setup_word(test_str: &str) -> Word {
        match Word::new(test_str) {
            Ok(w) => return w,
            Err(e) => {
                println!("{}", e.format_word_error());
                assert!(false);
            },
        }
        unreachable!();
    }

    fn setup_phrase(test_str: &str) -> Phrase {
        match Phrase::try_from(test_str, &[]) {
            Ok(p) => return p,
            Err(e) => {
                println!("{}", e.format_word_error());
                assert!(false);
            },
        }
        unreachable!();
    }

    #[test]
    fn test_wildcard() {
        assert!(run("[] > x", "a.r.i.s.a.n", "x.x.x.x.x.x"));
        assert!(run("[] > x", "arisan",      "xːːːːː"));

        assert!(run("{[], []} > {x,w}", "a.r.i.s.a.n", "x.x.x.x.x.x"));
        assert!(run("{[], []} > {x,w}", "arisan",      "xːːːːː"));
        
        assert!(run("{[], []} > x", "a.r.i.s.a.n", "x.x.x.x.x.x"));
        assert!(run("{[], []} > w", "a.r.i.s.a.n", "w.w.w.w.w.w"));

        assert!(run("{[], []} > {w,x}", "a.r.i.s.a.n", "w.w.w.w.w.w"));
        assert!(run("{[], []} > {w,x}", "arisan",      "wːːːːː"));

        assert!(run("[]=1 > 1:[+red]", "a.r.i.s.a.n", "aᵊ.rᵊ.iᵊ.sᵊ.aᵊ.nᵊ"));
        assert!(run("[]=1 > 1:[+red]", "arisan",      "aᵊrᵊiᵊsᵊaᵊnᵊ"));

        assert!(run("[] > [+red]", "a.r.i.s.a.n", "aᵊ.rᵊ.iᵊ.sᵊ.aᵊ.nᵊ"));
        assert!(run("[] > [+red]", "arisan",      "aᵊrᵊiᵊsᵊaᵊnᵊ"));
    }


    #[test]
    fn test_semivowel_syllabication() {
        assert!(run("[-syll, +approx, -lat, Ahi] > [+syll, +son, -cons, +lab, - PHR, Atense]", "ʕ̞.w.ʟ", "ɑ.u.ʟ"));

        assert!(run("[+approx], [+approx, +hi] > [+syll, +son, -cons, +lab, -phr], [+tense]", "w.ʕ̞", "u.ɑ"));

        assert!(run("[+approx, +hi] > [+syll, +tense]", "wj", "ui"));

        assert!(run("V > a", "e.o", "a.a"));

        assert!(run("V > [-tns]", "e.o", "ɛ.ɔ"));
    }

    #[test]
    fn test_spec_env() {
        assert!(run("V > * / _,#",  "a.ri.sa", "ri.s"));
        assert!(run("C > * / _,#a", "a.ri.sa", "a.i.a"));
    }

    #[test]
    fn test_debuccalisation() {
        assert!(run("O:[-voi] > [-cons, +c.g., -place]", "p", "ʔ"));

        let test_rule = "O:[-voi, Acont] > [-cons, As.g., -Ac.g., -place, -strid]";
        assert!(run(test_rule, "pa.sa.ta.fa", "ʔa.ha.ʔa.ha"));
        assert!(run(test_rule, "sa.pa.fa.pa", "ha.ʔa.ha.ʔa"));
    }

    #[test]
    fn test_greek_regressive_voicing() {
        let test_rule = "O > [Alar] / _O:[Alar]";
        assert!(run(test_rule, "at.ba",  "ad.ba"));
        assert!(run(test_rule, "ktʰoːn", "kʰtʰoːn"));
    }

    #[test]
    fn test_germanic_a_mutation() {
        // let test_rule = "V:[+hi] > [-hi] / _ (C,2) V:[+lo] | _{j, N}";
        // let test_rule = "V:[+hi] > [-hi] / _ C (C) V:[+lo] | _{j, NC}";
        let test_rule = "V:[+hi] > [-hi] / _ (C,1:2) V:[+lo] | _{j, NC}";
        assert!(run(test_rule, "ˈwur.ðɑ̃",       "ˈwor.ðɑ̃"));
        assert!(run(test_rule, "ˈne.stɑz",      "ˈne.stɑz"));
        assert!(run(test_rule, "ˈwi.rɑz",       "ˈwe.rɑz"));
        assert!(run(test_rule, "ˈhur.nɑ̃",       "ˈhor.nɑ̃"));
        assert!(run(test_rule, "ˈnun.dɑz",      "ˈnun.dɑz"));
        assert!(run(test_rule, "ˈswem.mɑ.nɑ̃",   "ˈswem.mɑ.nɑ̃"));
        assert!(run(test_rule, "ˈgul.ðɑ̃",       "ˈɡol.ðɑ̃"));
        assert!(run(test_rule, "ˈgul.ði.jɑ.nɑ", "ˈɡul.ði.jɑ.nɑ"));
        assert!(run(test_rule, "ˈwird.pɑz",     "ˈwird.pɑz"));
    }

    #[test]
    fn test_considerations() {
        assert!(run("a > e", "hat",  "het"));
        assert!(run("a > e", "ha:t", "heːt"));
        // assert!(run("a > e", "ha:t", "het")); // Previous behaviour
        
        assert!(run("a > ee",        "ha:t", "heːːːt")); // a a > ee ee
        assert!(run("a:[+lng] > ee", "ha:t", "heːt"));   // aa > ee
        assert!(run("a:[+lng] > e",  "ha:t", "het"));    // aa > e

        assert!(run("a > [+fr, -lo, +tns]", "ha:t", "heːt"));
        assert!(run("a:[Alen] > e:[Alen]",  "ha:t", "heːt"));
        
        
        assert!(run("V > e",          "hat", "het"));
        assert!(run("V > e",          "ha:t", "het"));

        assert!(run("V > ee",         "ha:t", "heːt"));
        assert!(run("V:[+long] > ee", "ha:t", "heːt"));
        assert!(run("V:[+long] > e",  "ha:t", "het"));

        assert!(run("V > [+fr, -lo, +tns]", "ha:t", "heːt"));
        assert!(run("V:[Alen] > e:[Alen]",  "ha:t", "heːt"));
    }

    #[test]
    fn test_sub_simple_ipa() {
        assert!(run("r > l",       "la.ri.sa", "la.li.sa"));
        assert!(run("a > e",       "hat", "het"));
        assert!(run("V > e",       "ha:t", "het"));
        assert!(run("a:[+lo] > e", "hat", "het"));
        assert!(run("a:[+lo] > e", "ha:t", "heːt"));

        assert!(run("a:[-long] > e",              "hat", "het"));
        assert!(run("a:[-long] > e:[-long]",      "hat", "het"));
        assert!(run("a:[-long] > e:[+long]",      "hat", "heːt"));
        assert!(run("a:[-long] > e:[+vlong]",     "hat", "heːːt"));
        assert!(run("a:[+long] > e",              "ha:t", "het"));
        assert!(run("a:[Along] > e:[Along]",      "ha:t", "heːt"));
        assert!(run("a > [+fr, -lo, +tns]",       "ha:t", "heːt"));
        assert!(run("V:[+lo] > [+fr, -lo, +tns]", "ha:t", "heːt"));
        
        assert!(run("V:[Along] > e:[Along]",   "hat",  "het"));
        assert!(run("V:[Along] > e:[Along]",   "ha:t", "heːt"));
        assert!(run("V:[Along] > e:[Avlong]",  "ha:t", "heːːt"));
        assert!(run("V:[Along] > e:[-Avlong]", "ha:t", "heːt"));
        assert!(run("V:[Along] > e:[-Avlong]", "hat",  "heːːt"));
        assert!(run("V:[-long] > e:[+vlong]",  "hat",  "heːːt"));
        assert!(run("V:[+long] > e:[+vlong]",  "ha:t", "heːːt"));
        assert!(run("V:[-long] > e:[+vlong]",  "ha:t", "haːt"));
    
        assert!(run("V:[+long] > e:[-long]", "ha::t",   "het"));
        assert!(run("V:[+long] > e",         "ha::t",   "het"));
    }
    
    #[test]
    fn test_grouped_length() {
        assert!(run("V:[Alen] > e:[Alen]",      "ha:t",   "heːt"));
        assert!(run("V:[Alen] > e:[Alen]",      "ha::t",  "heːːt"));
        assert!(run("V:[Alen] > e:[Alen]",      "ha:::t", "heːːːt"));
        assert!(run("a:[+lo, Alen] > e:[Alen]", "ha:t",   "heːt"));

        assert!(run("V:[Alen] > e:[Alen] / <CVC>h_",        "dan.ha:t",  "dan.heːt"));
        assert!(run("V:[Alen] > e:[Alen] / <CV:[Alo]C>h_",  "dan.ha:t",  "dan.heːt"));
        assert!(run("V:[Alen] > e:[Alen] / <CV:[Alo]C>h_",  "da:n.ha:t", "daːn.heːt"));
        assert!(run("V:[Alen] > e:[Alen] / <CV:[Alen]C>h_", "da:n.ha:t", "daːn.heːt"));
    }

    #[test]
    fn test_sub_syll_bound_for_ipa() {
        assert!(run("$ > a | :{#_,_#}:", "es.a",  "esaː"));
        
        assert!(run("$ > a", "es.me",  "aesamea"));
        assert!(run("$ > a", "es.e",   "aesaea"));
        assert!(run("$ > a", "es.a",   "aesaːː"));
        assert!(run("$ > a", "sen.me", "asenamea"));
    }

    #[test]
    fn test_sub_insert_length() {
        assert!(run("Vr > [+long]l", "dark", "daːlk"));
    }

    #[test]
    fn test_sub_insert_ipa() {
        assert!(run("a > eoi", "dak", "deoik"));
    }

    #[test]
    fn test_sub_ipa_for_syll_bound() {
        assert!(run("a > $",         "deane",    "de.ne"));
        assert!(run("a:[+long] > $", "dea:ne",   "de.ne"));
        assert!(run("V > $",         "sen.me.a", "s.n.m"));
        assert!(run("$ > c$",        "sa.a",     "c.sac.ac"));
        assert!(run("$ > c$",        "sen.me.a", "c.senc.mec.ac"));
    }

    #[test]
    fn test_sub_insert_syll_bound() {
        assert!(run("a > a$e", "'dak",    "ˈda.ek"));
        assert!(run("a > a$e", "'dak.mo", "ˈda.ek.mo"));
        
        assert!(run("k > k$",  "dak", "dak"));
        assert!(run("k > k$",  "kak", "k.ak"));
        assert!(run("k > k$a", "dak", "dak.a"));
    }

    #[test]
    fn add_length_to_matrix() {
        assert!(run("V > [+long]",      "pe.ma", "peː.maː"));
        assert!(run("V > [+long] / _#", "pe.ma", "pe.maː"));
        assert!(run("V > [+long] | _#", "pe.ma", "peː.ma"));

        assert!(run("V > [+overlong]",      "pe.ma", "peːː.maːː"));
        assert!(run("V > [+overlong] / _#", "pe.ma", "pe.maːː"));
        assert!(run("V > [+overlong] | _#", "pe.ma", "peːː.ma"));
        
        assert!(run("[] > [+long]",     "pe.ma", "pːeː.mːaː"));
        assert!(run("[] > [+overlong]", "pe.ma", "pːːeːː.mːːaːː"));

        assert!(run("V:[-long] > [+long]", "pe:.ma", "peː.maː"));
        assert!(run("V:[+long] > [+long]", "pe:.ma", "peː.ma"));

        assert!(run("V > [+long]", "pe:.ma", "peː.maː"));
    }

    #[test]
    fn test_long_vowel_breaking() {
        assert!(run("e > ie",         "'pe.ma",  "ˈpie.ma"));
        assert!(run("e:[+long] > ie", "'pe:.ma", "ˈpie.ma"));

        assert!(run("V:[-long, -hi, -lo]=1 > 1:[+hi] 1", "'pe.ma", "ˈpie.ma"));

        let test_rule = "V:[+long, -hi, -lo]=1 > 1:[+hi, -long] 1:[-long]";
        assert!(run(test_rule, "'pe:.ma", "ˈpie.ma"));
        assert!(run(test_rule, "'po:.ma", "ˈpuo.ma"));

        let test_rule = "V:[+hi, +long]=1 > ə1:[-long]";
        assert!(run(test_rule, "'i:s", "ˈəis"));
        assert!(run(test_rule, "'hu:s", "ˈhəus"));
    }

    #[test]
    fn test_spanish_breaking() {
        let test_rule = "V:[+str,-lng, -hi, -lo]=1 > 1:[+hi] e";
        assert!(run(test_rule, "'pe.dra", "ˈpie.dra"));
        assert!(run(test_rule, "'fo.go", "ˈfue.ɡo"));
        assert!(run(test_rule, "'fes.ta", "ˈfies.ta"));
        assert!(run(test_rule, "'por.to", "ˈpuer.to"));
    }

    #[test]
    fn test_match_stress() {
        assert!(run("V > [Astress] / _C:[Astr]", "pe'sa", "ˈpeˈsa"));

        let test_rule = "V:[Astress] > [+long] / _C:[Astr]";
        assert!(run(test_rule, "'pe'sa",       "ˈpeːˈsa"));
        assert!(run(test_rule, "pe'sa",        "peˈsa"));
        assert!(run(test_rule, "sa'pe.sa.so",  "saˈpe.saː.so"));
        assert!(run(test_rule, "sa'pe'sa.so",  "saˈpeːˈsa.so"));
        assert!(run(test_rule, "'sa'pe'sa.so", "ˈsaːˈpeːˈsa.so"));
    }

    #[test]
    fn test_empty_word() {
        assert!(run("* > z / #wej_#", "ˈwej",      "ˈwejz"));
        // assert!(run("* > z / #wej_#", " ˈwej",     " ˈwejz")); // TODO
        assert!(run("* > z / #wej_#", "ˈeg  ˈwej", "ˈeɡ  ˈwejz"));
    }

    #[test]
    fn test_sub_syll_ref() {
        assert!(run("% > 1:[-str] / %:[+str]=1_", "keˈsa.lo", "keˈsa.sa"));
    }

    #[test]
    fn aasdasd() {
        assert!(run(" %  > <de>", "sa.va", "de.de"));
        assert!(run("{%} > <de>", "sa.va", "de.de"));
        // assert!(run("% > de", "sa.va", "de.de"));
        // assert!(run("{%} > de", "sa.va", "de.de"));
    }

    #[test]
    fn test_sub_assim() {
        assert!(run("V > [αround] / _C[αround]", "le.ro", "lø.ro"));
    }

    #[test]
    fn test_sub_turkish_suffix_vowel_harmony() {
        let test_rule = "V:[+hi] > [αbk, βfr, γrnd] / V:[αbk, βfr, γrnd] (C,0) _ (C) #";
        assert!(run(test_rule, "desun",  "desin"));
        assert!(run(test_rule, "røstin", "røstyn"));
        assert!(run(test_rule, "kɨzlik", "kɨzlɨk"));
        assert!(run(test_rule, "sɨdyn",  "sɨdɨn"));
        assert!(run(test_rule, "sodɨn",  "sodun"));
        assert!(run(test_rule, "dasin",  "dasɨn"));
    }

    #[test]
    fn test_optional_bounded() {
        assert!(run("a > e / _(C,3:6)", "ak.ka.k.k.k", "ak.ke.k.k.k"));
        assert!(run("a > e / (C,3:6)_", "k.k.k.ak.ka", "k.k.k.ek.ka"));
    }

    #[test]
    fn test_optional_unbounded() {
        let test_rule = "V > [-back, +front, +tense] / _(..) V:[+hi, -back]";
        assert!(run(test_rule, "aki", "æki"));
        assert!(run(test_rule, "ak.k.k.ki", "æk.k.k.ki"));
    }

    #[test]
    fn test_sub_del_ipa() {
        assert!(run("sk > ʃ", "skip", "ʃip"));
    }

    #[test]
    fn test_sub_del_ipa_bordering() {
        assert!(run("sk > ʃ", "skskip", "ʃːip"));
        assert!(run("sk > ʃ", "ask.skip", "aʃ.ʃip"));
    }
    #[test]
    fn test_sub_del_length() {
        assert!(run("Vr > [+lng]",       "dark", "daːk"));
        assert!(run("V:[+lng] > [-lng]", "daːk", "dak"));
    }

    #[test]
    fn test_sub_del_syll_bound() {
        assert!(run("V$ > [+long]", "da.rk", "daːrk"));

        assert!(run("V$ > V", "da.ra.ka", "daraka"));

        assert!(run("V$ > [+long]", "da.ra.ka", "daːraːkaː"));

        assert!(run("V$N > VN", "da.n", "dan"));
        assert!(run("V$N > VN", "da.na.m", "danam"));

        assert!(run("$t > t", "pa.ta.ka", "pata.ka"));
        assert!(run("$t > t$t", "pa.ta.ka", "pat.ta.ka"));
        assert!(run("$t > $tc", "pa.ta.ka", "pa.tca.ka"));
        assert!(run("$t > ts$", "pa.ta.ka", "pats.a.ka"));
    }

    #[test]
    fn test_sub_del_syll() {
        let test_rule = "V% > [+long]";
        assert!(run(test_rule, "da.rk", "daː"));

        let test_rule = "V% > V";
        assert!(run(test_rule, "da.rk", "da"));
        assert!(run(test_rule, "da.rk.a", "da.a"));
        assert!(run(test_rule, "da.rk.a.ka", "da.a"));
    }

    #[test]
    fn test_sub_set_mod() {
        let test_rule = "{p, t, k} > {b, d, g}:[+long]";
        let test_word = "pa.ta.ka";
        assert!(run(test_rule, test_word, "bːa.dːa.ɡːa"));

        let test_rule = "{i, e}:[+long] > [-long]";
        let test_word = "pi:.te.me:";
        assert!(run(test_rule, test_word, "pi.te.me"));
    }

    #[test]
    fn test_sub_set_mult_bound() {
        let test_rule = "{ai, u} > $";
        assert!(run(test_rule, "kai.ta", "k.ta"));
        assert!(run(test_rule, "kain.ta", "k.n.ta"));
        
        let test_rule = "{i<ta>} > $";
        assert!(run(test_rule, "kai.ta.na", "ka.na"));

        let test_rule = "{i<ta>n} > $";
        assert!(run(test_rule, "kai.ta.na", "ka.a"));

        let test_rule = "{i<ta>n$} > $";
        assert!(run(test_rule, "kai.ta.n.a", "ka.a"));

        let test_rule = "{i$t} > $";
        assert!(run(test_rule, "kai.ta.na", "ka.a.na"));
    }

    #[test]
    fn test_sub_set_mult_seg() {
        assert!(run("{p, t, k} > {pb, d, gk}", "pa.ta.ka", "pba.da.ɡka"));
        assert!(run("{p, t, k} > {pp, d, gg}", "pa.ta.ka", "pːa.da.ɡːa"));
    }

    #[test]
    fn test_sub_set_syll_bound() {
        assert!(run("{$, $, $} > {o, o, o}", "pa.ta.ka", "opaotaokao"));
        assert!(run("{$, $, $} > {o, a, e}", "pa.ta.ka", "opaotaokao"));
    }

    #[test]
    fn test_sub_set_mult_syll() {
        let test_rule = "{p, t, k} > {p, <d>j, ɡ}";
        assert!(run(test_rule, "pa.ta.ka", "pa.d.ja.ɡa"));
        let test_rule = "{p, t, k} > {p, j<d>, ɡ}";
        assert!(run(test_rule, "pa.ta.ka", "pa.j.d.a.ɡa"));
        let test_rule = "{p, n, k} > {p, j<d>, ɡ}";
        assert!(run(test_rule, "pan.ta.ka", "paj.d.ta.ɡa"));
        
        let test_rule = "{p, n$t, k} > {b, $d, ɡ}";
        assert!(run(test_rule, "pan.ta.ka", "ba.da.ɡa"));

        let test_rule = "{p, nt, k} > {b, d, ɡ}";
        assert!(run(test_rule, "pan.ta.ka", "bad.a.ɡa"));
    }

    #[test]
    fn test_sub_set() {
        let test_rule = "{p, t, k} > {b, d, g}";
        assert!(run(test_rule, "pa.ta.ka", "ba.da.ɡa"));

        let test_rule = "{%} > {[tone:5]}";
        let test_word = "pa.ta.ka";
        assert!(run(test_rule, test_word, "pa5.ta5.ka5"));

        let test_rule = "{%,C} > {[tone:5],C}";
        let test_word = "pa.ta.ka";
        assert!(run(test_rule, test_word, "pa5.ta5.ka5"));

        let test_rule = "{C, %} > {C, [tone:5]}";
        let test_word = "pa.at.ka";
        assert!(run(test_rule, test_word, "pa.at5.ka"));

        let test_rule = "{<ka>, O, %} > {<la>, O, [tone:5]}";
        let test_word = "pa.at.ka";
        assert!(run(test_rule, test_word, "pa.at5.la"));

        let test_rule = "{k, O, %} > {<l>, O, [tone:5]}";
        let test_word = "pa.at.ka";
        assert!(run(test_rule, test_word, "pa.at5.l.a5"));

        let test_rule = "{%} > {<la>}";
        let test_word = "pa.at.ka";
        assert!(run(test_rule, test_word, "la.la.la"));

        let test_rule = "{$} > {a}";
        let test_word = "p.t.k";
        assert!(run(test_rule, test_word, "apataka"));
        
        let test_rule = "{$} > {$}";
        let test_word = "pa.ta.ka";
        assert!(run(test_rule, test_word, "pa.ta.ka"));

        let test_rule = "{$} > {<u>}";
        let test_word = "pa.ta.ka";
        assert!(run(test_rule, test_word, "u.pa.u.ta.u.ka.u"));

        let test_rule = "{$} > {u}";
        let test_word = "pa.ta.ka";
        assert!(run(test_rule, test_word, "upautaukau"));

        let test_rule = "a{$} > a{u}";
        let test_word = "pa.ta.ka";
        assert!(run(test_rule, test_word, "pautaukau"));

        let test_rule = "V=1{$} > a{1}";
        let test_word = "pa.ta.ka";
        assert!(run(test_rule, test_word, "paːtaːkaː"));

        assert!(run("{p, t, k} > {b, d, g}", "pa.ta.ka", "ba.da.ɡa"));
        // test trailing commas
        assert!(run("{p, t, k} > {b, d, g,}", "pa.ta.ka", "ba.da.ɡa"));
        assert!(run("{p, t, k,} > {b, d, g}", "pa.ta.ka", "ba.da.ɡa"));
        assert!(run("{p, t, k,} > {b, d, g,}", "pa.ta.ka", "ba.da.ɡa"));

        assert!(run("{p} > {b}", "pa.ta.ka", "ba.ta.ka"));        
        assert!(run("{p,} > {b,}", "pa.ta.ka", "ba.ta.ka"));        

    }

    #[test]
    fn test_sub_alpha() {
        let test_rule = "d > [αvoice] / _[αvoice]";
        let test_word = "ad.ha";
        assert!(run(test_rule, test_word, "at.ha"));

        let test_rule = "d > [Avoice] / _[Avoice]";
        let test_word = "ad.ha";
        assert!(run(test_rule, test_word, "at.ha"));

        let test_rule = "k > [αvoice] / [-αvoice]_";
        let test_word = "əs.kɔl";
        assert!(run(test_rule, test_word, "əs.ɡɔl"));

        let test_rule = "k > [Bvoice] / [-Bvoice]_";
        let test_word = "əs.kɔl";
        assert!(run(test_rule, test_word, "əs.ɡɔl"));
    }


    mod meta {
        use super::{setup_rule, setup_word, run};

        #[test]
        fn simple_ipa() {
            let test_rule = "sk > &";
            let test_word = "ˈɑːs.ki.ɑn";
            assert!(run(test_rule, test_word, "ˈɑːk.si.ɑn"));

            let test_rule = "[+rhotic]V > & / _s";
            let test_word = "ˈhros";
            assert!(run(test_rule, test_word, "ˈhors"));

            let test_rule = "oba > &";
            let test_word = "ˈko.ba.lo.ba";
            assert!(run(test_rule, test_word, "ˈka.bo.la.bo"));
        }

        #[test]
        fn seg_different_lengths() {
            let test_rule = "d:[+long] .. s:[+long] > &";
            assert!(run(test_rule, "d:e.s::a", "sːːe.dːa"));
            assert!(run(test_rule, "d:es::a", "sːːedːa"));

            let test_rule = "d .. s:[+long] > &";
            assert!(run(test_rule, "de.s:a", "sːe.da"));
            assert!(run(test_rule, "des:a", "sːeda"));

            let test_rule = "d:[+long] .. s > &";
            assert!(run(test_rule, "d:e.sa", "se.dːa"));
            assert!(run(test_rule, "d:esa", "sedːa"));
        }
        
        #[test]
        fn met_pfas() {
            let test_rule = "pf..s > &";
            assert!(run(test_rule, "pfas", "safp"));
            let test_rule = "{pf}..s > &";
            assert!(run(test_rule, "pfas", "safp"));
        }

        #[test]
        fn met_sapf() {
            let test_rule = "s..pf > &";
            assert!(run(test_rule, "sapf", "fpas"));
        }

        #[test]
        fn ord_pfas() {
            let test_rule = "pf..s > @";
            assert!(run(test_rule, "pfas", "sapf"));
            let test_rule = "{pf}..s > @";
            assert!(run(test_rule, "pfas", "sapf"));
        }

        #[test]
        fn ord_sapf() {
            let test_rule = "s..pf > @";
            assert!(run(test_rule, "sapf", "pfas"));
        }

        #[test]
        fn asdasd() {
            assert!(run("pf..ts..k > &", "pfa.tsa.ka", "ka.sta.fpa"));
            assert!(run("pf..ts..k > @", "pfa.tsa.ka", "ka.tsa.pfa"));

        }

        #[test]
        fn met_abcde_fg() {
            assert!(run("abcde..fɡhi > &", "abcdexfɡhi", "ihɡfxedcba"));
            assert!(run("abcde..fɡh > &", "abcdexfɡh", "hɡfxedcba"));
            assert!(run("abcde..fɡ > &", "abcdexfɡ", "ɡfxedcba"));
            assert!(run("abcd..fɡ > &", "abcdxfɡ", "ɡfxdcba"));
            assert!(run("abc..fɡ > &", "abcxfɡ", "ɡfxcba"));
            assert!(run("abc..f > &", "abcxf", "fxcba"));

            assert!(run("abcd..fɡhi > &", "abcdxfɡhi", "ihɡfxdcba"));
            assert!(run("abc..fɡhi > &", "abcxfɡhi", "ihɡfxcba"));
            assert!(run("ab..fɡhi > &", "abxfɡhi", "ihɡfxba"));
            assert!(run("a..fɡhi > &", "axfɡhi", "ihɡfxa"));
        }

        #[test]
        fn met_abc_def_ghi() {
            assert!(run("abc..def..ghi > &", "abcxdefxɡhi", "ihɡxfedxcba"));
            assert!(run("abcd..ef..ghi > &", "abcdxefxɡhi", "ihɡxfexdcba"));
            assert!(run("abcde..f..ghi > &", "abcdexfxɡhi", "ihɡxfxedcba"));
            assert!(run("abcde..fg..hi > &", "abcdexfɡxhi", "ihxɡfxedcba"));
            assert!(run("abcde..fgh..i > &", "abcdexfɡhxi", "ixhɡfxedcba"));
            assert!(run("abcdef..gh..i > &", "abcdefxɡhxi", "ixhɡxfedcba"));
            assert!(run("abcdefg..h..i > &", "abcdefɡxhxi", "ixhxɡfedcba"));

            assert!(run("abc..de..fghi > &", "abcxdexfɡhi", "ihɡfxedxcba"));
            assert!(run("abc..d..efghi > &", "abcxdxefɡhi", "ihɡfexdxcba"));
            assert!(run("ab..cd..efghi > &", "abxcdxefɡhi", "ihɡfexdcxba"));
            assert!(run("a..bcd..efghi > &", "axbcdxefɡhi", "ihɡfexdcbxa"));
            assert!(run("a..bc..defghi > &", "axbcxdefɡhi", "ihɡfedxcbxa"));
            assert!(run("a..b..cdefghi > &", "axbxcdefɡhi", "ihɡfedcxbxa"));
        }

        #[test]
        fn ord_abc_def_ghi() {
            assert!(run("abc..def..ghi > @", "abcxdefxɡhi", "ɡhixdefxabc"));
            assert!(run("abcd..ef..ghi > @", "abcdxefxɡhi", "ɡhixefxabcd"));
            assert!(run("abcde..f..ghi > @", "abcdexfxɡhi", "ɡhixfxabcde"));
            assert!(run("abcde..fg..hi > @", "abcdexfɡxhi", "hixfɡxabcde"));
            assert!(run("abcde..fgh..i > @", "abcdexfɡhxi", "ixfɡhxabcde"));
            assert!(run("abcdef..gh..i > @", "abcdefxɡhxi", "ixɡhxabcdef"));
            assert!(run("abcdefg..h..i > @", "abcdefɡxhxi", "ixhxabcdefɡ"));

            assert!(run("abc..de..fghi > @", "abcxdexfɡhi", "fɡhixdexabc"));
            assert!(run("abc..d..efghi > @", "abcxdxefɡhi", "efɡhixdxabc"));
            assert!(run("ab..cd..efghi > @", "abxcdxefɡhi", "efɡhixcdxab"));
            assert!(run("a..bcd..efghi > @", "axbcdxefɡhi", "efɡhixbcdxa"));
            assert!(run("a..bc..defghi > @", "axbcxdefɡhi", "defɡhixbcxa"));
            assert!(run("a..b..cdefghi > @", "axbxcdefɡhi", "cdefɡhixbxa"));
        }

        #[test]
        fn ord_abcde_fg() {
            assert!(run("abcde..fɡhi > @", "abcdexfɡhi", "fɡhixabcde"));
            assert!(run("abcde..fɡh > @", "abcdexfɡh", "fɡhxabcde"));
            assert!(run("abcde..fɡ > @", "abcdexfɡ", "fɡxabcde"));
            assert!(run("abcd..fɡ > @", "abcdxfɡ", "fɡxabcd"));
            assert!(run("abc..fɡ > @", "abcxfɡ", "fɡxabc"));
            assert!(run("abc..f > @", "abcxf", "fxabc"));
        }

        #[test]
        fn met_pfs() {
            let test_rule = "pfs > &";
            assert!(run(test_rule, "pfs", "sfp"));
            let test_rule = "{pf}s > &";
            assert!(run(test_rule, "pfs", "sfp"));
        }

        #[test]
        fn manual_met() {
            let test_rule = "[+rho]=1 V=2 > 2 1  / _s";
            let test_word = "ˈhros";
            assert!(run(test_rule, test_word, "ˈhors"));
        }

        #[test]
        fn long_dist_ipa() {
            let test_rule = "r..l > &";
            let test_word = "ˈpa.ra.bo.la";
            assert!(run(test_rule, test_word, "ˈpa.la.bo.ra"));
        }

        #[test]
        fn long_and_short_dist_ipa() {
            let test_rule = "r(..)l > &";

            let test_word = "ˈpar.la";
            assert!(run(test_rule, test_word, "ˈpal.ra"));

            let test_word = "ˈpa.ra.bo.la";
            assert!(run(test_rule, test_word, "ˈpa.la.bo.ra"));
        }

        #[test]
        fn segment_syll_bound() {
            let test_rule = "s$ > & / _V";
            let test_word = "es.a";
            assert!(run(test_rule, test_word, "e.sa"));

            let test_rule = "s:[+long]$ > & / _V";
            let test_word = "ess.a";
            assert!(run(test_rule, test_word, "e.sːa"));
        }

        #[test]
        fn syll_bound_segment() {
            let test_rule = "$s > & / V_{p,t,k}";
            let test_word = "e.spa.ɲa";
            assert!(run(test_rule, test_word, "es.pa.ɲa"));

            let test_rule = "$s:[+long] > & / V_{p,t,k}";
            let test_word = "e.sːpa.ɲa";
            assert!(run(test_rule, test_word, "esː.pa.ɲa"));

            let test_rule = "$e > &";
            let test_word = "es.pa.ɲa";
            assert!(run(test_rule, test_word, "e.s.pa.ɲa"));

            let test_rule = "e$ > &";
            let test_word = "es.pa.ɲe";
            assert!(run(test_rule, test_word, "es.pa.ɲ.e"));
        }
        
        #[test]
        fn what() {
            assert!(run("ə > * / [+cons, -syll]_[+son, +cont]V",      "sə.we.nəˈlo.o.i.sə", "s.we.nˈlo.o.i.sə"));
            assert!(run("[+cons, -syll, -nas]$ > & / _[+son, +cont]", "s.we.nˈlo.o.i.sə",   "swe.nˈlo.o.i.sə"));
            
            assert!(run("$N > & / V_$", "swe.nˈlo.o.i.sə", "swenˈlo.o.i.sə"));
        }

        #[test]
        fn syll_syll() {
            assert!(run("%% > &", "sa.ro.na", "ro.sa.na"));
        }

        #[test]
        fn ident() {
            let test_rule = "V > &";
            assert!(run(test_rule, "saus", "saus"));
        }
        #[test]
        fn simple_mixed() {
            let test_rule = "lVr > &";
            assert!(run(test_rule, "la.ri", "ra.li"));
            // V matches a vowel segment of arbitrary length (user must specify [-long] if only matching short segments)
            assert!(run(test_rule, "la:.ri", "raː.li"));
            // But does not match different consecutive vowels
            assert!(run(test_rule, "lau.ri", "lau.ri"));
        }

        #[test]
        fn long_segments() {
            let test_rule = "a:[Along] .. i:[Blong] > &";
            assert!(run(test_rule, "raː.li", "ri.laː"));
            assert!(run(test_rule, "ra.liː", "riː.la"));
            assert!(run(test_rule, "raː.liː", "riː.laː"));

            let test_rule = "a:[Alen] .. i:[Blen] > &";
            assert!(run(test_rule, "raː.li", "ri.laː"));
            assert!(run(test_rule, "ra.liː", "riː.la"));
            assert!(run(test_rule, "raː.liː", "riː.laː"));
        }

        #[test]
        fn cross_bound_segment() {
            assert!(run("## n > &", "ta nej.prun", "tan ej.prun"));
            assert!(run("n ## > &", "an ej.prun", "a nej.prun"));

            assert!(run("## n:[+long] > &", "a n:ej.prun", "anː ej.prun"));
            assert!(run("n:[+long] ## > &", "an: ej.prun", "a nːej.prun"));
        }

        #[test]
        fn cross_bound_syll() {
            assert!(run("## % > &", "a nej.prun", "a.nej prun"));
            assert!(run("% ## > &", "a.nej prun", "a nej.prun"));
            
            assert!(run("% ## % > &", "a.nej prun", "a.prun nej")); 
            assert!(run("% ## % > &", "sa.lo sa.lo", "sa.sa lo.lo")); 
        }

        #[test]
        fn segment_syllable() {
            assert!(run("s% > &", "tas.na", "ta.na.s"));
            assert!(run("s..% > &", "sa.na", "na.a.s"));

            assert!(run("s:[+long]% > &", "tasː.na", "ta.na.sː"));
            assert!(run("s:[+long]..% > &", "sːa.na", "na.a.sː"));

            assert!(run("%s > &", "ta.sa", "s.ta.a"));
            assert!(run("%..s > &", "ta.na.sa", "s.na.ta.a"));

            assert!(run("%s:[+long] > &", "ta.sːa", "sː.ta.a"));
            assert!(run("%..s:[+long] > &", "ta.na.sːa", "sː.na.ta.a"));
        }

        #[test]
        fn ellipsis_even_segment_segment() {
            assert!(run("b..c > &", "bac", "cab"));
            
            assert!(run("b..c..d..f > &",   "ba.ca.daf",   "fa.da.cab"));
            assert!(run("bk..c..d..fd > &", "bka.ca.dafd", "dfa.da.cakb"));
        }

        #[test]
        fn ellipsis_uneven_segment_segment() {
            assert!(run("br..c > &",       "brac",  "carb"));
            assert!(run("[+long]r..c > &", "b:rac", "carbː"));
            assert!(run("br..[+long] > &", "brac:", "cːarb"));

            assert!(run("b..rc > &",       "barc",  "crab"));
            assert!(run("[+long]..rc > &", "b:arc", "crabː"));
            assert!(run("b..r[+long] > &", "barc:", "cːrab"));
        }

        #[test]
        fn ellipsis_uneven_segment_syll() {
            assert!(run("br..% > &", "bra.ca",    "ca.a.r.b")); // Should really be ca.a.rb
            assert!(run("br..% ~ &", "bra.ca",    "ca.a.rb"));
            assert!(run("br..% > &", "bra.ca.ta", "ca.a.r.bta"));
            assert!(run("br..% ~ &", "bra.ca.ta", "ta.a.ca.rb"));
            assert!(run("br..% > @", "bra.ca",    "ca.a.b.r"));
            assert!(run("br..% > @", "bra.ca.ta", "ca.a.b.rta"));


            assert!(run("%..rb > &", "ac.arb",    "br.a.ac"));
            assert!(run("%..rb ~ &", "ac.arb",    "b.r.a.ac"));


            assert!(run("%..rc > &", "ba.tarc",    "cr.ta.ba"));
            assert!(run("%..rc > &", "mo.ba.tarc", "cr.ba.ta.mo"));
            assert!(run("%..rc > @", "ba.tarc",    "rc.ta.ba"));

            
            assert!(run("[+long]r..% > &", "bːra.ca",    "ca.a.r.bː"));
            assert!(run("[+long]r..% > &", "bːra.ca.ta", "ca.a.r.bːta"));

            assert!(run("%..r[+long] > &", "ac.arbː",    "bːr.a.ac"));

        }

        #[test]
        fn ellipsis_even_segment_syll_bound() {
            assert!(run("r..$ > &", "bra",    "ba.r"));
            assert!(run("r..$ > &", "bra.ca", "ba.rca"));

            assert!(run("r..$ > &", "so.ra.ca",  "so.a.rca"));
            assert!(run("r..$ > &", "so.bra.ca", "so.ba.rca"));
        }

        #[test]
        fn ellipsis_uneven_segment_syll_bound() {
            assert!(run("br..$ > &", "bra",       "a.rb"));
            assert!(run("br..$ > &", "bra.ca",    "a.rbca"));
            assert!(run("br..$ > &", "bra.ca.ta", "a.rbca.ta"));

            assert!(run("$..rb > &",      "arb",       "br.a"));
            assert!(run("$..rb > &",      "ac.arb",    "br.ac.a"));
            assert!(run("$..rb > & | #_", "ac.arb",    "acbr.a"));
            assert!(run("$..rb ~ &", "ac.arb",    "acbr.a"));
            assert!(run("$..rb ~ &", "at.ac.arb", "at.acbr.a"));
            assert!(run("$..rb > &", "at.ac.arb", "br.at.ac.a"));
            
            assert!(run("[+long]r..$ > &", "bːra",       "a.rbː"));
            assert!(run("[+long]r..$ > &", "bːra.ca",    "a.rbːca"));
            assert!(run("[+long]r..$ > &", "bːra.ca.ta", "a.rbːca.ta"));

            assert!(run("obr..$ > &", "o.bra", "a.rbo"));
            assert!(run("obr..$ > &", "obra",  "a.rbo"));

            assert!(run("obr..$ ~ &", "o.bra", "a.rbo"));
            assert!(run("obr..$ ~ &", "obra",  "a.rbo"));
        }

        #[test]
        fn ellipsis_even_segment_word_bound() {
            let test_rule = "b..## > &";
            assert!(run(test_rule, "bra", "bra"));
            assert!(run(test_rule, "bra ca", "ra bca"));
            assert!(run(test_rule, "bra ca.ta", "ra bca.ta"));
            assert!(run(test_rule, "bra ca ta", "ra bca ta"));
            
            let test_rule = "[+long]..## > &";
            assert!(run(test_rule, "bːra", "bːra"));
            assert!(run(test_rule, "bːra ca", "ra bːca"));
            assert!(run(test_rule, "bːra ca.ta", "ra bːca.ta"));
            assert!(run(test_rule, "bːra ca ta", "ra bːca ta"));


            assert!(run("r..## > &", "bra ca", "ba rca"));            
            assert!(run("##..r > &", "ac arb", "acr ab"));
        }

        #[test]
        fn ellipsis_uneven_segment_word_bound() {
            let test_rule = "br..## > &";
            assert!(run(test_rule, "bra ca",    "a rbca"));
            assert!(run(test_rule, "bra ca.ta", "a rbca.ta"));
            assert!(run(test_rule, "bra ca ta", "a rbca ta"));

            let test_rule = "##..rb > &";
            assert!(run(test_rule, "ac arb",    "acbr a"));
            assert!(run(test_rule, "at.ac arb", "at.acbr a"));
            assert!(run(test_rule, "at ac arb", "at acbr a"));

            let test_rule = "[+long]r..## > &";
            assert!(run(test_rule, "bːra ca", "a rbːca"));
            assert!(run(test_rule, "bːra ca.ta", "a rbːca.ta"));
            assert!(run(test_rule, "bːra ca ta", "a rbːca ta"));

            let test_rule = "##..r[+long] > &";
            assert!(run(test_rule, "ac arbː",    "acbːr a"));
            assert!(run(test_rule, "at.ac arbː", "at.acbːr a"));
            assert!(run(test_rule, "at ac arbː", "at acbːr a"));
        }

        #[test]
        fn ellipsis_even_syll_segment() {
            let test_rule = "r..a > &";
            assert!(run(test_rule, "so.ra.ca", "so.aː.cr"));
            let test_rule = "r(..)a > &";
            assert!(run(test_rule, "so.ra.ca", "so.ar.ca"));
            assert!(run(test_rule, "so.ran.ca", "so.arn.ca"));
        }

        #[test]
        fn ellipsis_uneven_syllable_segment() {
            assert!(run("%r..a > &",   "so.ra.ca",  "aː.cr.so"));
            assert!(run("%r(..)a > &", "so.ra.ca",  "ar.so.ca"));
            assert!(run("%r(..)a > &", "so.ran.ca", "ar.so.n.ca"));
            
            assert!(run("a..r% > &",   "ac.ar.os",  "os.rc.aː"));
            assert!(run("a(..)r% > &", "ac.ar.os",  "os.rc.aː"));
            assert!(run("a(..)r% > &", "ac.nar.os", "os.rc.naː"));

            assert!(run("%r..a:[+long] > &", "so.ra.ca:", "aːː.cr.so"));
            assert!(run("a:[+long]..r% > &", "a:c.ar.os", "os.rc.aːː"));
            
            let test_rule = "%r(..)a:[+long] > &";
            assert!(run(test_rule, "so.ra:.ca", "aːr.so.ca"));
            assert!(run(test_rule, "so.ra:n.ca", "aːr.so.n.ca"));
        }

        #[test]
        fn ellipsis_even_syllable_syllable() {
            let test_rule = "%..% > &";
            assert!(run(test_rule, "so.ra.ca", "ca.ra.so"));

            let test_rule = "r..% > &";
            assert!(run(test_rule, "so.ra.ca", "so.ca.a.r"));
        }

        #[test]
        fn ellipsis_uneven_syllable_syllable() {
            let test_rule = "%r..% > &";
            assert!(run(test_rule, "so.ra.ca", "ca.a.r.so"));
        }

        #[test]
        fn ellipsis_even_syllable_syll_bound() {
            let test_rule = setup_rule("<so>..$ > &");
            assert!(test_rule.apply_word(setup_word("so.ra.ca")).is_err());
        }

        #[test]
        fn ellipsis_uneven_syllable_syll_bound() {
            let test_rule = "%r..$ > &";
            assert!(run(test_rule, "so.ra.ca", "a.rso.ca"));
            assert!(run(test_rule, "so.rba.ca", "ba.rso.ca"));
            
            
            assert!(run(test_rule, "so.bra.ca", "so.bra.ca"));
        }

        #[test]
        fn ellipsis_even_syll_word_bound() {
            let test_rule = "<so>..## > &";
            assert!(run(test_rule, "so.ra.ca", "so.ra.ca"));
            assert!(run(test_rule, "so.ra.ca na", "ra.ca so.na"));
        }

        #[test]
        fn ellipsis_uneven_syll_word_bound() {
            let test_rule = "<so>r..## > &";
            assert!(run(test_rule, "so.ra.ca", "so.ra.ca"));
            assert!(run(test_rule, "so.ra.ca na", "a.ca rso.na"));
            let test_rule = "<so><ra>..## > &";
            assert!(run(test_rule, "so.ra.ca na", "ca ra.so.na"));
        }

        #[test]
        fn ellipsis_even_syll_bound_segment() {
            let test_rule = "$..c > &";
            assert!(run(test_rule, "bra.ca", "c.bra.a"));
            let test_rule = "$..c:[+long] > &";
            assert!(run(test_rule, "bra.cːa", "cː.bra.a"));
        }

        #[test]
        fn ellipsis_uneven_syll_bound_segment() {
            let test_rule = "$b..c > &";
            assert!(run(test_rule, "bra.ca", "cra.b.a"));
            assert!(run(test_rule, "so.bra.ca", "socra.b.a"));
            
            let test_rule = "c..b$ > &";
            assert!(run(test_rule, "ac.arb", "a.b.arc"));
            assert!(run(test_rule, "ac.arb.os", "a.b.arcos"));

            let test_rule = "$b..c:[+long] > &";
            assert!(run(test_rule, "bra.cːa", "cːra.b.a"));
            assert!(run(test_rule, "so.bra.cːa", "socːra.b.a"));

            let test_rule = "c:[+long]..b$ > &";
            assert!(run(test_rule, "acː.arb", "a.b.arcː"));
            assert!(run(test_rule, "acː.arb.os", "a.b.arcːos"));
        }

        #[test]
        fn ellipsis_uneven_syll_bound_syll() {
            let test_rule = "b..<ca> > &";
            assert!(run(test_rule, "bra.ca", "ca.ra.b"));
            assert!(run(test_rule, "so.bra.ca", "so.ca.ra.b"));
            
            let test_rule = "$b..<ca> > &";
            assert!(run(test_rule, "bra.ca", "ca.ra.b"));
            assert!(run(test_rule, "so.bra.ca", "so.ca.ra.b"));

            let test_rule = "<ac>..b$ > &";
            assert!(run(test_rule, "ac.arb", "b.ar.ac"));
            assert!(run(test_rule, "ac.arb.os", "b.ar.ac.os"));
            
            let test_rule = "o$b..<ca> > &";
            assert!(run(test_rule, "so.bra.ca", "s.ca.ra.b.o"));
            
            let test_rule = "<ac>..b$o > &";
            assert!(run(test_rule, "ac.arb.os", "ob.ar.ac.s")); // maybe should be o.b.ar.ac.s

            let test_rule = "ob..<ca> > &";
            assert!(run(test_rule, "so.bra.ca", "s.ca.ra.b.o"));

            let test_rule = "<ac>..bo > &";
            assert!(run(test_rule, "ac.arb.os", "ob.ar.ac.s")); // maybe should be o.b.ar.ac.s
        }

        #[test]
        fn ellipsis_uneven_word_bound_segment() {
            let test_rule = "##b..c > &";
            assert!(run(test_rule, "so bra.ca", "so.cra.b a"));
            assert!(run(test_rule, "so bra.can", "so.cra.b an"));
            assert!(run(test_rule, "so bra.cant", "so.cra.b ant"));
            assert!(run(test_rule, "so bra.cran", "so.cra.b ran"));
            assert!(run(test_rule, "so bra.scran", "so.cra.sb ran"));
            assert!(run(test_rule, "so bra.scran.tan", "so.cra.sb ran.tan"));

            assert!(run(test_rule, "so bac.ta", "so.cab ta"));
            assert!(run(test_rule, "so bac ta", "so.cab ta"));

            let test_rule = "o##b..c > &";
            assert!(run(test_rule, "so bac.ta", "s.cab o.ta"));

            let test_rule = "c..b##o ~ &";
            assert!(run(test_rule, "at.cab os", "at.o bac.s"));

            let test_rule = "c..b##o > &";
            assert!(run(test_rule, "at.cab os", "ato bac.s")); // should be at.o bac.s

        }

        #[test]
        fn ellipsis_uneven_word_bound_long_segment() {
            let test_rule = "##b..c:[+long] > &";
            assert!(run(test_rule, "so bra.cːa", "so.cːra.b a"));
            assert!(run(test_rule, "so bra.cːan", "so.cːra.b an"));
            assert!(run(test_rule, "so bra.cːant", "so.cːra.b ant"));
            assert!(run(test_rule, "so bra.cːran", "so.cːra.b ran"));
            assert!(run(test_rule, "so bra.scːran", "so.cːra.sb ran"));
            assert!(run(test_rule, "so bra.scːran.tan", "so.cːra.sb ran.tan"));

            assert!(run(test_rule, "so bacː.ta", "so.cːab ta"));
            assert!(run(test_rule, "so bacː ta", "so.cːab ta"));

            let test_rule = "o##b..c:[+long] > &";
            assert!(run(test_rule, "so bacː.ta", "s.cːab o.ta"));
        }

        #[test]
        fn ellipsis_uneven_word_bound_syllable() {
            let test_rule = "##b..<ca> > &";
            assert!(run(test_rule, "so bra.ca.ta", "so.ca.ra.b ta"));
            assert!(run(test_rule, "so bra.ca.tan", "so.ca.ra.b tan"));

            assert!(run(test_rule, "so bra.ca", "so.ca.ra.b"));
        }

        #[test]
        fn ellipsis_uneven_word_bound_syll_bound() {
            let test_rule = "##b..$ > &";
            assert!(run(test_rule, "so bra.ca.ta", "so.rab ca.ta"));
            assert!(run(test_rule, "so bra.ca.tan", "so.rab ca.tan"));

            assert!(run(test_rule, "so bra.ca", "so.rab ca"));
        }

        #[test]
        fn bacsa() {
            let test_rule = "c..b$ > &";
            assert!(run(test_rule, "cab.sa", "bacsa"));
        }

        #[test]
        fn kakohe() {
            assert!(run("k..h > &",      "ka.ko.he", "ha.ko.ke"));
            assert!(run("k..h > & | #_", "ka.ko.he", "ka.ho.ke"));
        }
    }



    #[test]
    fn test_del_simple_ipa() {
        let test_rule = "o > *";
        let test_word = "o.so.on.o";
        assert!(run(test_rule, test_word, "s.n"));
    }

    #[test]
    fn test_del_mult_ipa() {
        let test_rule = "ta > *";
        let test_word = "pa.ta.ka";
        assert!(run(test_rule, test_word, "pa.ka"));

        let test_rule = "ata > *";
        let test_word = "pa.ta.ka";
        assert!(run(test_rule, test_word, "p.ka"));

        let test_rule = "pata > *";
        let test_word = "pa.ta.ka";
        assert!(run(test_rule, test_word, "ka"));

        let test_rule = "patak > *";
        let test_word = "pa.ta.ka";
        assert!(run(test_rule, test_word, "a"));

        let test_rule = "pata..a > *";
        let test_word = "pa.ta.ka";
        assert!(run(test_rule, test_word, "k"));

        let test_rule = "p..a > *";
        let test_word = "pa.ta.ka";
        assert!(run(test_rule, test_word, "a.t.ka"));

        let test_rule = "p(..)a > *";
        let test_word = "pa.ta.ka";
        assert!(run(test_rule, test_word, "ta.ka"));
    }

    #[test]
    fn test_sub_skip() {
        assert!(run("p..t > x..x", "pa.ta.ka", "xa.xa.ka"));
        assert!(run("p..t > t..p", "pa.ta.ka", "ta.pa.ka"));
        assert!(run("p..t > x:[+long]..x", "pa.ta.ka", "xːa.xa.ka"));
        
        assert!(setup_rule("p..t > xx").apply_word(setup_word("pa.ta.ka")).is_err());
        assert!(setup_rule("p..t > x:[+long]x").apply_word(setup_word("pa.ta.ka")).is_err());

        assert!(run("a$ > e$", "a.pa.ta.ka.a.da", "e.pe.te.ke.e.de"));
        assert!(run("a$ > e$", "a.pa.ta.ki.a.da", "e.pe.te.ki.e.de"));
    }

    #[test]
    fn test_asdasd() {
        // r(..)l > l(..)r
        assert!(run("r(..)l > l(..)r", "ar.la", "al.ra"));
        assert!(run("r(..)l > l(..)r", "ˈpa.ra.bo.la", "ˈpa.la.bo.ra"));

        assert!(run("r(..)l > l(..)r", "ar.la.la", "al.ra.la"));

    }

    #[test]
    fn test_match_mult_ellises() {
        assert!(run("p..t..k > x..x..x", "pa.ta.ka", "xa.xa.xa"));
        assert!(run("p..t..k > x.. ..x", "pa.ta.ka", "xa.a.xa"));

        // Should be the same as p..k > x..x
        assert!(run("p.. ..k > x.. ..x", "pa.ta.ka", "xa.ta.xa"));
        // TODO: Should probably error
        assert!(run("p.. ..k > x..x..x", "pa.ta.ka", "xa.ta.xa"));
    }

    #[test]
    fn test_ellipses_prop() {
        assert!(run("p..kʷ > kʷ..kʷ", "pa.pa.kʷa", "kʷa.pa.kʷa"));
        assert!(run("p > kʷ / _..kʷ", "pa.pa.kʷa", "kʷa.kʷa.kʷa"));

        assert!(run("p..kʷ > kʷ..kʷ", "pa.pa.kʷa.kʷa", "kʷa.pa.kʷa.kʷa"));
        assert!(run("p..kʷ > p..p", "pa.pa.kʷa.kʷa", "pa.pa.pa.kʷa"));

    }

    #[test]
    fn test_sub_del_ellipsis() {
        assert!(run("pf..t > x..x", "pfa.ta.ka", "xa.xa.ka"));
        assert!(setup_rule("pf..t..k > x..x").apply_word(setup_word("pfa.ta.ka")).is_err());

        assert!(run("p..t > x..", "pa.ta.ka", "xa.a.ka"));
        
        assert!(run("p..$t > x..", "pa.ta.ka", "xaː.ka"));
    }

    #[test]
    fn test_all_ellipses() {
        assert!(run(".. > ..", "pa.ta.ka", "pa.ta.ka"));
        assert!(run("p > ..", "pa.ta.ka", "pa.ta.ka"));
        assert!(run(".. > p", "pa.ta.ka", "pa.ta.ka"));

    }

    #[test]
    fn test_del_ellipsis() {
        assert!(run("p..t > *", "pa.ta.ka", "a.a.ka"));
        assert!(run("p..$t > *", "pa.ta.ka", "aː.ka"));
    }

    #[test]
    fn test_del_syll_bound() {
        assert!(run("$ > *", "a.ske.sa.re", "askesare"));
        assert!(run("$ > * / _s", "a.ske.sa.re", "askesa.re"));
        assert!(run("$ > * | _s", "a.ske.sa.re", "a.ske.sare"));

        assert!(run("$a > *", "a.ske.sa.re", "ske.sa.re"));

        assert!(run("$a > *", "es.a", "es"));
        assert!(run("a$ > *", "es.a", "es"));
        assert!(run("$a > *", "as.a", "s"));
        assert!(run("a$ > *", "as.a", "as"));
    }

    #[test]
    fn test_del_syll() {
        let test_rule = "% > * | _s";
        let test_word = "a.ske.sa.re";
        assert!(run(test_rule, test_word, "a.ske"));
    }

    #[test]
    fn test_del_syll_ref() {
        let test_rule = "%=1 > * / 1_";
        let test_word = "ke.sa.sa";
        assert!(run(test_rule, test_word, "ke.sa"));
    }

    #[test]
    fn test_del_ipa_before_wbound() {
        let test_rule = "t > *  / _#";
        let test_word = "kat.kat";
        assert!(run(test_rule, test_word, "kat.ka"));

        let test_rule = "[] > *  / _[]#";
        let test_word = "kat.kat";
        assert!(run(test_rule, test_word, "kat.kt"));

        let test_rule = "[] > *  / []_[]#";
        let test_word = "kat.kat";
        assert!(run(test_rule, test_word, "kat.kt"));
    }

    // TODO: This doesn't work
    // #[test]
    // fn test_del_all() {
    //     assert!(run("[] > *", "kat.kat", ""))
    // }

    #[test]
    fn test_del_ipa_after_wbound() {
        let test_rule = "[] > *  / #_";
        let test_word = "kat.kat";
        assert!(run(test_rule, test_word, "at.kat"));

        let test_rule = "[] > *  / #C_";
        let test_word = "kat.kat";
        assert!(run(test_rule, test_word, "kt.kat"));

        let test_rule = "[] > *  / #[]_[]";
        let test_word = "kat.kat";
        assert!(run(test_rule, test_word, "kt.kat"));

        let test_rule = "[] > e  / #[]_[]";
        let test_word = "kat.kat";
        assert!(run(test_rule, test_word, "ket.kat"));
    }

    #[test]
    fn test_del_ipa_before_sbound() {
        let test_rule = "t > *  / _$ | _#";
        let test_word = "kat.kat";
        assert!(run(test_rule, test_word, "ka.kat"));
    }
    
    #[test]
    fn test_del_vowel_after_vowel() {
        let test_rule = "V > * / V_";
        let test_word = "kai.lua";
        assert!(run(test_rule, test_word, "ka.lu"));
    }

    #[test]
    fn test_del_matrix_after_matrix() {
        let test_rule = "[+syll, +high] > * / [+syll, -high]_";
        let test_word = "kai.lua";
        // from Assamese, "a high vowel gets deleted following a non-high vowel"
        assert!(run(test_rule, test_word, "ka.lua"));
    }

    #[test]
    fn test_del_set() {
        assert!(run("{i, u} > * ", "kau.lai", "ka.la"));
        assert!(run("{<lai>, u} > * ", "kau.lai", "ka"));
    }

    #[test]
    fn test_except_after_simple_ipa() {
        let test_rule = " i > e | c_";
        let test_word = "ki.ci";
        assert!(run(test_rule, test_word, "ke.ci"));
    }

    #[test]
    fn test_except_after_ipa() {
        let test_rule = " i > e | c:[+long] _";
        let test_word = "ki.cːi";
        assert!(run(test_rule, test_word, "ke.cːi"));
    }

    #[test]
    fn test_except_after_ipa_bound() {
        let test_rule = " i > e | cc_";
        let test_word = "kic.ci";
        assert!(run(test_rule, test_word, "kec.ci"));
    }

    #[test]
    fn test_except_before_simple_ipa() {
        let test_rule = " i > e | _c";
        let test_word = "ki.ci";
        assert!(run(test_rule, test_word, "ki.ce"));
    }

    #[test]
    fn test_except_before_ipa() {
        let test_rule = " i > e | _c:[+long]";
        let test_word = "ki.cːi";
        assert!(run(test_rule, test_word, "ki.cːe"));
    }

    #[test]
    fn test_except_before_ipa_bound() {
        let test_rule = " i > e | _cc";
        let test_word = "kic.ci";
        assert!(run(test_rule, test_word, "kic.ce"));
    }

    #[test]
    fn test_except_after_ipa_bound_false() {
        let test_rule = " i > e | cc_";
        let test_word = "ki.ci";
        assert!(run(test_rule, test_word, "ke.ce"));
    }

    #[test]
    fn test_except_before_ipa_bound_false() {
        let test_rule = " i > e | _cc";
        let test_word = "ki.ci";
        assert!(run(test_rule, test_word, "ke.ce"));
    }

    #[test]
    fn test_hang() {
        // TODO
        assert!(run("* > e / ()_", "san", "eseaen")); // Should be eseaene
        
        assert!(run("* > i / _n",     "pon", "poin"));
        assert!(run("* > i / ()_n",   "pon", "poin"));
        assert!(run("* > i / ([])_n", "pon", "poin"));

        assert!(run("* > i / []([])_n", "pon", "poin"));
        assert!(run("* > i / ([])[]_n", "pon", "poin"));

        assert!(run("* > i / _(n)",   "pon", "ipoin")); // Should be ipioini
        assert!(run("* > i / []_(n)", "pon", "pioin")); // Should be ipioini
        assert!(run("* > i / []_",    "pon", "pioin")); // Should be ipioini

        assert!(run("* > i / []([])_(n)",   "pon", "pioin")); // Should be ipioini
        assert!(run("* > i / ([], 1:)_(n)", "pon", "pioin")); // Should be ipioini

        assert!(run("* > i / #([])_(n)", "pon", "ipon"));
        assert!(run("* > i / #(p)_(n)",  "pon", "ipon"));
        assert!(run("* > i / #(p)_..n",  "pon", "ipon"));
    }

    // TODO
    mod hang {
        use super::run;

        #[test]
        fn opt0() {
            assert!(run("* > i / ()_", "pon", "ipioin")); // Should be ipioini
        }

        #[test]
        fn opt1() {
            assert!(run("* > i / ()_(n)", "pon", "ipioin")); // Should be ipioini
        }

        #[test]
        fn opt2() {
            assert!(run("* > i / ([])_(n)", "pon", "ipioin")); // Should be ipioini
        }

        #[test]
        fn opt3() {
            assert!(run("* > i / (p)_(n)", "pon", "ipioin")); // Should be ipioini
        }

        #[test]
        fn opt4() {
            assert!(run("* > i / []_", "pon", "pioin")); // Should be pioini
        }
    }

    #[test]
    fn test_insertion_between_syll_break_and_else() {
        let test_rule = " * > e / #_CC";
        let test_word = "ki.ci";
        assert!(run(test_rule, test_word, "ki.ci"));

        let test_rule = " * > e / #_C";
        let test_word = "ki.ci";
        assert!(run(test_rule, test_word, "eki.ci"));

        let test_rule = " * > e / $_CC";
        let test_word = "ki.ci";
        assert!(run(test_rule, test_word, "ki.ci"));

        let test_rule = " * > e / $_C";
        let test_word = "ki.ci";
        assert!(run(test_rule, test_word, "eki.eci"));
    }

    #[test]
    fn test_context_matrices() {
        let test_rule = "i > e / _(..)V:[-long]#";
        assert!(run(test_rule, "si.haː", "si.haː"));

        let test_rule = "i ~ e / _(..)V:[-long]#";
        assert!(run(test_rule, "si.haː", "si.haː"));

        let test_rule = "i > e / _(..)V:[+long]#";
        assert!(run(test_rule, "si.haː", "se.haː"));

        let test_rule = "i ~ e / _(..)V:[+long]#";
        assert!(run(test_rule, "si.haː", "se.haː"));

        let test_rule = "i > 1 / _(..)V:[+long]=1#";
        assert!(run(test_rule, "si.haː", "sa.haː"));

        let test_rule = "i ~ 1 / _(..)V:[+long]=1#";
        assert!(run(test_rule, "si.haː", "sa.haː"));
    }

    #[test]
    fn test_context_set() {
        let test_rule = "i > ɛ / _{r,h,ʍ}";
        let test_word = "si.sir";
        assert!(run(test_rule, test_word, "si.sɛr"));
        
        let test_word = "si.si.haz";
        assert!(run(test_rule, test_word, "si.sɛ.haz"));

        let test_word = "ri.hi.ʍaz";
        assert!(run(test_rule, test_word, "rɛ.hɛ.ʍaz"));
    }

    #[test]
    fn test_context_set_mult() {
        let test_rule = "i > ɛ / _{rV,hV}";
        assert!(run(test_rule, "si.si.haz", "si.sɛ.haz"));
        assert!(run(test_rule, "si.si.raz", "si.sɛ.raz"));
        assert!(run(test_rule, "si.sirs", "si.sirs"));

        let test_rule = "i > ɛ / _{rV,hV:[+high]}";
        assert!(run(test_rule, "si.si.haz", "si.si.haz"));
        assert!(run(test_rule, "si.si.huz", "si.sɛ.huz"));
    }

    #[test]
    fn test_insertion_segment_before_ipa() {
        let test_rule = "* > e / _s";
        let test_word = "ski";
        assert!(run(test_rule, test_word, "eski"));

        let test_rule = "* > e / _k";
        let test_word = "ski";
        assert!(run(test_rule, test_word, "seki"));
    }

    #[test]
    fn test_insertion_segment_after_ipa() {
        let test_rule = "* > e / s_";
        let test_word = "ski";
        println!("* > e / s_");
        assert!(run(test_rule, test_word, "seki"));

        let test_rule = "* > e / s_";
        let test_word = "kas";
        assert!(run(test_rule, test_word, "kase"));

        let test_rule = "* > e / k_";
        let test_word = "ski";
        assert!(run(test_rule, test_word, "skei"));


        assert!(run("* > e:[-long] / s_", "ski", "seki"));
        assert!(run("* > e:[+long] / s_", "ski", "seːki"));
        assert!(run("* > e:[+vlng] / s_", "ski", "seːːki"));
        assert!(run("* ~ e:[+long] / s:[+long]_", "sːki", "sːeːki"));
        assert!(run("* > e:[+long] / s:[+long]_", "sːki", "sːeːki"));
        assert!(run("* > e:[Alen] / s:[Alen]_", "sːki", "sːeːki"));
        assert!(run("* ~ e:[Alen] / s:[Alen]_", "sːki", "sːeːki"));
    }

    #[test]
    fn test_insertion_segment_between_ipa() {
        let test_rule = "* > e / s_k";
        assert!(run(test_rule, "kskis", "ksekis"));

        let test_rule = "* > j / t_e";
        assert!(run(test_rule, "steft.steft", "stjeft.stjeft"));

        let test_rule = "* > j / t_e";
        assert!(run(test_rule, "steft.teft", "stjeft.tjeft"));
    }

    #[test]
    fn test_insertion_segment_before_set() {
        let test_rule = "* > e / _{s,k}";
        let test_word = "ski";
        println!("* > e / _{{s,k}}");
        assert!(run(test_rule, test_word, "eseki"));

        let test_rule = "* > e / _k";
        let test_word = "ski";
        assert!(run(test_rule, test_word, "seki"));
    }

    #[test]
    fn test_insertion_segment_after_set() {
        let test_rule = "* > e / {s,k}_";
        let test_word = "ski";
        println!("* > e / {{s,k}}_");
        assert!(run(test_rule, test_word, "sekei"));
    }

    #[test]
    fn test_insertion_segment_before_matrix() {
        let test_rule = "* > e / _C";
        let test_word = "ski";
        println!("* > e / _C");
        assert!(run(test_rule, test_word, "eseki"));
    }

    #[test]
    fn test_insertion_segment_after_matrix() {
        let test_rule = "* > e / C_";
        let test_word = "ski";
        println!("* > e / C_");
        assert!(run(test_rule, test_word, "sekei"));
    }

    #[test]
    fn test_insertion_syllable_before_segment() {
        let test_rule = "* > % / _k";
        let test_word = "ski";
        println!("* > % / _k");
        assert!(run(test_rule, test_word, "s.ki"));
    }

    #[test]
    fn test_insertion_syllable_after_segment() {
        let test_rule = "* > % / s_";
        let test_word = "ski";
        println!("* > e / C_");
        assert!(run(test_rule, test_word, "s.ki"));
    }

    #[test]
    fn test_insertion_bound_before_segment() {
        assert!(run("* > $ / _k", "ski", "s.ki"));
        assert!(run("* > $ / _k", "skki", "s.k.ki"));
        assert!(run("* > $ / _k:[+long]", "skki", "s.kːi"));
    }

    #[test]
    fn test_insertion_bound_after_segment() {
        let test_rule = "* > $ / s_";
        let test_word = "ski";
        println!("* > $ / s_");
        assert!(run(test_rule, test_word, "s.ki"));
    }

    #[test]
    fn test_insertion_spanish() {
        // let test_rule = "* > b, d / m_r, n_r";
        let test_rule = "* > b:[Aplace] / [+nasal, Aplace]_r";
        let test_word = "om.re";
        assert!(run(test_rule, test_word, "om.bre"));
    }

    #[test]
    fn test_insertion_context_syll() {
        let test_rule = "* > e / _%";
        let test_word = "s.ki";
        println!("* > e / _%");
        assert!(run(test_rule, test_word, "se.ki"));
    }

    #[test]
    fn test_insertion_context_before_syll_bound() {
        let test_rule = "* > e / _$";
        let test_word = "s.ki";
        println!("* > e / _$");
        assert!(run(test_rule, test_word, "se.kie"));
        println!();

        let test_rule = "* > e / _$ | _#";
        let test_word = "s.ki";
        println!("* > e / _$ | _#");
        assert!(run(test_rule, test_word, "se.ki"));
    }

    #[test]
    fn test_insertion_context_after_syll_bound() {
        let test_rule = "* > e / $_";
        let test_word = "as.k";
        println!("* > e / $_");
        assert!(run(test_rule, test_word, "eas.ek"));
        println!();

        let test_rule = "* > e / $_ | #_";
        let test_word = "as.k";
        println!("* > e / $_ | #_");
        assert!(run(test_rule, test_word, "as.ek"));
        println!();
    }

    #[test]
    fn test_insertion_exception_before() {
        let test_rule = "* > e / _C | _C#";
        let test_word = "as.k";
        println!("* > e / _C | _C#");
        assert!(run(test_rule, test_word, "aes.k"));
        println!();

        let test_rule = "* > e / _C";
        let test_word = "as.k";
        println!("* > e / _C");
        assert!(run(test_rule, test_word, "aes.ek"));
        println!();
    }

    #[test]
    fn test_insertion_asdf() {
        let test_rule = "* > e / e_";
        let test_word = "sen";
        println!("* > e / e_");
        assert!(run(test_rule, test_word, "seːn"));
        println!("--------------------------");
        let test_rule = "* ~ e / (e)_";
        let test_word = "sen";
        println!("* ~ e / (e)_");
        assert!(run(test_rule, test_word, "seːne")); // TODO: eseːːne
        println!("--------------------------");
        let test_rule = "* > e / (e)_";
        let test_word = "sen";
        println!("* > e / (e)_");
        assert!(run(test_rule, test_word, "eseːːn")); // TODO: eseːːne
    }

    // #[test]
    // fn test_insertion_options() {
    //     let test_rule = "* > e / _([])";
    //     let test_word = "ask";
    //     assert!(run(test_rule, test_word, "eaeseke"));
    //     println!("------------------");
    //     let test_rule = "* ~ e / ([])_";
    //     let test_word = "ask";
    //     assert!(run(test_rule, test_word, "eaeseke"));

    //     // let test_rule = "* > e / ([])_";
    //     // let test_word = "ask";
    //     // assert!(run(test_rule, test_word).unwrap().render(&[]).0, "eaeseke"));
    // }

    #[test]
    fn test_insertion_word_boundary() {
        let test_rule = "* > $ka / _#";
        let test_word = "de.su";
        assert!(run(test_rule, test_word, "de.su.ka"));
    }


    #[test]
    fn test_match_alpha_feature() {
        let test_rule = "V > [αnasal] / _[αnasal]";

        assert!(run(test_rule, "an.ti", "ãn.ti"));
        assert!(run(test_rule, "a.na.ti", "ã.na.ti"));
        assert!(run(test_rule, "tan", "tãn"));
    }

    #[test]
    fn test_nasal_assim() {
        let test_rule = "[+nasal] > [αPLACE] / _C:[αPLACE] | _[-place]";
        assert!(run(test_rule, "ˈsɑm.dɑz", "ˈsɑn.dɑz"));
        assert!(run(test_rule, "ˈhʊng", "ˈhʊŋɡ"));
        assert!(run(test_rule, "ˈɪn.pʊt", "ˈɪm.pʊt"));

        assert!(run(test_rule, "samk", "saŋk"));
        assert!(run(test_rule, "sang", "saŋɡ"));
        assert!(run(test_rule, "sanp", "samp"));
        assert!(run(test_rule, "sanf", "saɱf"));
        assert!(run(test_rule, "sanq", "saɴq"));
        assert!(run(test_rule, "san?", "sanʔ"));
    }

    #[test]
    fn test_portuguese() {
        let test_rules = [
            setup_rule("[+rho] > [-cont] / C_, _$"),
            setup_rule("{s,m} > * / _#"),
            setup_rule("k > t^s / _[+front]"),
            setup_rule("i > j / _V"),
            setup_rule("V:[+long] > [-long]"),
            setup_rule("e > * / Vr_#"),
            setup_rule("$ > * / _r#"),
            setup_rule("$w > * / V_V"),
            setup_rule("u > o / _#"),
            setup_rule("ŋn > ɲ"),
            setup_rule("O:[-dr, -voice] > [+voice] / V_V"), // p,t,k > b,d,g / V_V
            setup_rule("k > i / i_t, e_t"), 
            setup_rule("k > u / u_t, o_t"), 
            setup_rule("O:[-cont] > 1 / V_O:[-cont]=1"),    // p > t / V_t
            setup_rule("i:[+long] > [-long]"),
            setup_rule("e > * / C_rV"),
            setup_rule("t^s > s"),
            setup_rule("lj > ʎ"),
            setup_rule("s > ʃ / i_"),
            setup_rule("j > ʒ"),
            setup_rule("a:[-str], e:[-str], o:[-str] > ɐ, ɨ, u | _CC"),
            setup_rule("C=1 > * / _1"),
            setup_rule("O:[+voice] > [+cont] | #_"),        // b, d, g > β, ð, ɣ | #_
            setup_rule("C$ > & / $_"),
            setup_rule("$C > & / _$"),
            setup_rule("V:[+str] > [αnasal] / _[αnasal]C"),
            setup_rule("N > * / V:[+nasal]_"),
        ];
        let test_words = [
            setup_word("'fo.kus"),
            setup_word("'jo.kus"),
            setup_word("dis'trik.tus"),
            setup_word("ki:.wi'ta:.tem"),
            setup_word("a.dop'ta.re"),
            setup_word("'o.pe.ra"),
            setup_word("se'kun.dus"),
            setup_word("'fi:.liam"),
            setup_word("'po:n.tem"),
        ];
        let output_matchs = [
            setup_word("ˈfo.ɣu"),
            setup_word("ˈʒo.ɣu"),
            setup_word("diʃˈtɾi.tu"),
            setup_word("siˈða.ðɨ"),
            setup_word("ɐ.ðoˈtar"),
            setup_word("ˈo.βrɐ"),   // ˈɔ.βɾɐ
            setup_word("sɨˈɣũ.ðu"),
            setup_word("ˈfi.ʎɐ"),
            setup_word("ˈpõ.tɨ"),
        ];

        let mut output_words: Vec<Word> = vec![];

        for word in &test_words {
            let mut w = word.clone();
            for (i, rule) in test_rules.iter().enumerate() {
                println!("--{}--", i+1);
                println!("{}", w.render());
                w = rule.apply_word(w).unwrap();
            }
            output_words.push(w)
        }

        for (w, m) in output_words.iter().zip(output_matchs) {
            assert_eq!(w.render(), m.render());
        }
    }

    #[test]
    fn test_proto_germanic_spirant_law() {
        // (dʰt, dt, tt >) ts(t) > ss
        // (dʰs, ds, ts >) ts > ss
        // (bʰs, bs, ps >) ps > ɸs
        // (ɡʰs, ɡs, ks >) ks > xs
        // (bʰt, bt, pt >) pt > ɸt
        // (ɡʰt, ɡt, kt >) kt > xt

        // dʰt, dt, tt > tst
        // tst > ts
        // dʰs, ds > ts
        // {p, k} > {ɸ, x} / _{t,s}

        let test_rule = "O:[-voi, -cor] > [+cont] / _{t,s}";
        
        assert!(run(test_rule, "ˈɑp.ter", "ˈɑɸ.ter"));
        assert!(run(test_rule, "ˈɑp.sɑn", "ˈɑɸ.sɑn"));

        assert!(run(test_rule, "ˈɑk.ter", "ˈɑx.ter"));
        assert!(run(test_rule, "ˈɑk.sɑn", "ˈɑx.sɑn"));

        let test_rule = "O:[+cor] O:[+cor] > [+cont, +strid] [+cont, +strid]";

        assert!(run(test_rule, "ˈɑt.ter", "ˈɑs.ser"));
        assert!(run(test_rule, "ˈɑt.sɑn", "ˈɑs.sɑn"));
    }

    #[test]
    fn test_match_ipa_alpha_feature() {
        let test_rule = "l:[Asyll] > r:[Asyll]";
        
        assert!(run(test_rule, "lak", "rak"));
        assert!(run(test_rule, "wl̩k", "wr̩k"));
    }

    #[test]
    fn test_grimms_law() {
        let test_rule = "[+cons, -son, -voice, -cont], [+cons, -son, +voice, -cont, -sg], [+cons, +voice, +sg] > [+cont], [-voice], [-sg]";
        assert!(run(test_rule, "kumˈtom", "xumˈθom"));
        assert!(run(test_rule, "kunˈtos", "xunˈθos"));
        assert!(run(test_rule, "ˈdant", "ˈtanθ"));
        assert!(run(test_rule, "ˈme.dʱu", "ˈme.du"));
        assert!(run(test_rule, "ˈkʷod", "ˈxʷot"));
        assert!(run(test_rule, "'ɡʱans", "ˈɡans"));
        assert!(run(test_rule, "'ɡʷʱels", "ˈɡʷels"));

        let test_rule = "p, t, k, kʷ, b, d, g, gʷ, bʱ, dʱ, gʱ, gʷʱ > ɸ, θ, x, xʷ, p, t, k, kʷ, b, d, g, gʷ";
        assert!(run(test_rule, "kunˈtos", "xunˈθos"));
        assert!(run(test_rule, "ˈdant", "ˈtanθ"));
        assert!(run(test_rule, "ˈme.dʱu", "ˈme.du"));
        assert!(run(test_rule, "ˈkʷod", "ˈxʷot"));
        assert!(run(test_rule, "'ɡʱans", "ˈɡans"));
        assert!(run(test_rule, "'ɡʷʱels", "ˈɡʷels"));
    }

    #[test]
    fn test_verners_law() {
        let test_rule = "[-voice, +cont] > [+voice] / V:[-stress]([+cons, +son])_";
        
        assert!(run(test_rule, "xumˈθom", "xumˈðom"));
        assert!(run(test_rule, "xunˈθos", "xunˈðos"));
        assert!(run(test_rule, "fɑˈθer", "fɑˈðer"));
        assert!(run(test_rule, "uˈɸer", "uˈβer"));
        assert!(run(test_rule, "ɑɸ", "ɑβ"));
        assert!(run(test_rule, "ˈme.du", "ˈme.du"));
    }

    #[test]
    fn test_pgmc_stress_shift() {
        let test_rule = "%:[+stress], % > [-stress], [+stress] / _, #_";
        assert!(run(test_rule, "xunˈðos", "ˈxun.ðos"));
        assert!(run(test_rule, "fɑˈðer", "ˈfɑ.ðer"));
        assert!(run(test_rule, "uˈβer", "ˈu.βer"));
    }

    #[test]
    fn test_japanese_devoicing() {
        let test_rule = "V > [-voice] / [-voi]_{[-voi], #}";
        assert!(run(test_rule, "de.sɯ", "de.sɯ̥"));
        
        let test_rule = "V > [-voice] / [-voi]_[-voi], [-voi]_#";
        assert!(run(test_rule, "de.sɯ", "de.sɯ̥"));
    }

    #[test]
    fn test_latin_stress() {
        let test_rule = "% => [+str] / #_#";
        assert!(run(test_rule, "sar", "ˈsar"));
        let test_rule = "V:[+lng] => [+str] / _%#";
        assert!(run(test_rule, "peː.diː.kaː.boː", "peː.diːˈkaː.boː"));
        // let test_rule = "V => [+str] / _C%#";
        let test_rule = "C => [+str] / _%#";
        assert!(run(test_rule, "kae̯.sar", "ˈkae̯.sar"));
        assert!(run(test_rule, "de.kem.ber", "deˈkem.ber"));
        let test_rule = "% => [+str] / _%:[-str]%#";
        assert!(run(test_rule, "juː.li.us", "ˈjuː.li.us"));
        assert!(run(test_rule, "a.ba.ki.noː", "aˈba.ki.noː")); 

        let test_rule = "%, V:[+lng], C, % => [+str] / #_#, _%#, _%#, _%:[-str]%#";
        assert!(run(test_rule, "sar", "ˈsar"));
        assert!(run(test_rule, "peː.diː.kaː.boː", "peː.diːˈkaː.boː"));
        assert!(run(test_rule, "kae̯.sar", "ˈkae̯.sar"));
        assert!(run(test_rule, "de.kem.ber", "deˈkem.ber"));
        assert!(run(test_rule, "juː.li.us", "ˈjuː.li.us"));
        assert!(run(test_rule, "a.ba.ki.noː", "aˈba.ki.noː")); 
        assert!(run(test_rule, "sep.ti.mus", "ˈsep.ti.mus")); 
        assert!(run(test_rule, "sep.tem.ber", "sepˈtem.ber"));


        let test_rule = "%, {⟨(..)V:[+long]⟩, ⟨(..)VC⟩}, % ~> [+str] / #_#, _%#, _%:[-str]%#";
        assert!(run(test_rule, "sar", "ˈsar"));
        assert!(run(test_rule, "peː.diː.kaː.boː", "peː.diːˈkaː.boː"));
        assert!(run(test_rule, "kae̯.sar", "ˈkae̯.sar"));
        assert!(run(test_rule, "de.kem.ber", "deˈkem.ber"));
        assert!(run(test_rule, "juː.li.us", "ˈjuː.li.us"));
        assert!(run(test_rule, "a.ba.ki.noː", "aˈba.ki.noː")); 
        assert!(run(test_rule, "sep.ti.mus", "ˈsep.ti.mus")); 
        assert!(run(test_rule, "sep.tem.ber", "sepˈtem.ber"));
        

        let test_rule = "%, ⟨(..){V:[+long], C}⟩, % ~> [+str] / #_#, _%#, _%:[-str]%#";
        assert!(run(test_rule, "sar", "ˈsar"));
        assert!(run(test_rule, "peː.diː.kaː.boː", "peː.diːˈkaː.boː"));
        assert!(run(test_rule, "kae̯.sar", "ˈkae̯.sar"));
        assert!(run(test_rule, "de.kem.ber", "deˈkem.ber"));
        assert!(run(test_rule, "juː.li.us", "ˈjuː.li.us"));
        assert!(run(test_rule, "a.ba.ki.noː", "aˈba.ki.noː")); 
        assert!(run(test_rule, "sep.ti.mus", "ˈsep.ti.mus")); 
        assert!(run(test_rule, "sep.tem.ber", "sepˈtem.ber"));
    }

    #[test]
    fn test_haplology() {
        let test_rule = "%=1 > * / 1_";
        assert!(run(test_rule, "hap.lo.lo.ɡi", "hap.lo.ɡi"));
        assert!(run(test_rule, "nu.tri.tri", "nu.tri"));
        assert!(run(test_rule, "tra.ɡi.co.co.mi.co", "tra.ɡi.co.mi.co"));
        assert!(run(test_rule, "nar.si.si.zm", "nar.si.zm"));
        assert!(run(test_rule, "mor.fo.fo.no.lo.ɡi", "mor.fo.no.lo.ɡi"));
    }

    #[test]
    fn test_reduplication() {
        let test_rule = "* > 1:[-stress] / <CV>:[+stress]=1 _";
        assert!(run(test_rule, "'nu.tri", "ˈnu.nu.tri"));
        assert!(run(test_rule, "'sa", "ˈsa.sa"));
        assert!(run(test_rule, "'to.lo.ma", "ˈto.to.lo.ma"));
        assert!(run(test_rule, "to'lo.ma", "toˈlo.lo.ma"));
        
        assert!(run(test_rule, "'as.tri", "ˈas.tri"));
        assert!(run(test_rule, "'nus.tri", "ˈnus.tri"));
    }

    #[test]
    fn test_gemination() {
        let test_rule = "* > s / V:[+str, -long] _ $";
        assert!(run(test_rule, "'nu.sa", "ˈnus.sa"));
        
        let test_rule = "* > 1 / V:[-long] _ $ C=1";
        assert!(run(test_rule, "'nu.sa", "ˈnus.sa"));
        
        let test_rule = "* > 1 / ⟨(..)V:[-long]⟩ _ <C=1..>";
        assert!(run(test_rule, "'nu.sa", "ˈnus.sa"));
        
        let test_rule = "* > 1 / ⟨(..)V:[-long]⟩:[+str] _ ⟨C=1..⟩";
        assert!(run(test_rule, "'lu.ka", "ˈluk.ka"));
        
        let test_rule = "* > 1 / V:[-long, +str] _ $ C=1";
        assert!(run(test_rule, "'lu.ka", "ˈluk.ka"));
        
        let test_rule = "V:[-long, +str] $ C=1 > [] 1 $ 1";
        assert!(run(test_rule, "'lu.ka", "ˈluk.ka"));

        let test_rule = "$ C=1 > 1 $ 1 / V:[-long, +str]_";
        assert!(run(test_rule, "'lu.ka", "ˈluk.ka"));
    }

    #[test]
    fn test_set_feat_neg_when_node_neg() {
        let test_rule = "P:[α DOR] > [α round]";
        assert!(run(test_rule, "dor.kap", "dor.kʷap"));
    }


    #[test]
    fn test_engala_thingy() {
        let test_rule = "O:[+nas, Aplace]=1,$N > n:[Aplace]1:[-nas], & / _ , V_C";
        assert!(run(test_rule, "a.ᵐbo", "am.bo"));
    }

    #[test]
    fn test_engala_new() {
        let test_rules = [
            // 1) Copy Vowel Insertion
            setup_rule("* > <1> / #_O:[+nas]V=1"),
            // 2) Nasal Assimilation
            setup_rule("[+nas] > [Aplace] / _C:[Aplace]"),
            // 3) Voicing Assim
            setup_rule("s > [+voi] / _{P:[+voi], F:[+voi]}"),
            // 4) Spirant Lenitin
            setup_rule("s > h / _C:[-voi]"),
            // 5) Obstruent Assim
            setup_rule("h > 1 / _C=1"),
            // 6) Pre-nasalised Cons Simp
            setup_rule("O:[+nas, Aplace]=1 > n:[Aplace]1:[-nas]"),
            setup_rule("$N > & / V_C"),
            // 7) Hiatus Res
            setup_rule("$ > * / V_V"),
            setup_rule("ai, au, əi, əu > aj, aw, e, o"),
            setup_rule("i, u > [-syl] / _V:[-hi]"),
            setup_rule("əa, aə > ə:[+long], a:[+long]"),
            // 8) Pre-nasal Raising
            // setup_rule("{ə, a, e, o} > {ɨ, ə, i, u} / _N{O,#}"),
            setup_rule("V:[-hi, -lo] > [+hi, +tense -red] / _N{O,#}"),
            setup_rule("V:[-hi, +lo] > [-lo, +red] / _N{O,#}"),
            // 9) Central Vowel Annihilation Part Un
            setup_rule("V:[-fr, -bk, +str] > [+fr, +tens, -red]"),
            // 10) Progressive Assimilation
            setup_rule("V:[-lo, -bk, -fr] > [Abk, Bfr, +tens] / V:[Abk, Bfr]=1.._"),
            // 11) Post-Nasal Lenition
            setup_rule("O:[-cont, +voi] > * / N_"),
            setup_rule("N$ > & / V_V"),
            setup_rule("O:[-cont, -voi] > [+voi] / N_"),
            setup_rule("* > t:[Avoi] / n_C:[+cont, Avoi, +cor]"), // n_{s,z,l}
            setup_rule("{f, x} > h / N_"),
            // 12) Weak Fricative Lenition
            setup_rule("{f, x} > [+voi] / V_V"),
            // 13) Voiceless Stop Lenition 
            setup_rule("C:[-cont] > [+voi] / [+syll]_[+syll]"),
            // 14) Velarisation
            setup_rule("C:[+cor] > [Aplace, -lab] / _w:[Aplace]"),
            // 11) Central Vowel Annihilation Part Deux
            setup_rule("V:[-fr, -bk, -lo] > [+fr, +tens, -red]"),
        ];

        let test_words = [
            setup_word("'ᵑɡa.la"),
            setup_word("ᵑɡa'la"),
            setup_word("'ᵐbo"),
            setup_word("'ᵐba"),
            setup_word("'ᵐbə"),
            setup_word("'mo"),
            setup_word("'ha.mi"),
            setup_word("at.wa"),
            ];
        let output_matchs = [
            setup_word("eˈŋæ.la"),
            setup_word("e.ŋaˈlæ"),
            setup_word("uˈmo"),
            setup_word("eˈmæ"),
            setup_word("iˈme"),
            setup_word("ˈmo"),
            setup_word("'hæ.mi"),
            setup_word("ak.wa"),
        ];

        let mut output_words: Vec<Word> = vec![];

        for word in &test_words {
            let mut w = word.clone();
            for (ri, rule) in test_rules.iter().enumerate() {
                println!("-- {} --", ri);
                println!("{}", w.render());
                w = match rule.apply_word(w) {
                    Ok(w) => w,
                    Err(_) => {
                        // println!("{}", e.format_error(&["* > t:[Avoi] / n_C:[+cont, Avoi, +cor]".to_string()]));
                        assert!(false);
                        unreachable!()
                    },
                }
            }
            output_words.push(w)
        }

        for (w, m) in output_words.iter().zip(output_matchs) {
            assert_eq!(w.render(), m.render());
        }
    }

    #[test]
    fn test_prop_ltr() {

        // V > [α front, β back] > V:[α front, β back]C_	
        // /sinotehu/ becomes /sinøtehy/, not /sinøtɤhy/

        let test_rule = "V > [α front, β back] / V:[α front, β back]C_";
        assert!(run(test_rule, "si.no.te.hu", "si.nø.te.hy"));

        // V > [α front, β back] / _CV:[α front, β back]
        // /sinotehu/ becomes /sɯnøtɤhu/, note no propagation

        let test_rule = "V > [α front, β back] / _CV:[α front, β back]";
        assert!(run(test_rule, "si.no.te.hu", "sɯ.nø.tɤ.hu"));

        // V > [α front, β back] / _..V:[α front, β back]#
        // /sinotehu/ becomes /sɯnotɤhu/, as expected
        
        let test_rule = "V > [α front, β back] / _ (..) V:[α front, β back]#";
        assert!(run(test_rule, "si.no.te.hu", "sɯ.no.tɤ.hu"));
        assert!(run(test_rule, "si.no.te.se.hu", "sɯ.no.tɤ.sɤ.hu"));

        // blocking
        let test_rule = "V > [α front, β back] / _ (..) V:[α front, β back]# | _ (..) P (..) V:[α front, β back]#";
        assert!(run(test_rule, "si.no.te.hu", "si.no.tɤ.hu"));
        assert!(run(test_rule, "si.no.te.se.hu", "si.no.tɤ.sɤ.hu"));

        let test_rule = "V > [α front, β back] / _ (..) V:[α front, β back]# | _ (..) h (..) V:[α front, β back]#";
        assert!(run(test_rule, "si.no.te.hu", "si.no.te.hu"));

        let test_rule = "V > [α front, β back] / _ (..) V:[α front, β back]# | _ (..) s (..) V:[α front, β back]#";
        assert!(run(test_rule, "si.no.te.se.hu", "si.no.te.sɤ.hu"));
    }

    #[test]
    fn test_norwegian_lengthening() {
        let test_rule = "V > [Along] / _C:[-Along]";
        let test_word = "san:";
        assert!(run(test_rule, test_word, "sanː"));
        let test_word = "san";
        assert!(run(test_rule, test_word, "saːn"));
    }


    #[test]
    fn test_proto_anaki() {
        let test_rules = [
            // 1) Low Vowel Reduction
            setup_rule("a > ɐ"),
            setup_rule("ɐ:[-str] > ə | ɐ{h, ʔ}_"),
            // 2) Glottal Deletion
            setup_rule("h, ʔ > *"),
            // 3) Clustering I
            setup_rule("ə$ > * / s_[+cons, -son, -cont, -voice]"),
            // 4) Sonorant Syllabication
            setup_rule("[+son, -syll]=1 > 1:[+syll] / Cə_əC , Cə_ə#, #ə_əC"),
            setup_rule("ə > * / _,[+cons, +syll]"),
            // 5) Clustering II
            setup_rule("ə > * / s_[+cons, +son]"),
            setup_rule("s$ > & / _[+cons, +son]"),
            // 6) Clustering IV
            setup_rule("ə > * / [+cons, -syll]_[+son, +cont]V"),
            setup_rule("[+cons, -syll, -nasal]$ > & / _[+son, +cont]"),
            setup_rule("ə > * / VC:[+son, +cont]_C"),
            setup_rule("$C:[+son, +cont] > & / _$"),
            // 7) Clustering VIII
            setup_rule("ə > * / VC_s"),
            setup_rule("$ > * / V_Cs"),
            // 8) Clustering III
            setup_rule("ə > * / VC:[+nas]_C:[-nas]"),
            setup_rule("$C:[+nas] > & / V_$C:[-nas]"),
            // Clustering V
            // String::from("ə$ > * / C:[-cont, -nas, αPLACE]_C:[+nasal, -αPLACE]"),
            setup_rule("ə$ > * / P:[-nas, αPLACE]_N:[-αPLACE]"),
            // Schwa Hiatus Lengthening
            setup_rule("V:[-str] > [+long] / _,ə"),
            setup_rule("ə > * / _,V:[-str]"),
            // String::from("ə > 1 / _,V:[-str]=1"),
            // String::from("V:[-str]=1ə > 1:[+long] / _"),
            // Vowel Hiatus Merger
            setup_rule("$ > * / V:[-long]=1_1"),
            // Schwa Fronting
            setup_rule("{ɐ, ə} > e / _,i"),
            // Height Assimilation I
            setup_rule("{ɐ, ə} > [-low, +tense, -red, αhigh, -βback, -γfront, -δround] / _,V:[-low, αhigh, βback, γfront, δround]"),
            // Height Assimilation II
            setup_rule("V:[-low, +front, Blong] > [αhigh, Blong] / _,V:[-low, +back, αhigh]"),
            // Catalan-ish Vowel Reduction
            setup_rule("V:[-lo, -str, -long, -red] > [+hi] | C:[-fr, +bk, -hi, -lo]_"),
            // 2nd Vowel Hiatus Merger
            setup_rule("$ > * / V:[-long]=1_1"),
            // Uvular Lowering
            setup_rule("{ɐ, ə}, i, u > ɑ, e, o / [+cons, -high, +back]_"),
            // Loss of Schwa
            setup_rule("ə > * / _#"),
            setup_rule("$C > & / _#"),
            // Dorsal Nasal Merger
            setup_rule("ɴ, ɴʷ > ŋ, ŋʷ / _"),
            // Intervocalic Gliding
            setup_rule("V:[+hi, +tens] > [-syll, -tens] / V_V"),
            setup_rule("$ > * / V$G_V"),
            // A-Lowering
            setup_rule("ɐ:[+stress, Along], ə > [-tens, Along], ɐ"),
            // Geminate Avoidance
            setup_rule("V=1 C:[+long]=2 > 1:[+long]2:[-long]"),
            // // OR
            // String::from("V:[-long] > [+long] / _C:[+long]"), 
            // String::from("C:[+long] > [-long]"), 
            // Cluster Simplification
            // String::from("V=1 C=2 > 1:[+long] / _s2"),
            setup_rule("V > [+long] / _C=1s1"),
            setup_rule("C=1 > * / _s1"),
            // Hap(lo)logy
            setup_rule("$ > * / C=1 V:[-str]=2 _ 1 2"), 
            setup_rule("C=1 > * / 1V:[-str]=2_2"),
            // Labialisation
            setup_rule("C:[+hi, +bk] > [+rnd] / _w"),
            setup_rule("w > * / C:[+hi, +bk, +rnd]_"),
            
        ];

        let test_words = [
            setup_word("'ra.ka.sa"),
            setup_word("'gʷe.la.sa"),
            setup_word("'su.ma.qo"),
            setup_word("sa'mu.ha.la"),
            setup_word("sa'mi.ha.la"),
            setup_word("sa'mo.ha.la"),
            setup_word("sa'me.ha.la"),
            setup_word("sa'ma.ha.la"),
            setup_word("'me.hu"),
            setup_word("ka're.hu"),
            setup_word("'re.ka.re.hu"),
            setup_word("'ku.ŋe"),
            setup_word("qo'?e.ta"),
            setup_word("pa'mo"),
            setup_word("pa'no"),
            setup_word("pa'ŋo"),
            setup_word("pa'ɴo"),
            setup_word("'da.ra.sa.ri"),
            setup_word("'dars.ri"),
            setup_word("'se.re.re"),
            setup_word("'ba.ka.wi"),
            setup_word("'se.ra"),
            setup_word("'se.se.ra"),
            setup_word("'se.ra.e.he.ma"),
            setup_word("'se.ra.ka.ra"),
            setup_word("'se.se.ra.ka.ra"),
            setup_word("ˈse.ra.ɢo.ta"),
            setup_word("se.ra'te.?e"),
            setup_word("sa'qa.la"),
            setup_word("sa.ma'pi"),
            setup_word("so'?a.ma"),
            setup_word("'?a.so.?a.ma"),
            setup_word("sa'we.na"),
            setup_word("sa.we.na'te.?e"),
            setup_word("sa.we.na'lo.?a"),
            setup_word("sa.we.na'lo.?o.sa"),
            setup_word("sa.we.na'lo.?o.na"),
            setup_word("sa.we.na'lo.?o.ta"),
            setup_word("sa.we.na'lo.?a.hi.sa"),
            setup_word("sa.we.na'lo.?e.hi.sa"),
            setup_word("sa.we.na'lo.?u.hi.sa"),
            setup_word("sa.we.na'lo.?o.hi.sa"),
            setup_word("'la.hi.sa"),
            setup_word("'la.hi.hu"),
            setup_word("'ra.ke.sa.sa"),
            setup_word("'ra.ka.sa.sa"),
        ];
        let output_matchs = [
            setup_word("ˈraks"),
            setup_word("ˈɡʷels"),
            setup_word("ˈsum.qo"),
            setup_word("ˈsmu.il"),
            setup_word("ˈsmiːl"),
            setup_word("ˈsmo.il"),
            setup_word("ˈsme.ul"),
            setup_word("ˈsmaːl"),
            setup_word("ˈmi.u"),
            setup_word("ˈkri.u"),
            setup_word("ˈre.kri.u"),
            setup_word("ˈku.ŋi"),
            setup_word("qoˈet"),
            setup_word("pɐˈmo"),
            setup_word("ˈpno"),
            setup_word("ˈpŋo"),
            setup_word("ˈpŋo"),
            setup_word("ˈdaː.sri"),
            setup_word("ˈdaː.sri"),
            setup_word("ˈse.riː"),
            setup_word("ˈba.kʷi"),
            setup_word("ˈser"),
            setup_word("ˈse.sir"),
            setup_word("ˈse.reːm"),
            setup_word("ˈser.kr̩"),
            setup_word("ˈse.sir.kr̩"),
            setup_word("ˈser.ɢot"),
            setup_word("sirˈteː"),
            setup_word("ˈsqɑl"),
            setup_word("sm̩ˈpi"),
            setup_word("suˈem"),
            setup_word("ˈa.soːm"),
            setup_word("ˈswen"),
            setup_word("swinˈteː"),
            setup_word("swinˈlo.i"),
            setup_word("swinˈloːs"),
            setup_word("swinˈloːn"),
            setup_word("swinˈloːt"),
            setup_word("swinˈlo.eːs"),
            setup_word("swinˈlo.iːs"),
            setup_word("swinˈlo.wis"),
            setup_word("swinˈloː.is"),
            setup_word("ˈle.is"),
            setup_word("ˈle.ju"),
            setup_word("ˈra.kiːs"),
            setup_word("ˈrak.sɐs"),
        ];

        let mut output_words: Vec<Word> = vec![];

        for word in &test_words {
            let mut w = word.clone();
            for (_ri, rule) in test_rules.iter().enumerate() {
                // println!("-- {} --", _ri+1);
                // println!("{}", w.render(&[]));
                w = match rule.apply_word(w) {
                    Ok(w) => w,
                    Err(_) => {
                        // println!("{}", e.format_error(&["      ".to_string()]));
                        assert!(false);
                        unreachable!()
                    },
                }
            }
            output_words.push(w)
        }

        for (w, m) in output_words.iter().zip(output_matchs) {
            assert_eq!(w.render(), m.render());
        }   
    }

    #[test]
    fn test_clicks() {
        let test_rule = "[+clk] > [+dr]";
        
        assert!(run(test_rule, "ɴǃa", "ǃɴa"));
    }

    #[test]
    fn test_structure_substitution() {
        let test_rule = "% > <han>:[tone:51]";
        assert!(run(test_rule, "sleft", "han51"));

        let test_rule = "% > <han>:[+str] / _#";
        assert!(run(test_rule, "sleft", "ˈhan"));
        assert!(run(test_rule, "sleft.te", "sleftˈhan"));

        let test_rule = "% > <ha:[+long]n> / #_";
        assert!(run(test_rule, "sleft", "haːn"));
        assert!(run(test_rule, "sleft.te", "haːn.te"));
    }

    #[test]
    fn test_structure_ref() {
        let test_rule = "% > <ha1>:[tone:51] / _C=1";
        assert!(run(test_rule, "sleft.sa", "has51.sa"));

        let test_rule = "C > 1 / _<C=1as>";
        assert!(run(test_rule, "sleft.has", "slefh.has"));
    }

    #[test]
    fn test_structure_substitution_insert() {
        let test_rule = "te > ten<han>:[tone:51] / _#";
        assert!(run(test_rule, "sleft.te", "sleft.ten.han51"));
        let test_rule = "ef > eft<han>:[tone:51]";
        assert!(run(test_rule, "slef.te", "sleft.han51.te"));
        let test_rule = "a > a<han>:[tone:51]";
        assert!(run(test_rule, "slaft.te", "sla.han51.ft.te"));
        let test_rule = "e > e<han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "sle.han51.ft.te.han51"));
        let test_rule = "<>:[+stress]=1 > 1 <han>:[tone:51]";
        assert!(run(test_rule, "'sleft.te", "ˈsleft.han51.te"));
        let test_rule = "f > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "sle.han51.t.te"));
    }

    #[test]
    fn test_structure_substitution_segment() {
        // Segment Replacements
        let test_rule = "a > <hen>:[tone:51]";
        assert!(run(test_rule, "sleft.a.te", "sleft.hen51.te"));
        let test_rule = "a > <hen>:[tone:51]";
        assert!(run(test_rule, "sleft.a.a", "sleft.hen51.hen51"));
        let test_rule = "a > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.a.te", "sleft.han51.te"));
        let test_rule = "a > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.a.a", "sleft.han51.han51"));
        let test_rule = "a > <han>:[tone:51]";
        assert!(run(test_rule, "a.a.a", "han51.han51.han51"));
        let test_rule = "a > <han>:[tone:51]";
        assert!(run(test_rule, "a.e.a", "han51.e.han51"));
        let test_rule = "a > <han>:[tone:51]t";
        assert!(run(test_rule, "a.e.a", "han51.t.e.han51.t"));
        let test_rule = "a > <han>:[tone:51]<te>";
        assert!(run(test_rule, "a.e.a", "han51.te.e.han51.te"));
        let test_rule = "a > <han>:[tone:51]$t";
        assert!(run(test_rule, "a.e.a", "han51.t.e.han51.t"));
        let test_rule = "a$ > <han>:[tone:51]$t";
        assert!(run(test_rule, "a.e.a", "han51.te.han51.t"));

        let test_rule = "a > <wed>";
        assert!(run(test_rule, "asd.has", "wed.sd.h.wed.s"));
        let test_rule = "a > <wad>";
        assert!(run(test_rule, "asd.has", "wad.sd.h.wad.s"));
        let test_rule = "a > <wad>";
        assert!(run(test_rule, "asad.has", "wad.s.wad.d.h.wad.s"));
        let test_rule = "a > <wad>";
        assert!(run(test_rule, "asd", "wad.sd"));
        let test_rule = "a > <wad>";
        assert!(run(test_rule, "as", "wad.s"));
        let test_rule = "a > <wad>";
        assert!(run(test_rule, "as.as", "wad.s.wad.s"));
        let test_rule = "a > <wad>";
        assert!(run(test_rule, "a.a", "wad.wad"));
        let test_rule = "a > <a>";
        assert!(run(test_rule, "a.a", "a.a"));
        let test_rule = "a > <wad> | <wad>_";
        assert!(run(test_rule, "a.a", "wad.a"));
        let test_rule = "[] > <wad> | <wad>_";
        assert!(run(test_rule, "a.a", "wad.a"));
    }
    

    #[test]
    fn test_structure_context_match() {
        let test_rule = "% > <han>:[tone:51] / <sleft> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
        let test_rule = "% > <han>:[tone:51] / <(..)sleft> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
        let test_rule = "% > <han>:[tone:51] / <..left> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
        let test_rule = "% > <han>:[tone:51] / <..eft> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
        let test_rule = "% > <han>:[tone:51] / <s..eft> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
        let test_rule = "% > <han>:[tone:51] / <s..t> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
        let test_rule = "% > <han>:[tone:51] / <sl..> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
        let test_rule = "% > <han>:[tone:51] / <sle..> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
        let test_rule = "% > <han>:[tone:51] / <slef..> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
        let test_rule = "% > <han>:[tone:51] / <sleft(..)> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
        let test_rule = "% > <han>:[tone:51] / <..CC> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
        let test_rule = "% > <han>:[tone:51] / <..VCC> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
        let test_rule = "% > <han>:[tone:51] / <..CC> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
        let test_rule = "% > <han>:[tone:51] / <CCV..> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
        let test_rule = "% > <han>:[tone:51] / <CC..> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
        let test_rule = "% > <han>:[tone:51] / <CC..CC> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
        let test_rule = "% > <han>:[tone:51] / <CCVCC> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
        let test_rule = "% > <han>:[tone:51] / <CCV(..)CC> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
    }

    #[test]
    fn test_structure_context_match_multiple_ellipsis() {
        let test_rule = "% > <han>:[tone:51] / <s..e..t> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
        let test_rule = "% > <han>:[tone:51] / <..e..t> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
        let test_rule = "% > <han>:[tone:51] / <..e..> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
        let test_rule = "% > <han>:[tone:51] / <..l..> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
        let test_rule = "% > <han>:[tone:51] / <..f..> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
        let test_rule = "% > <han>:[tone:51] / <(..)t(..)> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
        let test_rule = "% > <han>:[tone:51] / <..t(..)> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
        let test_rule = "% > <han>:[tone:51] / <(..)s(..)> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
        let test_rule = "% > <han>:[tone:51] / <(..)s..> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
        // True Negatives
        let test_rule = "% > <han>:[tone:51] / <..t..> _";
        assert!(run(test_rule, "sleft.te", "sleft.te"));
        let test_rule = "% > <han>:[tone:51] / <..s..> _";
        assert!(run(test_rule, "sleft.te", "sleft.te"));
        let test_rule = "% > <han>:[tone:51] / <xs..e..t> _";
        assert!(run(test_rule, "sleft.te", "sleft.te"));
    }

    #[test]
    fn test_structure_input_match() {
        let test_rule = "<sleft> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));
        let test_rule = "<..eft> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));
        let test_rule = "<s..eft> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));
        let test_rule = "<s(..)eft> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));
        let test_rule = "<s(..)left> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));
        let test_rule = "<(..)sleft> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));
        let test_rule = "<s..t> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));
        let test_rule = "<sl..> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));
        let test_rule = "<(..)s(..)> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));
        let test_rule = "<..CC> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));
        let test_rule = "<..VCC> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));
        let test_rule = "<CC..CC> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));
        let test_rule = "<CCVCC> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));
    }

    #[test]
    fn test_structure_input_match_multiple_ellipsis() {
        let test_rule = "<s..e..t> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));
        let test_rule = "<..e..t> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));
        let test_rule = "<..e..> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));
        let test_rule = "<..l..> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));
        let test_rule = "<..f..> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));
        let test_rule = "<..V..> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));
        let test_rule = "<..CC> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));
        let test_rule = "<..VCC> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));
        let test_rule = "<CC..CC> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));
        let test_rule = "<CCVCC> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));
        let test_rule = "<(..)t(..)> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.han51"));
        let test_rule = "<(..)s(..)> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));
        // True Negatives
        let test_rule = "<..t..> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "sleft.te"));
        let test_rule = "<..s..> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "sleft.te"));
        let test_rule = "<xs..e..t> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "sleft.te"));
        let test_rule = "<s..Vt> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "sleft.te"));
    }

    #[test]
    fn test_structure_insert() {
        let test_rule = "* > <han>:[tone:51] / _#";
        assert!(run(test_rule, "sleft.te", "sleft.te.han51"));
        let test_rule = "* > <han>:[tone:51] / #_";
        assert!(run(test_rule, "sleft.te", "han51.sleft.te"));
        let test_rule = "* > <han>:[tone:51] / e_f";
        assert!(run(test_rule, "sleft.te", "sle.han51.ft.te"));
        let test_rule = "* > <han>:[tone:51] / t_e";
        assert!(run(test_rule, "sleft.te", "sleft.t.han51.e"));
        let test_rule = "* > <han>:[tone:51] / t_";
        assert!(run(test_rule, "sleft.te", "sleft.han51.t.han51.e"));
        let test_rule = "* > <han>:[tone:51] / e_";
        assert!(run(test_rule, "sleft.te", "sle.han51.ft.te.han51"));
    }

    #[test]
    fn test_structure_input_sets() {
        let test_rule = "<sl{e,o}ft> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));

        let test_rule = "<{t,s}l{e,o}f{t,s}> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));

        let test_rule = "<..{t, s}> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));
    }

    #[test]
    fn test_structure_context_sets() {
        let test_rule = "% > <han>:[tone:51] / <..{t, s}> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));

        let test_rule = "% > <han>:[tone:51] / <sl{e,o}ft> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
    }

    #[test]
    fn test_structure_references() {
        let test_rule = "% > <h1n>:[tone:51] / <slV=1ft> _";
        assert!(run(test_rule, "sleft.te", "sleft.hen51"));

        let test_rule = "<..V=1..{t, s}> > <h1n>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "hen51.te"));
    }

    #[test]
    fn test_structure_input_options() {
        let test_rule = "<sle(f)t> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));

        let test_rule = "<sle(f)..> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));

        let test_rule = "<(s,1:1)..> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));

        let test_rule = "<slef(t)> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));

        let test_rule = "<slef(tk)> > <han>:[tone:51]";
        assert!(run(test_rule, "sleftk.te", "han51.te"));

        let test_rule = "<slef(tk,1:2)> > <han>:[tone:51]";
        assert!(run(test_rule, "sleftk.te", "han51.te"));

        let test_rule = "<slef(tk,1:2)> > <han>:[tone:51]";
        assert!(run(test_rule, "sleftktk.te", "han51.te"));
        
        let test_rule = "<sl(Vf)t> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "han51.te"));

        // Negatives

        let test_rule = "<slef(tk)> > <han>:[tone:51]";
        assert!(run(test_rule, "sleftktktk.te", "sleftktktk.te"));

        let test_rule = "<sl(Vf)ft> > <han>:[tone:51]";
        assert!(run(test_rule, "sleft.te", "sleft.te"));
    }

    #[test]
    fn test_structure_context_options() {
        let test_rule = "% > <han>:[tone:51] / <sle(f)t> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));

        let test_rule = "% > <han>:[tone:51] / <sle(f)..> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));

        let test_rule = "% > <han>:[tone:51] / <(s,1:1)..> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));

        let test_rule = "% > <han>:[tone:51] / <slef(t)> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));

        let test_rule = "% > <han>:[tone:51] / <slef(tk)> _";
        assert!(run(test_rule, "sleftk.te", "sleftk.han51"));

        let test_rule = "% > <han>:[tone:51] / <slef(tk,1:2)> _";
        assert!(run(test_rule, "sleftk.te", "sleftk.han51"));

        let test_rule = "% > <han>:[tone:51] / <slef(tk,1:2)> _";
        assert!(run(test_rule, "sleftktk.te", "sleftktk.han51"));

        let test_rule = "% > <han>:[tone:51] / <sl(Vf)t> _";
        assert!(run(test_rule, "sleft.te", "sleft.han51"));
    }
 
    #[test]
    fn test_structure_underline_sub_matrix() {
        let test_rule = "a > e / <C_C>";
        assert!(run(test_rule, "san.ta" , "sen.ta"));
        assert!(run(test_rule, "sa.tan" , "sa.ten"));
        assert!(run(test_rule, "san.tan", "sen.ten"));
        assert!(run(test_rule, "an.tan" , "an.ten"));
        assert!(run(test_rule, "san.an" , "sen.an"));
        assert!(run(test_rule, "an.an"  , "an.an"));
        assert!(run(test_rule, "a.a"    , "a.a"));
        
        let test_rule = "a > e / <C_(C)>";
        assert!(run(test_rule, "san.ta" , "sen.te"));
        assert!(run(test_rule, "san.ta" , "sen.te"));
        assert!(run(test_rule, "sa.tan" , "se.ten"));
        assert!(run(test_rule, "san.tan", "sen.ten"));
        assert!(run(test_rule, "an.tan" , "an.ten"));
        assert!(run(test_rule, "san.an" , "sen.an"));
        assert!(run(test_rule, "an.an"  , "an.an"));
        assert!(run(test_rule, "a.a"    , "a.a"));

        let test_rule = "a > e / <(C)_C>";
        assert!(run(test_rule, "san.ta" , "sen.ta"));
        assert!(run(test_rule, "sa.tan" , "sa.ten"));
        assert!(run(test_rule, "san.tan", "sen.ten"));
        assert!(run(test_rule, "an.tan" , "en.ten"));
        assert!(run(test_rule, "san.an" , "sen.en"));
        assert!(run(test_rule, "an.an"  , "en.en"));
        assert!(run(test_rule, "a.a"    , "a.a"));

        let test_rule = "a > e / <(C)_(C)>";
        assert!(run(test_rule, "san.ta" , "sen.te"));
        assert!(run(test_rule, "sa.tan" , "se.ten"));
        assert!(run(test_rule, "san.tan", "sen.ten"));
        assert!(run(test_rule, "an.tan" , "en.ten"));
        assert!(run(test_rule, "san.an" , "sen.en"));
        assert!(run(test_rule, "an.an"  , "en.en"));
        assert!(run(test_rule, "a.a"    , "e.e"));

        let test_rule = "a > e / <C_>";
        assert!(run(test_rule, "san.ta" , "san.te"));


    }

    #[test]
    fn test_structure_underline_insertion_matrix_flanked() {
        let test_rule = "* > e / <C_C>";
        assert!(run(test_rule, "sn.ta", "sen.ta"));
        assert!(run(test_rule, "sa.tn", "sa.ten"));
        assert!(run(test_rule, "sn.tn", "sen.ten"));
        assert!(run(test_rule, "sa.ta", "sa.ta"));

        let test_rule = "* > e / <C_C>t";
        assert!(run(test_rule, "sn.ta", "sen.ta"));
        assert!(run(test_rule, "sn.tn", "sen.tn"));

        let test_rule = "* > e / <C_C>tn";
        assert!(run(test_rule, "sn.ta", "sn.ta"));
        assert!(run(test_rule, "sn.tn", "sen.tn"));


        let test_rule = "* > e / <C_C> | _t";
        assert!(run(test_rule, "sn.ta", "sn.ta"));
        assert!(run(test_rule, "sn.tn", "sn.ten"));


        let test_rule = "* > e / <C_C> | _";
        assert!(run(test_rule, "sn.ta", "sn.ta"));
        assert!(run(test_rule, "sn.tn", "sn.tn"));

        let test_rule = "* > e / <C_C> | <s_..>";
        assert!(run(test_rule, "sn.tn", "sn.ten"));
        let test_rule = "* > e / <C_C> | <s_..>..";
        assert!(run(test_rule, "sn.tn.sn", "sn.ten.sen"));
        let test_rule = "* > e / <C_C> | (..)<s_..>..";
        assert!(run(test_rule, "sn.tn.sn", "sn.ten.sen"));

        let test_rule = "* > e / <C_C> | <..><s_..>..";
        assert!(run(test_rule, "sn.tn", "sen.ten"));
        let test_rule = "* > e / <C_C> | ..<s_..>..";
        assert!(run(test_rule, "sn.tn", "sen.ten"));
    }

    #[test]
    fn test_structure_underline_insertion_matrix_ellipsis() {
        let test_rule = "* > e / <C_..>";
        assert!(run(test_rule, "sn.ta", "sen.tea"));
        let test_rule = "* > e / <.._C>";
        assert!(run(test_rule, "sn.ta", "sen.ta"));
        let test_rule = "* > e / <.._CC>";
        assert!(run(test_rule, "snt.ta", "sent.ta"));
        let test_rule = "* > e / <.._C>";
        assert!(run(test_rule, "stn.ta", "sten.ta"));
        let test_rule = "* > e / <.._CC>";
        assert!(run(test_rule, "stn.ta", "setn.ta"));

        let test_rule = "* > e / <.._C>";
        assert!(run(test_rule, "strn.ta", "stren.ta"));

        let test_rule = "* > e / <.._CC>";
        assert!(run(test_rule, "strn.ta", "stern.ta"));

        let test_rule = "* > e / <.._C:[+long]>";
        assert!(run(test_rule, "strn:.ta", "strenː.ta"));
        assert!(run(test_rule, "strn:.tnː", "strenː.tenː"));
        assert!(run(test_rule, "stn:.ta", "stenː.ta"));
        assert!(run(test_rule, "stn:t.ta", "stnːt.ta"));
        
        let test_rule = "* > e / <.._C:[+long]C>";
        assert!(run(test_rule, "stn:t.ta", "stenːt.ta"));

        assert!(run("* > e / <.._..>", "san", "sean"));
        assert!(run("* > e / <(..)_(..)>", "san", "esan"));
    }

    #[test]
    fn test_structure_underline_insertion_matrix_opt_ellipsis() {
        let test_rule = "* > e / <C_(..)>";
        assert!(run(test_rule, "sn.ta", "sen.tea"));
        let test_rule = "* > e / <(..)_C>";
        assert!(run(test_rule, "sn.ta", "sen.ta"));
        let test_rule = "* > e / <(..)_CC>";
        assert!(run(test_rule, "snt.ta", "sent.ta"));
        assert!(run(test_rule, "snt.tn", "sent.etn"));
        let test_rule = "* > e / <(..)_C>";
        assert!(run(test_rule, "stn.ta", "sten.ta"));
        assert!(run(test_rule, "stn.tn", "sten.ten"));
        let test_rule = "* > e / <(..)_CC>";
        assert!(run(test_rule, "stn.ta", "setn.ta"));

        let test_rule = "* > e / <(..)_C>";
        assert!(run(test_rule, "strn.ta", "stren.ta"));

        let test_rule = "* > e / <C(..)_C>";
        assert!(run(test_rule, "strn.ta", "stren.ta"));

        let test_rule = "* > e / <C_(..)C>";
        assert!(run(test_rule, "strn.ta", "setrn.ta"));

        let test_rule = "* > e / <(..)_C:[+long]>";
        assert!(run(test_rule, "strn:.ta", "strenː.ta"));
        assert!(run(test_rule, "strn:.tnː", "strenː.tenː"));
        assert!(run(test_rule, "stn:.ta", "stenː.ta"));
        assert!(run(test_rule, "stn:t.ta", "stnːt.ta"));

        let test_rule = "* > e / <(..)_C:[+long]C>";
        assert!(run(test_rule, "stn:t.ta", "stenːt.ta"));

        let test_rule = "* > e / <C(..)_C:[+long]C>";
        assert!(run(test_rule, "stn:t.ta", "stenːt.ta"));
    }

    #[test]
    fn saina() {
        assert!(run("a > e / <(..)_(..)N> ", "san",  "sen"));
        assert!(run("a > e / <(..)_(..)N> ", "sain", "sein"));
        assert!(run("a > e / <(..)_(..)N> ", "sai.ne", "sai.ne"));
        assert!(run("a > e / <(..)_(..)N> ", "sai.na", "sai.na"));
    }
 
    #[test]
    fn test_tmesis() {
        let test_rule = "* > ⟨blu⟩:[+sec.stress] ⟨mɪn⟩ / %_%:[+stress]";
        assert!(run(test_rule, "ˌab.soˈlut.ly", "ˌab.soˌblu.mɪnˈlut.ly"));
    }

    #[test]
    fn test_grouped_env() {
        let test_rule = "V:[+long, +hi, +rnd]=1 => ə 1:[-tens,-syll] | :{ _C:[+lab], j_ }:";
        assert!(run(test_rule, "su:p", "suːp"));
        assert!(run(test_rule, "ju:", "juː"));
        assert!(run(test_rule, "ju:p", "juːp"));
        assert!(run(test_rule, "bu:t", "bəwt"));

        let test_rule = "a => e / :{ _C:[+lab], j_ }:";
        assert!(run(test_rule, "sap", "sep"));
        assert!(run(test_rule, "jap", "jep"));
        assert!(run(test_rule, "jal", "jel"));
        assert!(run(test_rule, "sal", "sal"));

        let test_rule = "a => e | :{ _ }:";
        assert!(run(test_rule, "sap", "sap"));
        assert!(run(test_rule, "jap", "jap"));
        assert!(run(test_rule, "jal", "jal"));
        assert!(run(test_rule, "sal", "sal"));

        let test_rule = "a => e / :{ _{p,t,k}, _{b,d,g} }:";
        assert!(run(test_rule, "satad", "seted"));
        assert!(run(test_rule, "salad", "saled"));
    }

    #[test]
    fn test_word_boundary_deletion() {
        let test_rule = "## > *";
        assert!(run(test_rule, "a nif", "a.nif"));
        assert!(run(test_rule, "a nif te", "a.nif.te"));

        let test_rule = "## > * / _n";
        assert!(run(test_rule, "a nif", "a.nif"));
        assert!(run(test_rule, "a nif te", "a.nif te"));

        let test_rule = "## > * / a_";
        assert!(run(test_rule, "a nif", "a.nif"));
        let test_rule = "## > * / a_n";
        assert!(run(test_rule, "a nif", "a.nif"));

        let test_rule = "## > * | _n";
        assert!(run(test_rule, "a nif", "a nif"));
        assert!(run(test_rule, "a nif te", "a nif.te"));
        
        let test_rule = "a## > *";
        assert!(run(test_rule, "a nif te", "nif te"));
        assert!(run(test_rule, "da nif te", "d.nif te"));
        
        let test_rule = "<..a>## > *";
        assert!(run(test_rule, "da nif te", "nif te"));

        let test_rule = "##<..i..> > *";
        assert!(run(test_rule, "da nif te", "da te"));
        let test_rule = "<..i..>## > *";
        assert!(run(test_rule, "da nif te", "da te"));
        let test_rule = "<..i..> > * / _##";
        assert!(run(test_rule, "da nif te", "da te"));
    }

    #[test]
    fn test_french_contraction() {
        assert!(run("ə## > * / ʒ_", "ʒə sɥi", "ʒ.sɥi"));
        assert!(run("ʒ$s > ʃ", "ʒ.sɥi", "ʃɥi"));
    }

    #[test]
    fn test_extl_boundary_matching() {
        let test_rule = "a > e / _##";
        assert!(run(test_rule, "a na", "e na"));
        assert!(run(test_rule, "a na da", "e ne da"));

        let test_rule = "a > e / _##n";
        assert!(run(test_rule, "a na", "e na"));
        assert!(run(test_rule, "a nan", "e nan"));
        assert!(run(test_rule, "a na da", "e na da"));

        let test_rule = "a > e / #_##";
        assert!(run(test_rule, "a ha a", "e ha a"));
        assert!(run(test_rule, "a a a", "e e a"));

        let test_rule = "a > e / _,##";
        assert!(run(test_rule, "a a a", "e e e"));
    }

    #[test]
    fn test_extl_sandhi() {
        let test_rule = " b > v / n##_";
        assert!(run(test_rule, "an ban", "an van"));

        let test_rule = "C:[+cor] > [-ant, -dist] / r##_";
        assert!(run(test_rule, "væːr sɔ ɡuː", "væːr ʂɔ ɡuː"));
        assert!(run(test_rule, "væːr tɔ", "væːr ʈɔ"));

        let test_rule = "C:[+cor] > [-ant, -dist] / r(##)_";
        assert!(run(test_rule, "væːr sɔ væːr.sɔ væːrsɔ", "væːr ʂɔ væːr.ʂɔ væːrʂɔ"));
        assert!(run(test_rule, "væːr tɔ væːr.tɔ væːrtɔ", "væːr ʈɔ væːr.ʈɔ væːrʈɔ"));

        
        let test_rule = "r > * / _##[-ant, -dist] ";
        assert!(run(test_rule, "væːr ʂɔ ɡuː", "væː ʂɔ ɡuː"));

        let test_rule = "r > * / _(##)[-ant, -dist] ";
        assert!(run(test_rule, "væːr ʂɔ væːr.ʂɔ væːrʂɔ", "væː ʂɔ væː.ʂɔ væːʂɔ"));
    }

    #[test]
    fn test_phrase_padding() {
        let test_rule = " a > e";
        assert!(run(test_rule, "an ban",   "en ben"));
        assert!(run(test_rule, "an  ban",  "en  ben"));
        assert!(run(test_rule, "an   ban", "en   ben"));
        assert!(run(test_rule, "an   ban ", "en   ben"));
        assert!(run(test_rule, " an ban",  "en ben")); // TODO: See NOTE in Phrase::render()
    }
}