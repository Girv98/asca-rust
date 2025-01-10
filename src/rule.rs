use std::{
    cell::RefCell,
    collections::HashMap, 
    cmp ::max, 
    fmt, 
};

use crate   :: {
    error   :: { Error, RuleSyntaxError }, 
    parser  :: { Item, ParseElement }, 
    seg     :: NodeKind, 
    subrule :: SubRule, 
    word    :: Word, 
};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum RuleType {
    Substitution,
    Metathesis,
    Deletion,
    Insertion,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub(crate) enum Alpha {
    Node(NodeKind, Option<u8>),
    Place(PlaceMod),
    Feature(bool),
    Supra(bool),
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

    pub(crate) fn as_binary(&self) -> bool {
        match self {
            Alpha::Feature(pos) | Alpha::Supra(pos) => *pos,
            Alpha::Node(_, node_mod) => node_mod.is_some(),
            Alpha::Place(pm) => pm.lab.is_some() || pm.cor.is_some() || pm.dor.is_some() || pm.phr.is_some(),
        }
    }
}

pub(crate) struct Rule {
    pub(crate) input:     Vec<Vec<Item>>,    // to support multirules
    pub(crate) output:    Vec<Vec<Item>>,    // these need to be Vec<Vec<Item>>
    pub(crate) context:   Vec<Item>,
    pub(crate) except:    Vec<Item>,
}

impl Rule {
    pub(crate) fn new(i: Vec<Vec<Item>>, o: Vec<Vec<Item>>, c :Vec<Item>, e :Vec<Item>) -> Self {
        Self { input: i, output: o, context: c, except: e }
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
        let mut sub_vec = Vec::new();
        for i in 0..max {
            let input   = if  self.input.len() == 1 {  self.input[0].clone() } else {  self.input[i].clone() };
            let output  = if self.output.len() == 1 { self.output[0].clone() } else { self.output[i].clone() };
            let context = if self.context.is_empty() { None } else if self.context.len() == 1 { Some(self.context[0].clone()) } else { Some(self.context[i].clone()) };
            let except  = if  self.except.is_empty() { None } else if  self.except.len() == 1 { Some( self.except[0].clone()) } else { Some( self.except[i].clone()) };
            let rule_type = {
                match (&input[0].kind, &output[0].kind) {
                    (ParseElement::EmptySet, ParseElement::EmptySet) => return Err(RuleSyntaxError::InsertDelete(input[0].position.group, input[0].position.line, input[0].position.start, output[0].position.start)),
                    (ParseElement::EmptySet, ParseElement::Metathesis) => return Err(RuleSyntaxError::InsertMetath(input[0].position.group, input[0].position.line, input[0].position.start, output[0].position.start)),
                    (ParseElement::EmptySet, _) => RuleType::Insertion,
                    (_, ParseElement::EmptySet) => RuleType::Deletion,
                    (_, ParseElement::Metathesis) => RuleType::Metathesis,
                    (..) => RuleType::Substitution  
                }
            };

            sub_vec.push(
                SubRule {
                    input, 
                    output, 
                    context, 
                    except, 
                    rule_type, 
                    variables: RefCell::new(HashMap::new()), 
                    alphas: RefCell::new(HashMap::new()), 
                }
            );
        }

        Ok(sub_vec)
    }

    pub(crate) fn apply(&self, word: Word /*, trace: bool*/) -> Result<Word, Error> {
        
        let sub_rules = self.split_into_subrules()?;
        
        let mut res_word = word; 
        for i in sub_rules {
            res_word = i.apply(res_word)?;
            // println!("{i:#?} ---> {res_word:#?}");
        }
        Ok(res_word)
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
    use crate::{ASCAError, normalise, RuleGroup};

    use super::*;
    
    fn setup_rule(test_str: &str) -> Rule {
        use crate::{Lexer, Parser};

        let maybe_lex = Lexer::new(&normalise(test_str).chars().collect::<Vec<_>>(),0 ,0).get_line();
        match maybe_lex {
            Ok(lexed) => {
                match Parser::new(lexed, 0, 0).parse() {
                    Ok(rule) => return rule.unwrap(),
                    Err(e) => {
                        let rg = RuleGroup { name: String::new(), rule: vec![test_str.to_owned()], description: String::new() };
                        println!("{}", e.format_rule_error(&vec![rg]));
                        assert!(false);
                    },
                }
            },
            Err(e) => {
                let rg = RuleGroup { name: String::new(), rule: vec![test_str.to_owned()], description: String::new() };
                println!("{}", e.format_rule_error(&vec![rg]));
                assert!(false);
            },
        } 
        unreachable!()
    }

    fn setup_word(test_str: &str) -> Word {
        let maybe_word = Word::new(String::from(normalise(test_str)));
        match maybe_word {
            Ok(w) => return w,
            Err(e) => {
                println!("{}", e.format_word_error(&Vec::new()));
                assert!(false);
            },
        }
        unreachable!();
    }

    #[test]
    fn test_wildcard() {
        let test_rule = setup_rule("[] > x");
        let test_word = setup_word("a.r.i.s.a.n");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "x.x.x.x.x.x");
    
        let test_word = setup_word("arisan");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "xːːːːː");

        let test_rule = setup_rule("{[], []} > {x,w}");
        let test_word = setup_word("a.r.i.s.a.n");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "x.x.x.x.x.x");
    
        let test_word = setup_word("arisan");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "xːːːːː");

        let test_rule = setup_rule("[]=1 > 1:[+red]");
        let test_word = setup_word("a.r.i.s.a.n");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "aᵊ.rᵊ.iᵊ.sᵊ.aᵊ.nᵊ");
    
        let test_word = setup_word("arisan");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "aᵊrᵊiᵊsᵊaᵊnᵊ");

        let test_rule = setup_rule("[] > [+red]");
        let test_word = setup_word("a.r.i.s.a.n");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "aᵊ.rᵊ.iᵊ.sᵊ.aᵊ.nᵊ");
    
        let test_word = setup_word("arisan");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "aᵊrᵊiᵊsᵊaᵊnᵊ");
    }


    #[test]
    fn test_semivowel_syllabication() {
        let test_rule = setup_rule("[-syll, +approx, -lat, Ahi] > [+syll, +son, -cons, +lab, - PHR, Atense]");
        let test_word = setup_word("ʕ̞.w.ʟ");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ɑ.u.ʟ");

        let test_rule = setup_rule("[+approx, +hi] > [+syll, +tense]");
        let test_word = setup_word("wj");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ui");

        let test_rule = setup_rule("V > a");
        let test_word = setup_word("e.o");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "a.a");

        let test_rule = setup_rule("V > [-tense]");
        let test_word = setup_word("e.o");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ɛ.ɔ");

        let test_rule = setup_rule("[+approx], [+approx, +hi] > [+syll, +son, -cons, +lab, -phr], [+tense]");
        let test_word = setup_word("w.ʕ̞");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "u.ɑ");
    }

    #[test]
    fn test_spec_env() {
        let test_rule = setup_rule("V > * / _,#");
        let test_word = setup_word("a.ri.sa");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ri.s");

        let test_rule = setup_rule("C > * / _,#a");
        let test_word = setup_word("a.ri.sa");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "a.i.a");
    }

    #[test]
    fn test_debuccalisation() {
        let test_rule = setup_rule("O:[-voi] > [-cons, +c.g., -place]");
        let test_word = setup_word("p");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ʔ");

        let test_rule = setup_rule("O:[-voi, Acont] > [-cons, As.g., -Ac.g., -place, -strid]");
        let test_word = setup_word("pa.sa.ta.fa");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ʔa.ha.ʔa.ha");
        let test_word = setup_word("sa.pa.fa.pa");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ha.ʔa.ha.ʔa");
    }

    #[test]
    fn test_greek_regressive_voicing() {
        let test_rule = setup_rule("O > [Alar] / _O:[Alar]");
        let test_word = setup_word("at.ba");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ad.ba");
        let test_rule = setup_rule("O > [Alar] / _O:[Alar]");
        let test_word = setup_word("ktʰoːn");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "kʰtʰoːn");
    }

    #[test]
    fn test_germanic_a_mutation() {
        let test_rule = setup_rule("V:[+hi] > [-hi] / _ (C,2) V:[+lo] | _{j, N}");
        // let test_rule = setup_rule("V:[+hi] > [-hi] / _ (C,1:2) V:[+lo] | _{j, NC}");
        let test_word = setup_word("ˈwur.ðɑ̃");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈwor.ðɑ̃");
        let test_word = setup_word("ˈne.stɑz");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈne.stɑz");
        let test_word = setup_word("ˈwi.rɑz");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈwe.rɑz");
        let test_word = setup_word("ˈhur.nɑ̃");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈhor.nɑ̃");
        let test_word = setup_word("ˈnun.dɑz");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈnun.dɑz");
        let test_word = setup_word("ˈswem.mɑ.nɑ̃");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈswem.mɑ.nɑ̃");
        let test_word = setup_word("ˈgul.ðɑ̃");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈɡol.ðɑ̃");

        let test_word = setup_word("ˈgul.ði.jɑ.nɑ");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈɡul.ði.jɑ.nɑ");
        
        let test_word = setup_word("ˈwird.pɑz");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈwird.pɑz");
    }

    #[test]
    fn test_sub_simple_ipa() {
        let test_rule = setup_rule("r > l");
        let test_word = setup_word("la.ri.sa");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "la.li.sa");
    }

    #[test]
    fn test_sub_insert_length() {
        let test_rule = setup_rule("Vr > [+long]l");
        let test_word = setup_word("dark");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "daːlk");
    }

    #[test]
    fn  test_sub_insert_ipa() {
        let test_rule = setup_rule("a > eoi");
        let test_word = setup_word("dak");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "deoik");
    }

    #[test]
    fn  test_sub_insert_syll_bound() {
        let test_rule = setup_rule("a > a$e");
        let test_word = setup_word("'dak");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈda.ek");

        let test_rule = setup_rule("k > k$");
        let test_word = setup_word("dak");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "dak");

        let test_rule = setup_rule("k > k$a");
        let test_word = setup_word("dak");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "dak.a");      // TODO: Should this "dak.a" ?

        let test_rule = setup_rule("a > a$e");
        let test_word = setup_word("'dak.mo");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈda.ek.mo");
    }

    #[test]
    fn test_long_vowel_breaking() {
        let test_rule = setup_rule("e > ie");
        let test_word = setup_word("'pe.ma");
        println!("e > ie");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈpie.ma");

        let test_rule = setup_rule("e:[+long] > ie");
        let test_word = setup_word("'pe:.ma");
        println!("e:[+long] > ie");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈpie.ma");

        let test_rule = setup_rule("V:[-long, -hi, -lo]=1 > 1:[+hi] 1");
        let test_word = setup_word("'pe.ma");
        println!("V:[-long, -hi, -lo]=1 > 1:[+hi] 1");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈpie.ma");


        let test_rule = setup_rule("V:[+long, -hi, -lo]=1 > 1:[+hi, -long] 1:[-long]");
        println!("V:[+long, -hi, -lo]=1 > 1:[+hi, -long] 1:[-long]");
        let test_word = setup_word("'pe:.ma");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈpie.ma");
        let test_word = setup_word("'po:.ma");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈpuo.ma");

        let test_rule = setup_rule("V:[+hi, +long]=1 > ə1:[-long]");
        println!("V:[+hi, +long]=1 > ə1:[-long]");
        let test_word = setup_word("'i:s");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈəis");
        let test_word = setup_word("'hu:s");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈhəus");
    }

    #[test]
    fn test_spanish_breaking() {
        let test_rule = setup_rule("V:[+str,-lng, -hi, -lo]=1 > 1:[+hi] e");
        let test_word = setup_word("'pe.dra");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈpie.dra");
        let test_word = setup_word("'fo.go");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈfue.ɡo");
        let test_word = setup_word("'fes.ta");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈfies.ta");
        let test_word = setup_word("'por.to");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈpuer.to");
    }

    #[test]
    fn test_match_stress() {
        let test_rule = setup_rule("V > [Astress] / _C:[Astr]");
        let test_word = setup_word("pe'sa");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈpeˈsa");

        let test_rule = setup_rule("V:[Astress] > [+long] / _C:[Astr]");
        let test_word = setup_word("'pe'sa");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈpeːˈsa");
        let test_word = setup_word("pe'sa");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "peˈsa");
        let test_word = setup_word("sa'pe.sa.so");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "saˈpe.saː.so");

        let test_word = setup_word("sa'pe'sa.so");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "saˈpeːˈsa.so");

        let test_word = setup_word("'sa'pe'sa.so");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈsaːˈpeːˈsa.so");
    }

    #[test]
    fn test_sub_insert_syll() {
        let test_rule = setup_rule("a > a%:[tone:12]e");
        let test_word = setup_word("dak");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "da.ke12");
        let test_word = setup_word("dak.mo");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "da.k12.emo");  // weird, but makes sense
        let test_rule = setup_rule("a > a%:[tone:12]=1e1");
        let test_word = setup_word("dak");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "da.ke12.k12"); // also weird but makes sense

        let test_rule = setup_rule("k > k%");
        let test_word = setup_word("dak");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "dak");

        let test_rule = setup_rule("k > k%e");
        let test_word = setup_word("dak");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "dak.e");      

        let test_rule = setup_rule("k > k%:[tone: 12]e");
        let test_word = setup_word("dak");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "dak.e12");

        let test_rule = setup_rule("k > k%=1e1");
        let test_word = setup_word("dak");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "dak.e");
    }

    #[test]
    fn test_sub_syll_var() {
        let test_rule = setup_rule("% > 1:[-str] / %:[+str]=1_");
        let test_word = setup_word("keˈsa.lo");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "keˈsa.sa");
    }

    #[test]
    fn test_sub_assim() {
        let test_rule = setup_rule("V > [αround] / _C[αround]");
        let test_word = setup_word("le.ro");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "lø.ro");
    }

    #[test]
    fn test_sub_turkish_suffix_vowel_harmony() {
        let test_rule = setup_rule("V:[+hi] > [αbk, βfr, γrnd] / V:[αbk, βfr, γrnd] (C,0) _ (C) #");
        let test_word = setup_word("desun");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "desin");
        let test_word = setup_word("røstin");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "røstyn");
        let test_word = setup_word("kɨzlik");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "kɨzlɨk");
        let test_word = setup_word("sɨdyn");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "sɨdɨn");
        let test_word = setup_word("sodɨn");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "sodun");
        let test_word = setup_word("dasin");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "dasɨn");
    }

    #[test]
    fn test_optional_bounded() {
        let test_rule = setup_rule("a > e / _(C,3:6)");
        let test_word = setup_word("ak.ka.k.k.k");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ak.ke.k.k.k");

        let test_rule = setup_rule("a > e / (C,3:6)_");
        let test_word = setup_word("k.k.k.ak.ka");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "k.k.k.ek.ka");
    }

    #[test]
    fn test_optional_unbounded() {
        let test_rule = setup_rule("V > [-back, +front, +tense] / _([],0) V:[+hi, -back]");
        let test_word = setup_word("aki");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "æki");
        let test_word = setup_word("ak.k.k.ki");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "æk.k.k.ki");
    }

    #[test]
    fn test_sub_del_ipa() {
        let test_rule = setup_rule("sk > ʃ");
        let test_word = setup_word("skip");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ʃip");
    }

    #[test]
    fn test_sub_del_ipa_bordering() {
        let test_rule = setup_rule("sk > ʃ");
        let test_word = setup_word("skskip");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ʃːip");

        let test_word = setup_word("ask.skip");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "aʃ.ʃip");
    }
    #[test]
    fn test_sub_del_length() {
        let test_rule = setup_rule("Vr > [+long]");
        let test_word = setup_word("dark");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "daːk");

        let test_rule = setup_rule("V > [-long]");
        let test_word = setup_word("daːk");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "dak");
    }

    #[test]
    fn test_sub_set() {
        let test_rule = setup_rule("{p, t, k} > {b, d, g}");
        let test_word = setup_word("pa.ta.ka");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ba.da.ɡa");

        let test_rule = setup_rule("{%} > {[tone:5]}");
        let test_word = setup_word("pa.ta.ka");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "pa5.ta5.ka5");

        let test_rule = setup_rule("{%,C} > {[tone:5],C}");
        let test_word = setup_word("pa.ta.ka");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "pa5.ta5.ka5");

        let test_rule = setup_rule("{C, %} > {C, [tone:5]}");
        let test_word = setup_word("pa.at.ka");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "pa.at5.ka");

        
    }

    #[test]
    fn test_sub_alpha() {
        let test_rule = setup_rule("d > [αvoice] / _[αvoice]");
        let test_word = setup_word("ad.ha");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "at.ha");

        let test_rule = setup_rule("d > [Avoice] / _[Avoice]");
        let test_word = setup_word("ad.ha");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "at.ha");

        let test_rule = setup_rule("k > [αvoice] / [-αvoice]_");
        let test_word = setup_word("əs.kɔl");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "əs.ɡɔl");

        let test_rule = setup_rule("k > [Bvoice] / [-Bvoice]_");
        let test_word = setup_word("əs.kɔl");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "əs.ɡɔl");
    }

    #[test]
    fn test_met_simple_ipa() {
        let test_rule = setup_rule("sk > &");
        let test_word = setup_word("ˈɑːs.ki.ɑn");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈɑːk.si.ɑn");

        let test_rule = setup_rule("[+rhotic]V > & / _s");
        let test_word = setup_word("ˈhros");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈhors");

        let test_rule = setup_rule("oba > &");
        let test_word = setup_word("ˈko.ba.lo.ba");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈka.bo.la.bo");
    }

    #[test]
    fn test_manual_met() {
        let test_rule = setup_rule("[+rho]=1 V=2 > 2 1  / _s");
        let test_word = setup_word("ˈhros");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈhors");
    }

    #[test]
    fn test_met_long_dist_ipa() {
        let test_rule = setup_rule("r...l > &");
        let test_word = setup_word("ˈpa.ra.bo.la");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ˈpa.la.bo.ra");
    }

    #[test]
    fn test_met_syll_bound() {
        let test_rule = setup_rule("$s > & / V_{p,t,k}");
        let test_word = setup_word("e.spa.ɲa");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "es.pa.ɲa");
    }

    #[test]
    fn test_met_syll() {
        let test_rule = setup_rule("%% > &");
        let test_word = setup_word("sa.ro.na");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ro.sa.na");
    }

    #[test]
    fn test_met_ident() {
        let test_rule = setup_rule("V > &");
        let test_word = setup_word("saus");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "saus");
    }

    #[test]
    fn test_met_simple_mixed() {
        let test_rule = setup_rule("lVr > &");
        let test_word = setup_word("la.ri");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ra.li");
        // V matches a vowel segment of arbitrary length (user must specify [-long] if only matching short segments)
        let test_word = setup_word("la:.ri");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "raː.li");
        // But does not match different consecutive vowels
        let test_word = setup_word("lau.ri");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "lau.ri");
    }

    #[test]
    fn test_del_simple_ipa() {
        let test_rule = setup_rule("o > *");
        let test_word = setup_word("o.so.on.o");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "s.n");
    }

    #[test]
    fn test_del_mult_ipa() {
        let test_rule = setup_rule("ta > *");
        let test_word = setup_word("pa.ta.ka");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "pa.ka");

        let test_rule = setup_rule("ata > *");
        let test_word = setup_word("pa.ta.ka");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "p.ka");

        let test_rule = setup_rule("pata > *");
        let test_word = setup_word("pa.ta.ka");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ka");

        let test_rule = setup_rule("patak > *");
        let test_word = setup_word("pa.ta.ka");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "a");

        let test_rule = setup_rule("pata...a > *");
        let test_word = setup_word("pa.ta.ka");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "k");

        let test_rule = setup_rule("p...a > *");
        let test_word = setup_word("pa.ta.ka");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "a.t.ka");
    }

    #[test]
    fn test_del_syll() {
        let test_rule = setup_rule("% > * | _s");
        let test_word = setup_word("a.ske.sa.re");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "a.ske");
    }

    #[test]
    fn test_del_syll_var() {
        let test_rule = setup_rule("%=1 > * / 1_");
        let test_word = setup_word("ke.sa.sa");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ke.sa");
    }

    #[test]
    fn test_del_ipa_before_wbound() {
        let test_rule = setup_rule("t > *  / _#");
        let test_word = setup_word("kat.kat");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "kat.ka");

        let test_rule = setup_rule("[] > *  / _[]#");
        let test_word = setup_word("kat.kat");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "kat.kt");

        let test_rule = setup_rule("[] > *  / []_[]#");
        let test_word = setup_word("kat.kat");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "kat.kt");
    }

    #[test]
    fn test_del_ipa_after_wbound() {
        let test_rule = setup_rule("[] > *  / #_");
        let test_word = setup_word("kat.kat");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "at.kat");

        let test_rule = setup_rule("[] > *  / #C_");
        let test_word = setup_word("kat.kat");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "kt.kat");

        let test_rule = setup_rule("[] > *  / #[]_[]");
        let test_word = setup_word("kat.kat");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "kt.kat");

        let test_rule = setup_rule("[] > e  / #[]_[]");
        let test_word = setup_word("kat.kat");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ket.kat");
    }

    #[test]
    fn test_del_ipa_before_sbound() {
        let test_rule = setup_rule("t > *  / _$ | _#");
        let test_word = setup_word("kat.kat");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ka.kat");
    }
    
    #[test]
    fn test_del_vowel_after_vowel() {
        let test_rule = setup_rule("V > * / V_");
        let test_word = setup_word("kai.lua");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ka.lu");
    }

    #[test]
    fn test_del_matrix_after_matrix() {
        let test_rule = setup_rule("[+syll, +high] > * / [+syll, -high]_");
        let test_word = setup_word("kai.lua");
        // from Assamese, "a high vowel gets deleted following a non-high vowel"
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ka.lua");
    }

    #[test]
    fn test_except_before_simple_ipa() {
        let test_rule = setup_rule(" i > e | c_");
        let test_word = setup_word("ki.ci");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ke.ci");
    }

    #[test]
    fn test_except_before_ipa() {
        let test_rule = setup_rule(" i > e | c:[+long] _");
        let test_word = setup_word("ki.cːi");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ke.cːi");
    }

    #[test]
    fn test_except_before_ipa_bound() {
        let test_rule = setup_rule(" i > e | cc_");
        let test_word = setup_word("kic.ci");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "kec.ci");
    }

    #[test]
    fn test_except_after_simple_ipa() {
        let test_rule = setup_rule(" i > e | _c");
        let test_word = setup_word("ki.ci");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ki.ce");
    }

    #[test]
    fn test_except_after_ipa() {
        let test_rule = setup_rule(" i > e | _cc");
        let test_word = setup_word("ki.cːi");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ki.cːe");
    }

    #[test]
    fn test_except_after_ipa_bound() {
        let test_rule = setup_rule(" i > e | _cc");
        let test_word = setup_word("kic.ci");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "kic.ce");
    }

    #[test]
    fn test_except_before_ipa_bound_false() {
        let test_rule = setup_rule(" i > e | cc_");
        let test_word = setup_word("ki.ci");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ke.ce");
    }

    #[test]
    fn test_except_after_ipa_bound_false() {
        let test_rule = setup_rule(" i > e | _cc");
        let test_word = setup_word("ki.ci");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ke.ce");
    }

    #[test]
    fn test_context_set() {
        let test_rule = setup_rule("i > ɛ / _{r,h,ʍ}");
        let test_word = setup_word("si.sir");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "si.sɛr");
        
        let test_word = setup_word("si.si.haz");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "si.sɛ.haz");

        let test_word = setup_word("ri.hi.ʍaz");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "rɛ.hɛ.ʍaz");
    }

    #[test]
    fn test_insertion_segment_before_ipa() {
        let test_rule = setup_rule("* > e / _s");
        let test_word = setup_word("ski");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "eski");

        let test_rule = setup_rule("* > e / _k");
        let test_word = setup_word("ski");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "seki");
    }

    #[test]
    fn test_insertion_segment_after_ipa() {
        let test_rule = setup_rule("* > e / s_");
        let test_word = setup_word("ski");
        println!("* > e / s_");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "seki");

        let test_rule = setup_rule("* > e / s_");
        let test_word = setup_word("kas");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "kase");

        let test_rule = setup_rule("* > e / k_");
        let test_word = setup_word("ski");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "skei");
    }

    #[test]
    fn test_insertion_segment_between_ipa() {
        let test_rule = setup_rule("* > e / s_k");
        let test_word = setup_word("kskis");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "ksekis");
    }

    #[test]
    fn test_insertion_segment_before_set() {
        let test_rule = setup_rule("* > e / _{s,k}");
        let test_word = setup_word("ski");
        println!("* > e / _{{s,k}}");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "eseki");
    }

    #[test]
    fn test_insertion_segment_after_set() {
        let test_rule = setup_rule("* > e / {s,k}_");
        let test_word = setup_word("ski");
        println!("* > e / {{s,k}}_");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "sekei");
    }

    #[test]
    fn test_insertion_segment_before_matrix() {
        let test_rule = setup_rule("* > e / _C");
        let test_word = setup_word("ski");
        println!("* > e / _C");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "eseki");
    }

    #[test]
    fn test_insertion_segment_after_matrix() {
        let test_rule = setup_rule("* > e / C_");
        let test_word = setup_word("ski");
        println!("* > e / C_");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "sekei");
    }

    #[test]
    fn test_insertion_syllable_before_segment() {
        let test_rule = setup_rule("* > % / _k");
        let test_word = setup_word("ski");
        println!("* > e / C_");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "s.ki");
    }

    #[test]
    fn test_insertion_syllable_after_segment() {
        let test_rule = setup_rule("* > % / s_");
        let test_word = setup_word("ski");
        println!("* > e / C_");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "s.ki");
    }

    #[test]
    fn test_insertion_bound_before_segment() {
        let test_rule = setup_rule("* > $ / _k");
        let test_word = setup_word("ski");
        println!("* > e / C_");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "s.ki");

        // let test_rule = setup_rule("* > $ / _k");
        // let test_word = setup_word("skki");
        // println!("* > e / C_");
        // assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "skːi");
    }

    #[test]
    fn test_insertion_bound_after_segment() {
        let test_rule = setup_rule("* > $ / s_");
        let test_word = setup_word("ski");
        println!("* > e / C_");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "s.ki");
    }

    #[test]
    fn test_insertion_spanish() {
        // let test_rule = setup_rule("* > b, d / m_r, n_r");
        let test_rule = setup_rule("* > b:[Aplace] / [+nasal, Aplace]_r");
        let test_word = setup_word("om.re");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "om.bre");
    }

    #[test]
    fn test_insertion_context_syll() {
        let test_rule = setup_rule("* > e / _%");
        let test_word = setup_word("s.ki");
        println!("* > e / _%");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "se.ki");
    }

    #[test]
    fn test_insertion_context_before_syll_bound() {
        let test_rule = setup_rule("* > e / _$");
        let test_word = setup_word("s.ki");
        println!("* > e / _$");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "se.kie");
        println!();

        let test_rule = setup_rule("* > e / _$ | _#");
        let test_word = setup_word("s.ki");
        println!("* > e / _$ | _#");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "se.ki");
    }

    #[test]
    fn test_insertion_context_after_syll_bound() {
        let test_rule = setup_rule("* > e / $_");
        let test_word = setup_word("as.k");
        println!("* > e / $_");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "eas.ek");
        println!();

        let test_rule = setup_rule("* > e / $_ | #_");
        let test_word = setup_word("as.k");
        println!("* > e / $_ | #_");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "as.ek");
        println!();
    }

    #[test]
    fn test_insertion_exception_before() {
        let test_rule = setup_rule("* > e / _C | _C#");
        let test_word = setup_word("as.k");
        println!("* > e / _C | _C#");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "aes.k");
        println!();

        let test_rule = setup_rule("* > e / _C");
        let test_word = setup_word("as.k");
        println!("* > e / _C");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "aes.ek");
        println!();
    }


    #[test]
    fn test_match_alpha_feature() {
        let test_rule = setup_rule("V > [αnasal] / _[αnasal]");

        assert_eq!(test_rule.apply(setup_word("an.ti")).unwrap().render(&[]).unwrap(), "ãn.ti");
        assert_eq!(test_rule.apply(setup_word("a.na.ti")).unwrap().render(&[]).unwrap(), "ã.na.ti");
        assert_eq!(test_rule.apply(setup_word("tan")).unwrap().render(&[]).unwrap(), "tãn");
    }

    #[test]
    fn test_nasal_assim() {
        let test_rule = setup_rule("[+nasal] > [αPLACE] / _C:[αPLACE] | _[-place]");
        assert_eq!(test_rule.apply(setup_word("ˈsɑm.dɑz")).unwrap().render(&[]).unwrap(), "ˈsɑn.dɑz");
        assert_eq!(test_rule.apply(setup_word("ˈhʊng")).unwrap().render(&[]).unwrap(), "ˈhʊŋɡ");
        assert_eq!(test_rule.apply(setup_word("ˈɪn.pʊt")).unwrap().render(&[]).unwrap(), "ˈɪm.pʊt");

        assert_eq!(test_rule.apply(setup_word("samk")).unwrap().render(&[]).unwrap(), "saŋk");
        assert_eq!(test_rule.apply(setup_word("sang")).unwrap().render(&[]).unwrap(), "saŋɡ");
        assert_eq!(test_rule.apply(setup_word("sanp")).unwrap().render(&[]).unwrap(), "samp");
        assert_eq!(test_rule.apply(setup_word("sanf")).unwrap().render(&[]).unwrap(), "saɱf");
        assert_eq!(test_rule.apply(setup_word("sanq")).unwrap().render(&[]).unwrap(), "saɴq");
        assert_eq!(test_rule.apply(setup_word("san?")).unwrap().render(&[]).unwrap(), "sanʔ");
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
                println!("{}", w.render(&[]).unwrap());
                w = rule.apply(w).unwrap();
            }
            output_words.push(w)
        }

        for (w, m) in output_words.iter().zip(output_matchs) {
            assert_eq!(w.render(&[]).unwrap(), m.render(&[]).unwrap());
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

        let test_rule = setup_rule("O:[-voi, -cor] > [+cont] / _{t,s}");
        
        assert_eq!(test_rule.apply(setup_word("ˈɑp.ter")).unwrap().render(&[]).unwrap(), "ˈɑɸ.ter");
        assert_eq!(test_rule.apply(setup_word("ˈɑp.sɑn")).unwrap().render(&[]).unwrap(), "ˈɑɸ.sɑn");

        assert_eq!(test_rule.apply(setup_word("ˈɑk.ter")).unwrap().render(&[]).unwrap(), "ˈɑx.ter");
        assert_eq!(test_rule.apply(setup_word("ˈɑk.sɑn")).unwrap().render(&[]).unwrap(), "ˈɑx.sɑn");

        let test_rule = setup_rule("O:[+cor] O:[+cor] > [+cont, +strid] [+cont, +strid]");

        assert_eq!(test_rule.apply(setup_word("ˈɑt.ter")).unwrap().render(&[]).unwrap(), "ˈɑs.ser");
        assert_eq!(test_rule.apply(setup_word("ˈɑt.sɑn")).unwrap().render(&[]).unwrap(), "ˈɑs.sɑn");
    }

    #[test]
    fn test_match_ipa_alpha_feature() {
        let test_rule = setup_rule("l:[Asyll] > r:[Asyll]");
        
        assert_eq!(test_rule.apply(setup_word("lak")).unwrap().render(&[]).unwrap(), "rak");
        assert_eq!(test_rule.apply(setup_word("wl̩k")).unwrap().render(&[]).unwrap(), "wr̩k");
    }

    #[test]
    fn test_grimms_law() {
        let test_rule = setup_rule("[+cons, -son, -voice, -cont], [+cons, -son, +voice, -cont, -sg], [+cons, +voice, +sg] > [+cont], [-voice], [-sg]");
        assert_eq!(test_rule.apply(setup_word("kumˈtom")).unwrap().render(&[]).unwrap(), "xumˈθom");
        assert_eq!(test_rule.apply(setup_word("kunˈtos")).unwrap().render(&[]).unwrap(), "xunˈθos");
        assert_eq!(test_rule.apply(setup_word("ˈdant")).unwrap().render(&[]).unwrap(), "ˈtanθ");
        assert_eq!(test_rule.apply(setup_word("ˈme.dʱu")).unwrap().render(&[]).unwrap(), "ˈme.du");
        assert_eq!(test_rule.apply(setup_word("ˈkʷod")).unwrap().render(&[]).unwrap(), "ˈxʷot");
        assert_eq!(test_rule.apply(setup_word("'ɡʱans")).unwrap().render(&[]).unwrap(), "ˈɡans");
        assert_eq!(test_rule.apply(setup_word("'ɡʷʱels")).unwrap().render(&[]).unwrap(), "ˈɡʷels");

        let test_rule = setup_rule("p, t, k, kʷ, b, d, g, gʷ, bʱ, dʱ, gʱ, gʷʱ > ɸ, θ, x, xʷ, p, t, k, kʷ, b, d, g, gʷ");
        assert_eq!(test_rule.apply(setup_word("kunˈtos")).unwrap().render(&[]).unwrap(), "xunˈθos");
        assert_eq!(test_rule.apply(setup_word("ˈdant")).unwrap().render(&[]).unwrap(), "ˈtanθ");
        assert_eq!(test_rule.apply(setup_word("ˈme.dʱu")).unwrap().render(&[]).unwrap(), "ˈme.du");
        assert_eq!(test_rule.apply(setup_word("ˈkʷod")).unwrap().render(&[]).unwrap(), "ˈxʷot");
        assert_eq!(test_rule.apply(setup_word("'ɡʱans")).unwrap().render(&[]).unwrap(), "ˈɡans");
        assert_eq!(test_rule.apply(setup_word("'ɡʷʱels")).unwrap().render(&[]).unwrap(), "ˈɡʷels");
    }

    #[test]
    fn test_verners_law() {
        let test_rule = setup_rule("[-voice, +cont] > [+voice] / V:[-stress]([+cons, +son])_");
        
        assert_eq!(test_rule.apply(setup_word("xumˈθom")).unwrap().render(&[]).unwrap(), "xumˈðom");
        assert_eq!(test_rule.apply(setup_word("xunˈθos")).unwrap().render(&[]).unwrap(), "xunˈðos");
        assert_eq!(test_rule.apply(setup_word("fɑˈθer")).unwrap().render(&[]).unwrap(), "fɑˈðer");
        assert_eq!(test_rule.apply(setup_word("uˈɸer")).unwrap().render(&[]).unwrap(), "uˈβer");
        assert_eq!(test_rule.apply(setup_word("ɑɸ")).unwrap().render(&[]).unwrap(), "ɑβ");
        assert_eq!(test_rule.apply(setup_word("ˈme.du")).unwrap().render(&[]).unwrap(), "ˈme.du");
    }

    #[test]
    fn test_pgmc_stress_shift() {
        let test_rule = setup_rule("%:[+stress], % > [-stress], [+stress] / _, #_");
        assert_eq!(test_rule.apply(setup_word("xunˈðos")).unwrap().render(&[]).unwrap(), "ˈxun.ðos");
        assert_eq!(test_rule.apply(setup_word("fɑˈðer")).unwrap().render(&[]).unwrap(), "ˈfɑ.ðer");
        assert_eq!(test_rule.apply(setup_word("uˈβer")).unwrap().render(&[]).unwrap(), "ˈu.βer");
    }

    #[test]
    fn test_japanese_devoicing() {
        // let test_rule = setup_rule("V > [-voice] / [-voi]_{[-voi], #}");
        let test_rule = setup_rule("V > [-voice] / [-voi]_[-voi], [-voi]_#");

        assert_eq!(test_rule.apply(setup_word("de.sɯ")).unwrap().render(&[]).unwrap(), "de.sɯ̥");
    }

    #[test]
    fn test_latin_stress() {
        let test_rule = setup_rule("% => [+str] / #_#");
        assert_eq!(test_rule.apply(setup_word("sar")).unwrap().render(&[]).unwrap(), "ˈsar");
        let test_rule = setup_rule("V:[+lng]=> [+str] / _%#");
        assert_eq!(test_rule.apply(setup_word("peː.diː.kaː.boː")).unwrap().render(&[]).unwrap(), "peː.diːˈkaː.boː");
        // let test_rule = setup_rule("V => [+str] / _C%#");
        let test_rule = setup_rule("C => [+str] / _%#");
        assert_eq!(test_rule.apply(setup_word("kae̯.sar")).unwrap().render(&[]).unwrap(), "ˈkae̯.sar");
        assert_eq!(test_rule.apply(setup_word("de.kem.ber")).unwrap().render(&[]).unwrap(), "deˈkem.ber");
        let test_rule = setup_rule("% => [+str] / _%:[-str]%#");
        assert_eq!(test_rule.apply(setup_word("juː.li.us")).unwrap().render(&[]).unwrap(), "ˈjuː.li.us");
        assert_eq!(test_rule.apply(setup_word("a.ba.ki.noː")).unwrap().render(&[]).unwrap(), "aˈba.ki.noː"); 

        let test_rule = setup_rule("%, V:[+lng], C, % => [+str] / #_#, _%#, _%#, _%:[-str]%#");
        assert_eq!(test_rule.apply(setup_word("sar")).unwrap().render(&[]).unwrap(), "ˈsar");
        assert_eq!(test_rule.apply(setup_word("peː.diː.kaː.boː")).unwrap().render(&[]).unwrap(), "peː.diːˈkaː.boː");
        assert_eq!(test_rule.apply(setup_word("kae̯.sar")).unwrap().render(&[]).unwrap(), "ˈkae̯.sar");
        assert_eq!(test_rule.apply(setup_word("de.kem.ber")).unwrap().render(&[]).unwrap(), "deˈkem.ber");
        assert_eq!(test_rule.apply(setup_word("juː.li.us")).unwrap().render(&[]).unwrap(), "ˈjuː.li.us");
        assert_eq!(test_rule.apply(setup_word("a.ba.ki.noː")).unwrap().render(&[]).unwrap(), "aˈba.ki.noː"); 
        assert_eq!(test_rule.apply(setup_word("sep.ti.mus")).unwrap().render(&[]).unwrap(), "ˈsep.ti.mus"); 
        assert_eq!(test_rule.apply(setup_word("sep.tem.ber")).unwrap().render(&[]).unwrap(), "sepˈtem.ber"); 
    }

    #[test]
    fn test_haplology() {
        let test_rule = setup_rule("%=1 > * / 1_");
        assert_eq!(test_rule.apply(setup_word("hap.lo.lo.ɡi")).unwrap().render(&[]).unwrap(), "hap.lo.ɡi");
        assert_eq!(test_rule.apply(setup_word("nu.tri.tri")).unwrap().render(&[]).unwrap(), "nu.tri");
        assert_eq!(test_rule.apply(setup_word("tra.ɡi.co.co.mi.co")).unwrap().render(&[]).unwrap(), "tra.ɡi.co.mi.co");
        assert_eq!(test_rule.apply(setup_word("nar.si.si.zm")).unwrap().render(&[]).unwrap(), "nar.si.zm");
        assert_eq!(test_rule.apply(setup_word("mor.fo.fo.no.lo.ɡi")).unwrap().render(&[]).unwrap(), "mor.fo.no.lo.ɡi");
    }

    #[test]
    fn test_engala_thingy() {
        let test_rule = setup_rule("O:[+nas, Aplace]=1,$N > n:[Aplace]1:[-nas], & / _ , V_C");
        assert_eq!(test_rule.apply(setup_word("a.ᵐbo")).unwrap().render(&[]).unwrap(), "am.bo");
    }

    #[test]
    fn test_engala_new() {
        let test_rules = [
            // 1) Copy Vowel Insertion
            setup_rule("* > 1:[-str]%:[Astr] / #_O:[+nas]V:[Astr]=1"),
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
            setup_rule("V:[-lo, -bk, -fr] > [Abk, Bfr, +tens] / V:[Abk, Bfr]=1..._"),
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
            // setup_word("ᵑɡa'la"),
            // setup_word("'ᵐbo"),
            // setup_word("'ᵐba"),
            // setup_word("'ᵐbə"),
            // setup_word("'mo"),
            // setup_word("'ha.mi"),
            // setup_word("at.wa"),
            ];
        let output_matchs = [
            setup_word("eˈŋæ.la"),
            // setup_word("e.ŋaˈlæ"),
            // setup_word("uˈmo"),
            // setup_word("eˈmæ"),
            // setup_word("iˈme"),
            // setup_word("ˈmo"),
            // setup_word("'hæ.mi"),
            // setup_word("ak.wa"),
        ];

        let mut output_words: Vec<Word> = vec![];

        for word in &test_words {
            let mut w = word.clone();
            for (ri, rule) in test_rules.iter().enumerate() {
                println!("-- {} --", ri+1);
                println!("{}", w.render(&[]).unwrap());
                w = match rule.apply(w) {
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
            assert_eq!(w.render(&[]).unwrap(), m.render(&[]).unwrap());
        }
    }

    // #[test]
    // fn test_engala_old() {
    //     let test_rules = [
    //         // 1) Copy Vowel Insertion
    //         setup_rule("* > 1:[-str]%:[Astr] / #_O:[+nas]V:[Astr]=1"),
    //         // setup_rule("* > 1 / #_O:[+nas]V=1"),
    //         // setup_rule("* > %:[Astr] / #V_O:[+nas, Astr]"),
    //         // setup_rule("%:[+str] > [-str] / _%:[+str]"),
    //         // 2) Nasal Assimilation
    //         setup_rule("[+nas] > [Aplace] / _C:[Aplace]"),
    //         // 3) Pre-nasalised Cons Simp
    //         setup_rule("O:[+nas, Aplace]=1 > n:[Aplace]1:[-nas]"),
    //         setup_rule("$N > & / V_C"),
    //         // 4) Hiatus Res
    //         setup_rule("$ > * / V_V"),
    //         setup_rule("ai, au, əi, əu > aj, aw, e, o"),
    //         setup_rule("i, u > [-syl] / _V:[-hi]"),
    //         setup_rule("əa, aə > ə:[+long], a:[+long]"),
    //         // 5) Pre-nasal Raising
    //         // setup_rule("{ə, a, e, o} > {ɨ, ə, i, u} / _N{O,#}"),
    //         setup_rule("V:[-hi, -lo] > [+hi, +tense -red] / _N{O,#}"),
    //         setup_rule("V:[-hi, +lo] > [-lo, +red] / _N{O,#}"),
    //         // 6) Central Vowel Annihilation Part Un
    //         setup_rule("V:[-fr, -bk, +str] > [+fr, +tens, -red]"),
    //         // 7) Progressive Assimilation
    //         setup_rule("V:[-lo, -bk, -fr] > [Abk, Bfr, +tens] / V:[Abk, Bfr]=1..._"),
    //         // 8) Post-Nasal Lenition
    //         setup_rule("O:[-cont, +voi] > * / N_"),
    //         setup_rule("N$ > & / V_V"),
    //         setup_rule("O:[-cont, -voi] > [+voi] / N_"),
    //         setup_rule("* > t:[Avoi] / n_C:[+cont, Avoi, +cor]"), // n_{s,z,l}
    //         setup_rule("{f, x} > h / N_"),
    //         // 9) Weak Fricative Lenition
    //         setup_rule("{f, x} > [+voi] / V_V"),
    //         // 10) Voiceless Stop Lenition 
    //         setup_rule("C:[-cont] > [+voi] / V_V"),
    //         // 11) Central Vowel Annihilation Part Deux
    //         setup_rule("V:[-fr, -bk, -lo] > [+fr, +tens, -red]"),
            
    //     ];

    //     let test_words = [
    //         setup_word("'ᵑɡa.la"),
    //         setup_word("ᵑɡa'la"),
    //         setup_word("'ᵐbo"),
    //         setup_word("'ᵐba"),
    //         setup_word("'ᵐbə"),
    //         setup_word("'mo"),
    //         setup_word("'ha.mi"),
    //     ];
    //     let output_matchs = [
    //         setup_word("eˈŋæ.la"),
    //         setup_word("e.ŋaˈlæ"),
    //         setup_word("uˈmo"),
    //         setup_word("eˈmæ"),
    //         setup_word("iˈme"),
    //         setup_word("ˈmo"),
    //         setup_word("'hæ.mi"),
    //     ];

    //     let mut output_words: Vec<Word> = vec![];

    //     for word in &test_words {
    //         let mut w = word.clone();
    //         for (ri, rule) in test_rules.iter().enumerate() {
    //             println!("-- {} --", ri+1);
    //             println!("{}", w.render(&[]).unwrap());
    //             w = rule.apply(w).unwrap();
    //         }
    //         output_words.push(w)
    //     }

    //     for (w, m) in output_words.iter().zip(output_matchs) {
    //         assert_eq!(w.render(&[]).unwrap(), m.render(&[]).unwrap());
    //     }
    // }

    #[test]
    fn test_prop() {

        // V > [α front, β back] > V:[α front, β back]C_	
        // /sinotehu/ becomes /sinøtehy/, not /sinøtɤhy/

        let test_rule = setup_rule("V > [α front, β back] / V:[α front, β back]C_");
        assert_eq!(test_rule.apply(setup_word("si.no.te.hu")).unwrap().render(&[]).unwrap(), "si.nø.te.hy");

        // V > [α front, β back] / _CV:[α front, β back]
        // /sinotehu/ becomes /sɯnøtɤhu/, note no propagation

        let test_rule = setup_rule("V > [α front, β back] / _CV:[α front, β back]");
        assert_eq!(test_rule.apply(setup_word("si.no.te.hu")).unwrap().render(&[]).unwrap(), "sɯ.nø.tɤ.hu");

        // V > [α front, β back] / _...V:[α front, β back]#
        // /sinotehu/ becomes /sɯnotɤhu/, as expected

        let test_rule = setup_rule("V > [α front, β back] / _...V:[α front, β back]#");
        assert_eq!(test_rule.apply(setup_word("si.no.te.hu")).unwrap().render(&[]).unwrap(), "sɯ.no.tɤ.hu");
    }

    #[test]
    fn test_norwegian_lengthening() {
        let test_rule = setup_rule("V > [Along] / _C:[-Along]");
        let test_word = setup_word("san:");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "sanː");
        let test_word = setup_word("san");
        assert_eq!(test_rule.apply(test_word).unwrap().render(&[]).unwrap(), "saːn");
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
            setup_rule("[+cons, -syll]$ > & / _[+son, +cont]"),
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
            setup_rule("V:[-low, +front] > [αhigh] / _,V:[-low, +back, αhigh]"),
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
            setup_rule("ɐ:[+stress, Along], ə > [-tens], ɐ"),
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
                // println!("{}", w.render(&[]).unwrap());
                w = match rule.apply(w) {
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
            assert_eq!(w.render(&[]).unwrap(), m.render(&[]).unwrap());
        }   
    }


    #[test]
    fn test_clicks() {
        let test_rule = setup_rule("[+clk] > [+dr]");
        
        assert_eq!(test_rule.apply(setup_word("ɴǃa")).unwrap().render(&[]).unwrap(), "ǃɴa");
    }
}