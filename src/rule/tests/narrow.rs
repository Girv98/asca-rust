use crate::{
    error::{ASCAError, RuleRuntimeError, RuleSyntaxError}, 
    rule ::tests::{setup_phrase, setup_rule}
};

use super::run;

#[test]
fn matrix() {
    // C:[-nasal] is not necessarily the same as C:-N
    // C:-N => [+cons, -syll] and NOT [+son, -approx, +nasal]
    // While C:[-nasal] => [+cons, -syll, -nasal]
    assert!(run("C:-N > [+s.g.]",              "kan", "kʰan"));
    assert!(run("[+cons, -syll]:-N > [+s.g.]", "kan", "kʰan"));

}

#[test]
fn set_matrix() {
    assert!(run("C:-{N} > [+s.g.]",          "kan", "kʰan"));
    assert!(run("-V:-{N} > [+s.g.]",         "kan", "kʰan"));
    assert!(run("-G:-{N} > [+s.g.]",         "kan", "kʰa̤n"));
    assert!(run("-G:-{[+nasal]} > [+s.g.]",  "kan", "kʰa̤n"));
    assert!(run("-G:-{[-nasal]} > [+s.g.]",  "kan", "kanʱ"));

    assert!(run("{C:-{N}} > [+s.g.]",          "kan", "kʰan"));
    assert!(run("{-V:-{N}} > [+s.g.]",         "kan", "kʰan"));
    assert!(run("{-G:-{N}} > [+s.g.]",         "kan", "kʰa̤n"));
    assert!(run("{-G:-{[+nasal]}} > [+s.g.]",  "kan", "kʰa̤n"));
    assert!(run("{-G:-{[-nasal]}} > [+s.g.]",  "kan", "kanʱ"));
}

#[test]
fn set_segment() {
    assert!(run("{-G}      > [+s.g.]", "kan", "kʰa̤nʱ"));
    assert!(run("{-G:-{n}} > [+s.g.]", "kan", "kʰa̤n"));
    assert!(run("{-G:-{n}} > [+s.g.]", "kan", "kʰa̤n"));

    assert!(run("{-G:-{n:[+long]}} > [+s.g.]", "ka:n:a", "kʰa̤:n:a̤"));
    assert!(run(" -G:-{n:[+long]}  > [+s.g.]", "ka:n:a", "kʰa̤:n:a̤"));

    assert!(run("  n:[-long]  > [+s.g.]", "ka:n:a", "ka:n:a"));
    assert!(run("{ n:[-long]} > [+s.g.]", "ka:n:a", "ka:n:a"));
    assert!(run("{-n:[+long]} > [+s.g.]", "ka:n:a", "kʰa̤:n:a̤"));

    assert!(run(" n:[-long] > [+s.g.]", "ka:na", "ka:nʱa"));
    assert!(run("-n:[-long] > [+s.g.]", "ka:na", "kʰa̤:na̤"));
}

#[test]
fn segment() {
    assert!(run("C:-n > [+s.g.]",          "kan",   "kʰan"));
    assert!(run("C:-[+nas] > [+s.g.]",     "kan",   "kʰan"));
    assert!(run("[]:-n > [+s.g.]",         "kan",   "kʰa̤n"));
    assert!(run("[]:-n > [+s.g.]",         "kan:a", "kʰa̤n:a̤"));
    assert!(run("[]:-n:[+long] > [+s.g.]", "kan:a", "kʰa̤n:a̤"));
    assert!(run("[]:-n:[-long] > [+s.g.]", "kan:a", "kʰa̤nʱ:a̤"));
}


#[test]
fn error() {
    let res = setup_rule("C:-{[]:-a} > [+s.g.]").unwrap_err();
    assert!(matches!(res, RuleSyntaxError::Narrowseption(_)));
    
    let res = setup_rule("C:-{N=1} > [+s.g.]").unwrap_err();
    assert!(matches!(res, RuleSyntaxError::NarrowReference(_)));

    let res = setup_rule("C:-{sasds,} > [+s.g.]").unwrap_err();
    assert!(matches!(res, RuleSyntaxError::NarrowTooMany(_)));

    let res = setup_rule("C:-{%} > [+s.g.]").unwrap_err();
    assert!(matches!(res, RuleSyntaxError::NarrowSetWrongKind(_)));

    let res = setup_rule("C:-% > [+s.g.]").unwrap_err();
    assert!(matches!(res, RuleSyntaxError::NarrowWrongKind(_)));

    
    let res = setup_rule("C:-{s").unwrap_err();
    assert!(matches!(res, RuleSyntaxError::UnexpectedEol(_, "'}'")));
    let res = setup_rule("C:-{sx").unwrap_err();
    assert!(matches!(res, RuleSyntaxError::UnexpectedEol(_, "'}'")));
    let res = setup_rule("C:-{sasds,").unwrap_err();
    assert!(matches!(res, RuleSyntaxError::NarrowTooMany(_)));


    let phr = setup_phrase("kan");
    let rule = setup_rule("C > F:-s").unwrap();
    let res = rule.apply(phr.clone()).unwrap_err();
    assert!(matches!(res, ASCAError::RuleRun(RuleRuntimeError::SubstitutionNarrowing(_))));

    let rule = setup_rule("C > F:-[-stri]").unwrap();
    let res = rule.apply(phr.clone()).unwrap_err();
    assert!(matches!(res, ASCAError::RuleRun(RuleRuntimeError::SubstitutionNarrowing(_))));

    let rule = setup_rule("C > F:-{[-stri], s}").unwrap();
    let res = rule.apply(phr.clone()).unwrap_err();
    assert!(matches!(res, ASCAError::RuleRun(RuleRuntimeError::SubstitutionNarrowing(_))));
}

#[test]
fn env_matrix() {
    assert!(run("V > [+s.g.] / _C:-N",          "akan",   "a̤kan"));
    assert!(run("V > [+s.g.] / C:-N_",          "kana",   "ka̤na"));
    assert!(run("V > [+s.g.] / _{C:-N}",        "akan",   "a̤kan"));
    assert!(run("V > [+s.g.] / {C:-N}_",        "kana",   "ka̤na"));
    
    assert!(run("V > [+s.g.] / _C:-P",          "akan",   "aka̤n"));
    assert!(run("V > [+s.g.] / C:-P_",          "kana",   "kana̤"));
}

#[test]
fn env_segment() {
    assert!(run("V > [+s.g.] / _C:-n",          "akan",   "a̤kan"));
    assert!(run("V > [+s.g.] / C:-n_",          "kana",   "ka̤na"));
    
    assert!(run("V > [+s.g.] / C:-n:[+long]_",  "kana",   "ka̤na̤"));
}



#[test]
fn env_set() {
    assert!(run("V > [+s.g.] / _C:-{n}",          "akan",   "a̤kan"));
    assert!(run("V > [+s.g.] / C:-{n}_",          "kana",   "ka̤na"));
    
    assert!(run("V > [+s.g.] / C:-{n:[+long]}_",  "kana",   "ka̤na̤"));

    assert!(run("V > [+s.g.] / _C:-{P, N}",          "akalan",   "aka̤lan"));
    assert!(run("V > [+s.g.] / C:-{P, N}_",          "kalana",   "kala̤na"));
}


#[test]
fn doc() {
    assert!(run("O:-s => [+voi]",          "sa.ta.kam",   "sa.da.gam"));
    assert!(run("O:-F => [+voi]",          "sa.ta.kam",   "sa.da.gam"));
    assert!(run("O:-{s,t} => [+voi]",      "sa.ta.kam",   "sa.ta.gam"));
    assert!(run("O:-{t, [+cnt]} > [+voi]", "sa.ta.kam",   "sa.ta.gam"));
    
    assert!(run("O:-{s,t, [+dl]} => [+voi]", "sta.ta.k͡xam", "sta.ta.k͡xam"));
    assert!(run("O:-{s,t, [+dl]} => [+voi]", "sa.t͡sa.k͡xam", "sa.t͡sa.k͡xam"));
}