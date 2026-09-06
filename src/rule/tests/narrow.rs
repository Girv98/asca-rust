use crate::{error::RuleSyntaxError, rule::tests::setup_rule};

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
    assert!(run("[]:-n > [+s.g.]",         "kan",   "kʰa̤n"));
    assert!(run("[]:-n > [+s.g.]",         "kan:a", "kʰa̤n:a̤"));
    assert!(run("[]:-n:[+long] > [+s.g.]", "kan:a", "kʰa̤n:a̤"));
    assert!(run("[]:-n:[-long] > [+s.g.]", "kan:a", "kʰa̤nʱ:a̤"));
}


#[test]
fn error() {
    let Err(res) = setup_rule("C:-{[]:-a} > [+s.g.]") else { assert!(false); return };
    assert!(matches!(res, RuleSyntaxError::Narrowseption(_)));
    
    let Err(res) = setup_rule("C:-{N=1} > [+s.g.]") else { assert!(false); return };
    assert!(matches!(res, RuleSyntaxError::NarrowReference(_)));

    let Err(res) = setup_rule("C:-{sasds,} > [+s.g.]") else { assert!(false); return };
    assert!(matches!(res, RuleSyntaxError::NarrowTooMany(_)));

    let Err(res) = setup_rule("C:-{%} > [+s.g.]") else { assert!(false); return };
    assert!(matches!(res, RuleSyntaxError::NarrowSetWrongKind(_)));

    let Err(res) = setup_rule("C:-% > [+s.g.]") else { assert!(false); return };
    assert!(matches!(res, RuleSyntaxError::NarrowWrongKind(_)));

    
    let Err(res) = setup_rule("C:-{s") else { assert!(false); return };
    assert!(matches!(res, RuleSyntaxError::UnexpectedEol(_, "'}'")));
    let Err(res) = setup_rule("C:-{sx") else { assert!(false); return };
    assert!(matches!(res, RuleSyntaxError::UnexpectedEol(_, "'}'")));
    let Err(res) = setup_rule("C:-{sasds,") else { assert!(false); return };
    assert!(matches!(res, RuleSyntaxError::NarrowTooMany(_)));
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