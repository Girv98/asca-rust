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

