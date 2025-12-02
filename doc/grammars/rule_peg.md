``` peg
Line    ← Comment / Rule
Rule    ← Input Arrow Output ('/' EnvExpr)? (Pipe EnvExpr)? Eol                 // NOTE: InpTerm cannot be Empty when corresponding OutTerm is (Ampersand / Empty) 

Comment ← ';;' [<Valid Unicode Character>]* Eol
Eol     ← <End of Line> / Comment

Input   ← InpTerm ( ',' InpTerm )*
InpTerm ← Empty / InpElem+
InpElem ← OptEllipsis / Ellipsis / CrossBound / SyllBound / Term

Output  ← OutTerm  ( ',' OutTerm )*
OutTerm ← Ampersand / Empty / OutElem+
OutElem ← Syll / Struct / SET / Segment / Reference / SyllBound                 // NOTE: 'SET' here only makes sense if it corresponds to a SET in INP

EnvExpr     ← EnvSpec / Env (',' Env)*
EnvSpec     ← Underline ',' EnvElem+                                            // e.g. _,# ==> #_ , _#
Env         ← EnvSet / EnvStat  
EnvSet      ← ':{' EnvStat (',' EnvStat)* '}:'                                  // i.e. :{ ... }:
EnvStat     ← WordBound? EnvElem* EnvCenter EnvElem* WordBound?
EnvElem     ← CrossBound / SyllBound / OptEllipsis / Ellipsis / Option / Term
EnvCenter   ← EnvStruct / Underline
EnvStruct   ← '<' SyllTerm* Underline SyllTerm* '>' // (':' Params)? RefAssign?

Term        ← Syll / Struct / SET / Segment / Reference
Syll        ← '%' (':' Params)? RefAssign?
Struct      ← '<' SyllTerm* '>' (':' Params)? RefAssign?
SyllTerm    ← Segment / OptEllipsis / Ellipsis / Reference / SET / Option       // NOTE: Boundaries and Syllables inside a struct are runtime invalid
SET         ← '{' SetTerm (',' SetTerm)* '}'                                    // NOTE: At the moment, we can't have multi-segment sets i.e. "{nd}" is not allowed 
SetTerm     ← Reference / Segment / Boundary / Syll                             // NOTE: WordBound not valid in input/output
Option      ← '(' OptTerm+ (',' [0-9]+ (':' [1-9]+)?)? ')'
OptTerm     ← CrossBound / Boundary / Syll / SET / Segment / Reference
Segment     ← IPA (':' Params)? / Matrix RefAssign?
Matrix      ← Group (':' Params)? / Params
Reference   ← [0-9]+ (':' Params)?
RefAssign   ← '=' [0-9]+

Group	    ← [A-Z]
Params      ← '[' (Argument (',' Argument)*)? ']'
Argument    ← ArgModifier [a-zA-Z]+ / Tone
Tone        ← [a-zA-Z]+ ':' [0-9]+
ArgModifier ← '+' / '-' / [α-ωA-Z] / '-' [α-ωA-Z]

Empty       ← '*' / '∅'
Ampersand   ← '&'
CrossBound  ← WordBound WordBound
Boundary	← WordBound / SyllBound
WordBound   ← '#'
SyllBound   ← '$'
OptEllipsis ← '(' Ellipsis ')'
Ellipsis    ← '...' / '..' / '…'
Arrow       ← (('='/'-')? '>') / '~' '>'?
Pipe        ← '|' / '//'
Underline   ← '_'+

IPA         ← PreNasal? IpaChar (Tie IpaChar)? IpaDiacrit*
PreNasal    ← 'ᵐ' / 'ⁿ' / 'ᶯ' / 'ᶮ' / 'ᵑ' / 'ᶰ'
IpaChar     ← [<Unicode IPA Character>]                     // NOTE: As defined in `cardinals.json`
Tie         ← '^' / [U+0361] / [U+035C]
IpaDiacrit  ← !PreNasal [<Unicode Diacritic Character>]     // NOTE: As defined in `diacritics.json`
```