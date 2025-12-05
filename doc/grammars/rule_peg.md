``` peg
RuleBlock ← Line*

Line ← <Whitespace> (Comment / Rule)
Rule ← Input Arrow Output ContBlock? ExptBlock? Terminal                        // NOTE: InpTerm cannot be Empty when corresponding OutTerm is (Ampersand / Empty) 

Input   ← InpTerm ( ',' InpTerm )*
InpTerm ← Empty / InpElem+
InpElem ← OptEllipsis / Ellipsis / CrossBound / SyllBound / Term

Output  ← OutTerm  ( ',' OutTerm )*
OutTerm ← Ampersand / Empty / OutElem+
OutElem ← Syll / Struct / Set / Segment / Reference / SyllBound                 // NOTE: 'Set' here only makes sense if it corresponds to a Set in INP

ContBlock   ← '/' EnvExpr
ExptBlock   ← Pipe EnvExpr

EnvExpr     ← EnvSpec / Env (',' Env)*
EnvSpec     ← Underline ',' EnvElem+                                            // e.g. _,# ==> #_ , _#
Env         ← EnvSet / EnvStat  
EnvSet      ← ':{' EnvStat (',' EnvStat)* '}:'                                  // i.e. :{ ... }:
EnvStat     ← WordBound? EnvElem* EnvCenter EnvElem* WordBound?
EnvElem     ← CrossBound / SyllBound / OptEllipsis / Ellipsis / Option / Term
EnvCenter   ← UndStruct / Underline
UndStruct   ← '<' SyllTerm* Underline SyllTerm* '>' (':' Params)?               // TODO RefAssign?

Term        ← Syll / Struct / Set / Segment / Reference
Syll        ← '%' (':' Params)? RefAssign?
Struct      ← '<' SyllTerm* '>' (':' Params)? RefAssign?
SyllTerm    ← Segment / OptEllipsis / Ellipsis / Reference / Set / Option       // NOTE: Boundaries and Syllables inside a struct are runtime invalid
Set         ← '{' SetTerm (',' SetTerm)* ','? '}'                               // NOTE: At the moment, we can't have multi-segment sets i.e. "{nd}" is not allowed 
SetTerm     ← Reference / Segment / Boundary / Syll                             // NOTE: WordBound not valid in input/output
Option      ← '(' (OptTerm+ (',' [0-9]* (':' [1-9]+)?)?)? ')'
OptTerm     ← CrossBound / Boundary / Syll / Set / Segment / Reference
Segment     ← IPA (':' Params)? / Matrix RefAssign?
Matrix      ← Group / Params
Reference   ← [0-9]+ (':' Params)?
RefAssign   ← '=' [0-9]+

Group	    ← [A-Z] (':' Params)?
Params      ← '[' (Argument (','? Argument)*)? ','? ']'
Argument    ← ArgModifier [a-zA-Z]+ / Tone
Tone        ← [a-zA-Z]+ ':' [0-9]+
ArgModifier ← AlphaMod / BinaryMod
AlphaMod    ← '-'? [α-ωA-Z]
BinaryMod   ← '+' / '-'

Empty       ← '*' / '∅'
Ampersand   ← '&'
CrossBound  ← WordBound WordBound
Boundary	← WordBound / SyllBound
WordBound   ← '#'
SyllBound   ← '$'
OptEllipsis ← '(' Ellipsis ')'
Ellipsis    ← '...' / '..' / '…'
Arrow       ← ForArrow / RevArrow
ForArrow    ← ('='/'-')? '>'
RevArrow    ← '~' '>'?
Pipe        ← '|' / '//'
Underline   ← '_'+

IPA         ← PreNasal? IpaNucleus IpaDiacrit*
PreNasal    ← 'ᵐ' / 'ⁿ' / 'ᶯ' / 'ᶮ' / 'ᵑ' / 'ᶰ'
IpaNucleus  ← Click / IpaChar (Tie IpaChar)?
Click       ← PlaceChar Tie? ClickChar / ClickChar Tie? PlaceChar
PlaceChar   ← 'k' / 'ɡ' / 'ŋ' / 'q' / 'ɢ' / 'ɴ'
ClickChar   ← 'ʘ' / 'ǀ' / 'ǃ' / 'ǁ' / '‼' / 'ǂ'
Tie         ← '^' / [U+0361] / [U+035C]
IpaChar     ← [<Unicode IPA Character>]                             // NOTE: As defined in `cardinals.json`
IpaDiacrit  ← !PreNasal [<Unicode Diacritic Character>]             // NOTE: As defined in `diacritics.json`

Terminal ← Comment / Eol
Comment  ← ';;' (!Eol .)* Eol
Eol      ← [U+000D]? [U+000A] / Eof
Eof      ← !.
```