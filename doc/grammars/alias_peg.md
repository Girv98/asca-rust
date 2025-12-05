```peg
Alias   ← Into / From

Into    ← Replacement Arrow Output Eol
From    ← Input Arrow Replacement Eol

Replacement ← ReplaceTerm ( ',' ReplaceTerm )* ','?
ReplaceTerm ← Empty / Plus? ReplaceChar+
ReplaceChar ← Escape / ValidChar

Escape          ← NamedEscape / UnicodeEscape / LiteralEscape
NamedEscape     ← '@' '{' [A-Za-z]+ '}'
UnicodeEscape   ← '\' 'u' '{' [0-9A-F]+ '}'
LiteralEscape   ← '\' SpecChar

Output  ← OutTerm ( ',' OutTerm )* ','?
OutTerm ← Empty / SyllBound / Segment+

Input   ← InpTerm ( ',' InpTerm )* ','?
InpTerm ← SyllBound / Segment+

Segment     ← IPA (':' Params)? / Group (':' Params)? / Params
Group       ← [A-Z]
Params      ← '[' (Argument (',' Argument)*)? ']' 
Argument    ← ArgModifier [a-zA-Z]+ / Tone
ArgModifier ← '+' / '-'
Tone        ← [a-zA-Z]+ ':' [0-9]+ 

ValidChar   ← !(SpecChar / Whitespace) .
SpecChar    ← '\' / '@' / '$' / '∅' / '*' / '>' / '=' / '+' / '-' / ','
Plus        ← '+'
Empty       ← '*' / '∅'   
SyllBound   ← '$'
Arrow       ← ('='/'-')? '>'  

IPA         ← PreNasal? IpaChar (Tie IpaChar)? IpaDiacrit*
PreNasal    ← 'ᵐ' / 'ⁿ' / 'ᶯ' / 'ᶮ' / 'ᵑ' / 'ᶰ'
IpaChar     ← [<Unicode IPA Character>]
Tie         ← '^' / [U+0361] / [U+035C]
IpaDiacrit  ← !PreNasal [<Unicode Diacritic Character>]

Whitespace  ← [<Whitespace>]
Eol         ← [U+000A] / Eof
Eof         ← !.
```