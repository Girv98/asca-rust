``` peg
LINE    ←   COMMENT / RULE
RULE    ←   INP ARR OUT ('/' ENV)? (PIPE ENV)? EOL                  // NOTE: INP_TRM cannot be EMP when corresponding OUT_TRM is (MET / EMP) 

COMMENT ←   ';;' [<Valid Unicode Character>]* EOL
EOL     ←   <End of Line> / COMMENT

INP     ←   INP_TRM ( ',' INP_TRM )*
INP_TRM ←   EMP / INP_EL+
INP_EL  ←   W_ELLIP / ELLIPSS / XBOUND / SBOUND / TERM

OUT     ←   OUT_TRM  ( ',' OUT_TRM )*
OUT_TRM ←   MET / EMP / OUT_EL+
OUT_EL  ←   SYL / STRUCT / SET / SEG / REF / SBOUND                 // NOTE: 'SET' here only makes sense if it corresponds to a SET in INP

ENV     ←   ENV_SPC / ENV_SET (',' ENV_SET)*
ENV_SET ←   ':{' ENV_TRS '}:' / ENV_TRS                             // i.e. :{ ... }:
ENV_TRS ←   ENV_TRM (',' ENV_TRM)*
ENV_TRM ←   WBOUND? ENV_ELS? '_'+ ENV_ELS? WBOUND?
ENV_ELS ←   ( XBOUND / SBOUND / W_ELLIP / ELLIPSS / OPT / TERM )+
ENV_SPC ←   '_' ',' ENV_ELS                                         // e.g. _,# ==> #_ , _#

TERM    ←   SYL / STRUCT / SET / SEG / REF
SYL     ←   '%' (':' PARAMS)? REF_ASN?
STRUCT  ←   '<' SYL_TRM+ '>' (':' PARAMS)? REF_ASN?
SYL_TRM ←   SEG / W_ELLIP / ELLIPSS / REF / SET / OPT               // NOTE: Boundaries and Syllables inside a struct are runtime invalid
SET     ←   '{' SET_TRM (',' SET_TRM)* '}'                          // NOTE: At the moment, we can't have multi-segment sets i.e. "{nd}" is not allowed 
SET_TRM ←   REF / SEG / BOUND / SYL                                 // NOTE: WBOUND not valid in input/output
OPT     ←   '(' OPT_TRM+ (',' [0-9]+ (':' [1-9]+)?)? ')'
OPT_TRM ←   XBOUND / BOUND / SYL / SET / SEG / REF
SEG     ←   IPA (':' PARAMS)? / MATRIX REF_ASN?
MATRIX  ←   GROUP (':' PARAMS)? / PARAMS
REF     ←   [0-9]+ (':' PARAMS)?
REF_ASN ←   '=' [0-9]+

GROUP	←   [A-Z]
PARAMS  ←   '[' (ARG (',' ARG)*)? ']'
ARG     ←   ARG_MOD [a-zA-Z]+ / TONE
TONE    ←   [a-zA-Z]+ ':' [0-9]+
ARG_MOD ←   '+' / '-' / [α-ωA-Z] / '-'[α-ωA-Z]

EMP     ←   '*' / '∅'
MET     ←   '&'
XBOUND  ←   WBOUND WBOUND
BOUND	←   WBOUND / SBOUND
WBOUND  ←   '#'
SBOUND  ←   '$'
W_ELLIP ←   '(' ELLIPSS ')'
ELLIPSS ←   '...' / '..' / '…'
ARR     ←   (('='/'-')? '>') / '~' '>'?
PIPE    ←   '|' / '//'

IPA     ←   PRE_NAS? IPA_CHR (TIE IPA_CHR)? IPA_DIA*
PRE_NAS ←   'ᵐ' / 'ⁿ' / 'ᶯ' / 'ᶮ' / 'ᵑ' / 'ᶰ'
IPA_CHR ←   [<Unicode IPA character>]                               // NOTE: As defined in `cardinals.json`
TIE     ←   '^' / [U+0361] / [U+035C]
IPA_DIA ←   !PRE_NAS [<Unicode DIACRITIC character>]                // NOTE: As defined in `diacritics.json`
```