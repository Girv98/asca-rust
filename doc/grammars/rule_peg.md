``` peg
RULE    ←   INP ARR OUT ('/' ENV)? (PIPE ENV)? EOL          // NOTE: INP_TRM cannot be EMP when corresponding OUT_TRM is (MET / EMP) 

INP     ←   INP_TRM  ( ',' INP_TRM )*                       //
INP_TRM ←   EMP / INP_EL+                                   //
INP_EL  ←   ELLIPSS / SBOUND / TERM                         //

OUT     ←   OUT_TRM  ( ',' OUT_TRM )*                       //
OUT_TRM ←   MET / EMP / OUT_EL+                             //
OUT_EL  ←   SYL / SET / SEG / VAR / SBOUND                  // NOTE: 'SET' here only makes sense if it corresponds to a SET in INP

ENV     ←   ENV_SPC / ENV_TRM  (',' ENV_TRM)*               // e.g. _,# ==> #_ , _#
ENV_TRM ←   ('WBOUND')? ENV_ELS? '_' ENV_ELS? ('WBOUND')?   //
ENV_ELS ←   ( SBOUND / ELLIPSS / OPT / TERM )+              //
ENV_SPC ←   '_' ',' ENV_EL                                  //

TERM    ←   SYL / SET / SEG / VAR                           //
SYL     ←   '%' (':' PARAMS)? VAR_ASN?                      //
SET     ←   '{' SET_TRM (',' SET_TRM)* '}'                  // NOTE: At the moment, we can't have multi-segment sets i.e. "{nd}" is not allowed 
SET_TRM ←   SEG / BOUND / SYL                               // NOTE: WBOUND not valid in input/output
OPT     ←   '(' OPT_TRM+ (',' [0-9]+ (':' [1-9]+)?)? ')'    // NOTE: (C) === (C,1) === (C, 0:1)
OPT_TRM ←   BOUND / SYL / SET / SEG / VAR                   //
SEG     ←   IPA (':' PARAMS)? / MATRIX VAR_ASN?             //
MATRIX  ←   GROUP (':' PARAMS)? / PARAMS                    //
VAR     ←   [0-9]+ (':' PARAMS)?                            //
VAR_ASN ←   '=' [0-9]+                                      //

GROUP	←   [A-Z]                                           //
PARAMS  ←   '[' (ARG (',' ARG)*)? ']'                       //
ARG     ←   ARG_MOD [a-zA-Z]+ / TONE                        //
TONE    ←   [a-zA-Z]+ ':' [0-9]+                            //
ARG_MOD ←   '+' / '-' / [α-ωA-Z] / '-'[α-ωA-Z]              //

EMP     ←   '*' / '∅'                                       //
MET     ←   '&'                                             //
BOUND	←   WBOUND / SBOUND                                 //
WBOUND  ←   '#'                                             //
SBOUND  ←   '$'                                             //
ELLIPSS ←   '...' / '..' / …                                //
ARR     ←   ('='/'-')? '>'                                  //
PIPE    ←   '|' / '//'                                      //

IPA     ←   PRE_NAS? IPA_CHR (TIE IPA_CHR)? IPA_DIA*        //
PRE_NAS ←   'ᵐ' / 'ⁿ' / 'ᶯ' / 'ᶮ' / 'ᵑ' / 'ᶰ'                  //
IPA_CHR ←   [<Unicode IPA character>]                       // NOTE: As defined in `cardinals.json`
TIE     ←   '^' / [U+0361] / [U+035C]                       //
IPA_DIA ←   !PRE_NAS [<Unicode DIACRITIC character>]        // NOTE: As defined in `diacritics.json`
```