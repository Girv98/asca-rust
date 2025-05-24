# ASCA Documentation and User Guide

This is documentation for the core principles of defining words and sound changes with ASCA. For platform specific documentation, see:

[Web](./doc-web.md) | [Cli](./doc-cli.md)

### Contents
* [Defining Words](#defining-words)
    * [IPA Characters](#ipa-characters)
    * [Suprasegmentals](#suprasegmentals)
    * [Inbuilt Aliases](#inbuilt-aliases)
    * [(De)Romanisation](#custom-aliasing--deromanisation)
* [Defining Basic Sound Changes](#defining-basic-sound-changes)
    * [The Basics](#the-basics)
    * [Single Line Comments](#single-line-comments)
    * [Special Characters](#special-characters)
    * [Insertion and Deletion](#insertion-and-deletion-rules)
    * [Metathesis](#metathesis-rules)
    * [Condensed Rules](#condensed-rules)
    * [Special Environment](#special-environment)
    * [Syllable Structure](#syllable-structure)
* [Distinctive Features](#distinctive-features)
    * [Using Distinctive Features](#using-distinctive-features)
    * [Nodes and Subnodes](#node-and-subnode-features)
    * [Inversion](#inversion)
* [Suprasegmental Features](#suprasegmental-features)
    * [Stress](#stress-1)
    * [Length](#length-1)
    * [Tone](#tone-1)
* [Groupings](#groupings)
* [Sets](#sets)
* [Environment Sets](#environment-sets)
* [Gemination](#gemination)
* [Optional Segments](#optional-segments)
* [Alpha Notation](#alpha-notation)
    * [Nodes and Subnodes](#nodes-and-subnodes)
* [Variables](#variables)
* [Syllable Structure Matching](#syllable-structure-matching)
* [Propagation](#propagation)
    * [Faux Right-to-left Propagation](#faux-right-to-left-propagation)
    * [True Right-to-left Propagation](#true-right-to-left-propagation)
    * [Blocking](#blocking)
* [Considerations](#considerations) 

## Defining Words

### IPA Characters

ASCA recognises over 360 base IPA phones which can be modified with any of 30 diacritics. Meaning that most commonly used IPA codepoints are representable, including:
- Clicks (Velar, uvular, and uvular contour)
- Ejectives & Implosives
- Voiceless, Creaky, & Breathy Phonation
- Syllabic Consonants
- Affricates and Pre-nasalised stops
- Advanced & Retracted Tongue Root
- Labialised, Glottalised, Velarised, Palatalised, Pharyngealised (etc.) Segments

ASCA supports digraphs; where two characters are joined by a tie (`◌͡◌`or `◌͜◌`) or, if not available, a caret `^` (i.e. `d͡ʒ` can be represented as `d^ʒ`). The tie/caret is not optional, `dʒ` is considered a sequence of two segments `d` and `ʒ`.

Clicks are preceded by a velar or uvular plosive/nasal, denoting place of rear articulation, voicing, and nasality. These do not need to be joined by a tie as it is implicit (i.e. `ŋʘ` not `ŋ^ʘ` nor `ʘ`).

Doubly articulated stops, such as `ɡ͡b`, are not supported.

In the event that ASCA is unable to render a segment in IPA, `�` will be used in its place.

Unless the diacritic is inherent to the base phone (e.g. `𝼆̬`) then diacritic order does not matter. When generating the output word list, ASCA tries to adhere to [PHOIBLE conventions](https://phoible.org/conventions#ordering-of-diacritics-and-modifier-letters) where possible. Meaning that the diacritics in the output may be in a different order than was input. Additionally, if a base phoneme with a combination of diacritics is equal in value to another base phoneme (or can be composed with less diacritics), then it shall be generated as such (i.e. `ɢ̃` will become `ᶰɢ`). 

A full list of supported base phones and diacritics (with their values) can be found [here](https://bit.ly/3sHjqvA).

### Suprasegmentals

#### Syllable Boundary
Syllables are separated by `.`.
A word with no marked boundaries is considered one syllable. There are no rules regarding internal syllable structure.

#### Length
Segment length can be represented by either `ː` or `:`. A segment can be followed by multiple length markers, representing overlong segments. Alternatively, length can be represented by repetition of the segment (i.e. `si:m` can be `siim`). Identical segments that are separated by a syllable boundary are not considered one long segment. If a long segment falls at the end of a syllable, `;` can be used as shorthand to also close the syllable (i.e. `si:.tiŋ` can be `si;tiŋ`).

#### Stress
Primary stress can be represented by either `ˈ` or `'` and secondary stress by either `ˌ` or `,`. These are placed at the start of the syllable. The boundary marker can be omitted if followed by a stressed syllable (i.e. `ə'gəʊ` instead of `ə.'gəʊ`). Note that ejective consonants cannot be marked with a `'` as this will be interpreted as stress. `ʼ` or `^'` must be used instead `i.e. /pʼ/ or /p^'/`.

#### Tone
ASCA does not currently support tone diacritics or tone letters. Tone instead is represented by numbers following the syllable. As of yet, there are no rules regarding the meaning or syntax of these numbers; However, for demonstration we will follow the [Chinese convention](https://en.wikipedia.org/wiki/Tone_letter#Numerical_values), using numbers from 1 (lowest pitch) to 5 (highest pitch). As with stress, either a syllable or a segment can be matched or modified with tone.

The tones of Mandarin would be represented in this system as:
```
mā => `ma55`
má => `ma35`
mǎ => `ma214`
mà => `ma51`
ma => `ma0` or just `ma`
```
Tone is placed at the end of a syllable and therefore automatically closes it. However, you may still mark the boundary for clarity.
```
pu35.jɑʊ̯51.tan55.ɕin55 == pu35jɑʊ̯51tan55ɕin55
```

### Inbuilt Aliases
Some common IPA characters that may be annoying to type without an IPA keyboard have aliases:
```
g => ɡ
? => ʔ
! => ǃ
ǝ => ə
φ => ɸ
^j => ʲ
^w => ʷ
^v => ᶹ
^g => ˠ
^' => ʼ
^? => ˀ
^h => ʰ
^ɦ => ʱ
^m => ᵐ
^n => ⁿ
^ŋ => ᵑ
^N => ᶰ

(The following cannot be used inside a rule)

S => ʃ
Z => ʒ
C => ɕ
G => ɢ
N => ɴ
B => ʙ
R => ʀ
X => χ (voiceless uvular fricative)
H => ʜ
A => ɐ
E => ɛ
I => ɪ
O => ɔ
U => ʊ
Y => ʏ
```
Aliases are rendered as their target IPA characters in the output.


A few common americanist characters can also be used:
```
ł => ɬ
ñ => ɲ
¢ => t͡s
ƛ => t͡ɬ
λ => d͡ɮ
```
***Note: Inbuilt americanist characters will be removed in v0.8.0 in favour of custom aliases, see below.***

Unlike with regular aliases, if a input word contains americanist characters, the output will be be rendered with these characters.

### Custom Aliasing / (De)Romanisation

ASCA allows for a **subset** of the [regular rule syntax](#defining-basic-sound-changes) to be used to define custom aliases and general romanisation/deromanisation.
These mappings are applied before the inbuilt aliases defined above. Segments can be selected with modifiers such as stress and tone. 
On the web version, these rules are defined through the **alias** button; For cli, see the [cli documentation](./doc-cli.md).

#### Unicode Escapes
ASCA allows for unicode character escapes to be used in replacement strings. 
There are three types, codepoint escapes, named escapes and character escapes:

##### Codepoint Escapes
Codepoint Escapes take the form of `\u{....}` where `....` are hex digits `0-F`.
This can be used to render any valid unicode scalar value.

```
θ, ð => \u{00FE}
$ => *

'θorn    (becomes) þorn
'eor.ðe  (becomes) eorþe
```

##### Named Escapes
Named escapes take to form of `@{....}`. They allow for common diacritics to be used without needing to memorise or look-up codepoints. Currently supported named escapes are:
```
@{Space}        (U+0020 ASCII Space)
@{Grave}        (U+0300 Combining Grave Accent)
@{Acute}        (U+0301 Combining Acute Accent)
@{Circumflex}   (U+0302 Combining Circumflex Accent)
@{Tilde}        (U+0303 Combining Tilde)
@{Macron}       (U+0304 Combining Macron)
@{OverLine}     (U+0305 Combining Overline)
@{Breve}        (U+0306 Combining Breve)
@{OverDot}      (U+0307 Combining Dot Below)
@{Umlaut}       (U+0308 Combining Diaeresis)
@{OverHook}     (U+0309 Combining Hook Above)
@{OverRing}     (U+030A Combining Ring Above)
@{DoubleAcute}  (U+030B Combining Double Acute Accent)
@{Caron}        (U+030C Combining Caron)
@{DoubleGrave}  (U+030F Combining Combining Double Grave Accent)
@{InvBreve}     (U+0311 Combining Inverted Breve)
@{Horn}         (U+031B Combining Horn)
@{UnderDot}     (U+0323 Combining Dot Below)
@{UnderUmlaut}  (U+0324 Combining Diaeresis Below)
@{UnderRing}    (U+0325 Combining Ring Below)
@{UnderComma}   (U+0326 Combining Comma Below)
@{Cedilla}      (U+0327 Combining Cedilla)
@{Ogonek}       (U+0328 Combining Ogonek)
```
Capitalisation and spaces have no effect i.e. `@{OverDot}` is equal to `@{over dot}`. 
Many also have alternatives, for examples as `@{OverX}` can be `@{XAbove}` or just `@{X}`.

It is important to note that at the aliasing stage asca does not decompose any unicode characters, so `ą (U+0105)` will not match `a @{ogonek}` which is the sequence `U+0061 U+0328` (This may change).

More characters can be added on request.

##### Character Escapes
Characters that might otherwise cause a syntax error can be used by being preceded with `\`.

```
\\ (becomes) \
\@ (becomes) @
\$ (becomes) $
\∅ (becomes) ∅
\* (becomes) *
\> (becomes) >
\= (becomes) =
\+ (becomes) +
\- (becomes) -
\, (becomes) ,
```

#### Romanisation
A romanisation rule allows you to manipulate the output from ipa into another desired form. 
The left-hand side of the arrow contains a list of the matching ipa segments or features. Additionally, `$` can be used to target syllable breaks. 
Right of the arrow contains a list of replacement strings. A star `*` or empty set symbol `∅` can be used to specify that the matching element should be removed.

Some examples:
```
a:[+str, +long], a:[+long] > â, ā
ʃ:[+long] > ssh
$ > *

'ʃ:a:.da: (becomes) sshâdā
```
```
xan:[tone: 55] => 憨
xan:[tone: 51] => 汉
  y:[tone:214] => 语
$ > *

han51.y214 (becomes) 汉语
```
```
ka, ta, na => カ, タ, ナ
$ => *

ka.ta.ka.na (becomes) カタカナ
```

#### Deromanisation
A deromanisation rule allows you to modify your input into ipa into a form ASCA can recognise.
The syntax here is the reverse of a romanisation rule, with replacement strings on the left and the ipa segments on the right of the arrow.
Deromanisation rules are currently less powerful than romanisation rules as they do not allow for syllable breaks to be specified or inserted.
```
â, ā =>  a:[+str, +long], a:[+long] 
ssh > ʃ:[+long]

sshâ.dā (becomes) 'ʃ:a:.da:
```

```
カ => ka
タ => ta
ナ => na

カ.タ.カ.ナ (becomes) ka.ta.ka.na
```

```
汉 > xan:[tone: 51] 
语 >   y:[tone:214]

汉.语 (becomes) han51.y214
```

#### Plus Operator
The plus operator `+` can be placed at the beginning of a replacement string. 
This indicates that the string should be **added** to the normal rendering of the segment instead of replacing it.

This can be useful for generalising diacritics to certain features:
```
V:[+str, +long] => +@{circumflex}       ( equiv. to:    a:[+str, +long], e:[+str, +long], ... => â, ê, ... )
V:[-str, +long] => +@{macron}           ( equiv. to:    a:[-str, +long], e:[-str, +long], ... => ā, ē, ... )
V:[+str, -long] => +@{acute}            ( equiv. to:    a:[+str, -long], e:[+str, -long], ... => á, é, ... )
V:[+nasal]      => +@{ogonek}           ( equiv. to:    a:[+nasal], e:[+nasal], ...           => ą, ę, ... )
```

When used in deromanisation rules, this allows for payload to be added to the previously calculated segment.
```
+@{ogonek} => [+nasal]

as.tą (becomes) /as.tã/

```
It should be noted that this same rule without '+' errors as incomplete matrices cannot be converted to a canonical segment.

#### Future 
Some things to look out for.

Environments:
```
s > Σ / #_
s > ς / _#
s > σ
```
Inserting syllable boundaries:
```
* > $ / V_C         (katakana) > ka.ta.ka.na
```


## Defining Basic Sound Changes

### The Basics
ASCA tries to stick to commonly used [notation](https://en.wikipedia.org/wiki/Phonological_rule) wherever possible. Though, it may differ from other sound change appliers.
In general, a rule is made of 4 parts:
```
input     -> the content to be transformed
output    -> the result of the transformation
context   -> the specificity of the surrounding environment
exception -> the exclusivity of the surrounding environment
```
These blocks are divided by specific separators so that a given rule looks like this:
```
input (=/-)> output / context (| or //) exception

e.g. ei > ie | c_ (/ei/ changes to /ie/, except when directly after /c/)
```

The 'arrow' can be `=>`, `->`, or just `>`. The exception block can be introduced by either `|` or `//`.

An environment can only contain one underline `_` or a series of joined underlines. An empty environment can be omitted:
```
a > e           (/saj/ > /sej/)
a > e / _       (this is equivalent to the above)
a > e / ___     (this is valid)
a > e / _ _     (this is invalid)
```

### Single Line Comments
ASCA supports single line comments delimited by two semi-colons `;;`. These comments can appear at the end of a line or take up the full line.

``` wasm
;; This is a comment!
a > e ;; This is also a comment!
```

### Special Characters

`%` represents a whole syllable.

`$` represents a syllable boundary.

`#` represents a word boundary.

Word boundaries `#` may only be used in environments, and must only be used once on either periphery.

```
 a > e / #_#    ;; valid
 a > e / _s#    ;; valid, /a.has/ > /a.hes/

 a > e / _##    ;; invalid
 a > e / _#s    ;; invalid
```

### Insertion and Deletion Rules

Unlike [SCA²](https://www.zompist.com/sca2.html), the input and output cannot be omitted. Insertion and deletion are marked by the `*` operator.
The input or output must contain *only* this operator to be valid.

```
e > * / #_      ;; Apheresis: /e/ elides at the beginning of a word
e > * / _#      ;; Apocope: /e/ elides at the end of a word
* > e / #_      ;; Prothesis: /e/ is inserted at the beginning of a word
* > e / _#      ;; Paragoge: /e/ is inserted at the end of a word
```
You may use the empty set character `∅` instead:

```
e > ∅ / #_
```

### Metathesis Rules
The ampersand operator `&` states that the order of the matched input is reversed; Such that, for example, a sequence of matched segments `ABC` becomes `CBA`. The operator can be used to flip an arbitrary number of segments or syllables.
Like deletion, the output of a metathesis rule must contain *only* `&` and nothing else. 

```
Old English R Metathesis (hros => hors)
[+rhotic]V > & / _s
```

An ellipsis `…` or double `..` or triple dot `...` can be used to implement long-range metathesis:

```
Spanish Hyperthesis (Old Spanish parabla => Spanish palabra)
r...l > &
```

Note that the ellipsis must match at least one segment, so a word such as `ar.la` would not change under the above rule. 

<!-- TODO: We can achieve both long-range and short-range metathesis by using `([],0)` (see [optionals](#optional-segments)) in place of the ellipsis. This denotes matching 'zero or more' segments.

```
r ([],0) l > &

parabla => palabra
arla > alra


``` -->

### Condensed Rules
Multiple rules can be condensed into one line. This can be useful when you have two or more sequential rules that share identical inputs, outputs, or environments.

For Example:
```
e > * / #_
e > * / _#
```
can be condensed into:
```
e > * / #_, _#
```
It is important to remember that the rules are still applied sequentially and not at the same time (see [environment sets](#environment-sets) for this).

### Special Environment

You may often have condensed rules like the one above where the same environs are being matched both before and after the input. As this can be common, there is a shorthand form of `_,` followed by the environment elements in question.

```
e > * / #_, _#
(becomes)
e > * / _,#
```
The before case always comes first.
Any elements past the comma are mirrored such that:
```
_,ABC => ABC_ , _CBA
```

### Syllable Structure
ASCA does not enforce 'legal' syllables and it is up to you to maintain syllable boundaries.
This can be done by metathesising, inserting, and deleting `$`.

For example, imagine a input word of `'si.tu`. If we apply the rule `V > * / C_#`, we end up with a floating consonant `'si.t`.

This can be repaired in a few ways, including: 
```
$C > & / _# (the consonant is moved into the preceding syllable, with the now empty second syllable being deleted)
or
$ > * / _C# (the two syllables are merged by deleting the boundary between them)
```

## Distinctive Features
ASCA allows for 26 segmental features.  
A full table of segments and there values can be found [here](https://bit.ly/3sHjqvA).

```
┌────────┬─────────┬─────────┬─────────────────────────────┬────────────────────────────┐
│  Node  │ SubNode │ Feature │              +              │             -              │
├────────┼─────────┴─────────┼─────────────────────────────┼────────────────────────────┤
│        │    consonantal    │ obstruents, nasals, liquids │ vowels, glides, laryngeals │
│  ROOT  │      sonorant     │      vowels, sonorants      │         obstruents         │
│        │      syllabic     │ vowels, syllabic consonants │     glides, consonants     │
├────────┼───────────────────┼─────────────────────────────┼────────────────────────────┤
│        │    continuant     │  fricatives, approximants,  │    Plosives, affricates,   │
│        │                   │       vowels, trills        │        nasals, flaps       │
│        │    approximant    │   vowels, glides, liquids   │     nasals, obstruents     │
│        │      lateral      │ l-like and lateralised segs │             -              │
│ MANNER │       nasal       │  nasals, nasalised vowels,  │ oral consonants and vowels │
│        │                   │      prenasalised stops     │                            │
│        │  delayed release  │     affricate consonants    │       Plosives, etc.       │
│        │     strident      │    f, v, s, z, ʃ, ʒ etc.    │   ɸ, β, θ, ð, ç, ʝ, etc.   │
│        │      rhotic       │    r-like trills & flaps    │             -              │
│        │                   │ rhoticised vowels and cons. │             -              │
│        │       click       │       click consonants      │             _              │
├────────┼───────────────────┼─────────────────────────────┼────────────────────────────┤
│        │       voice       │       voiced segments       │     voiceless segments     │
│ LARYNG │   spread glottis  │   aspirates, breathy voice  │             -              │
│        │   const glottis   │    ejectives, implosives    │             -              │
│        │                   │         creaky voice        │             -              │
├────────┼─────────┬─────────┼─────────────────────────────┼────────────────────────────┤
│        │ LABIAL  │ labdent │       ɱ, ʋ, f, v, etc.      │      ɸ, β, p, b, etc.      │
│        │         │  round  │       rounded segments      │      p, b, f, v, etc.      │
│        ├─────────┼─────────┼─────────────────────────────┼────────────────────────────┤
│        │ CORONAL │ anterior│      dentals, alveolars     │ post-palatals, retroflexes │
│        │         │ distrib │    palatals, post-palatals  │   alveolars, retroflexes   │
│        ├─────────┼─────────┼─────────────────────────────┼────────────────────────────┤
│        │         │  front  │    palatals, front vowels   │             -              │
│ PLACE  │         │  back   │ velars/uluvars, back vowels │             -              │
│        │ DORSAL  │  high   │     velars, high vowels     │             -              │
│        │         │   low   │   pharyngeals, low vowels   │             -              │
│        │         │  tense  │     tense vowels & cons.    │         lax vowels         │
│        │         │ reduced │    schwa, reduced vowels    │             -              │
│        ├─────────┼─────────┼─────────────────────────────┼────────────────────────────┤
│        │ PHARYNG │   atr   │      advanced root segs     │                            │
│        │         │   rtr   │         pharyngeals         │        epiglottals         │
└────────┴─────────┴─────────┴─────────────────────────────┴────────────────────────────┘
```

```
┌────────┬─────────────┬─────────────┬─────────────┬─────────────┐
│        │ -s.g. -c.g. │ -s.g. +c.g. │ +s.g. -c.g. │ +s.g. +c.g. │
├────────┼─────────────┼─────────────┼─────────────┼─────────────┤
│        │             │ ejectives,  │             │             │
│ -voice │  voiceless  │ glottalised │  aspirated  │     n/a     │
│        │             │  sonorants  │             │             │
├────────┼─────────────┼─────────────┼─────────────┼─────────────┤
│        │             │ implosives, │             │             │
│ +voice │   voiced    │   creaky    │   breathy   │     n/a     │
│        │             │  sonorants  │             │             │
└────────┴─────────────┴─────────────┴─────────────┴─────────────┘
```

### Using Distinctive Features

Distinctive features are defined between square brackets `e.g. [+cons]`. These are called matrices. A matrix can have multiple features, each separated by a comma `e.g. [+cons, -syll]`. 
Whitespace is not important, meaning `[+del.rel.]` is identical to `[ + d e l . r e l . ]`. Many features also have shorthands `e.g. [bk, hi, lo, dr] = [back, high, low, del.rel.]`.

A matrix can be used standalone to represent a segment, or can be used to modify a segment by joining them with a colon `:`.
```
[-cons, +son, +syll] > [+rtr] / q_  (vowels pharyngealise following /q/)
a:[-stress, -long] > ə              (unstressed short /a/ becomes schwa) 

note a[-stress, -long] would match two segments: /a/ followed by a short, unstressed segment 
```

```
Rule Example: Grimm's Law

Simple IPA:
p, t, k, kʷ > ɸ, θ, x, xʷ 
b, d, g, gʷ > p, t, k, kʷ
bʱ, dʱ, gʱ, gʷʱ > b, d, g, gʷ

Using Distinctive Features:
[+cons, -son, -cont, -voice] > [+cont]
[+cons, -son, -cont, +voice, -sg] > [-voice]
[+cons, +voice, +sg] > [-sg]
```
An empty matrix `[]` can be used to match any one segment (similar to a Regex wildcard).

### Node and Subnode features
#### Matching a subnode
SubNodes can be used to match segments by place of articulation.
```
[+labial]  -> rounded, labial, and labiodental segments
[+coronal] -> dental, alveolar, retroflex, palatal (etc.) segments
[+dorsal]  -> vowels & velar, uvular, palatal segments
[+phargyn] -> epiglottal/pharyngeal segments, and atr/rtr
[+place]   -> matches all non glottal segments
[-place]   -> glottal segments; h, ɦ, ʔ, etc. 
```
The major nodes Root, Manner, and Largyngeal cannot be positive or negative. See [alpha notation](#alpha-notation) for their use cases.

#### Applying a subnode
In the output block, these features can be used to add or remove a place of articulation:
```
Rule Example: Plosive Debuccalisation

[+cons, -son, -voi] > [-cons, +c.g., -place] ( {p,t,k} > ʔ )
```
When adding a node, all features within the node are set to `-`.

Again; Root, Manner, and Largyngeal cannot be used in this way. Place also cannot be `+place` in this case.

## Suprasegmental Features

### Stress
ASCA allows for a 3-way distinction between primary, secondary, and unstressed syllables.

```
┌──────────────────┬────────────────────────────────┐
│    Stress Type   │            Modifier            │
├──────────────────┼──────────────┬─────────────────┤
│    Unstressed    │  [- stress]  │                 │
├──────────────────┼──────────────┤ [- sec. stress] │
│  Primary Stress  │              │                 │
├──────────────────┤  [+ stress]  ├─────────────────┤
│ Secondary Stress │              │ [+ sec. stress] │
└──────────────────┴──────────────┴─────────────────┘
```

For example, if one wanted to match for syllables with primary stress but exclude secondary stress, `[+stress, -sec. stress]`.

Stress can be used on a whole syllable or on a segment. This allows you to change the stress of a syllable based on segments within it and vice-versa.

```
Rule Example: Latin Stress

% > [+str] / #_#            ;; If there is only one syllable, it is stressed
V:[+long] > [+str] / _%#    ;; A penult syll ending with a long vowel becomes stressed
V > [+str] / _C%#           ;; A penult syll ending with a consonant or glide becomes stressed
% > [+str] / _%:[-str]%#    ;; If the penult is unstressed, the antepenult becomes stressed

(Rules 2 and 3 could be condensed into one by matching to the consonant instead of the vowel in rule 3)
V > [+str] / _C%# (becomes) C > [+str] / _%#
(therefore)
V:[+long], C > [+str] / _%# ;; A penult syll ending with either a long vowel or a consonant/glide becomes stressed)
```

```
Rule Example: Germanic Inital Stress Shift

%:[+stress] > [-stress]     ;; All stressed syllables become unstressed
% > [+stress] / #_          ;; The syllable at the beginning of the word becomes stressed
```

### Length
Length also has a 3-way distinction; allowing for the overlong vowels of languages like Estonian and Proto-Germanic.

```
┌──────────────┬────────────────────────────────┐
│    Length    │            Modifier            │
├──────────────┼──────────────┬─────────────────┤
│     Short    │   [- long]   │                 │
├──────────────┼──────────────┤   [-overlong]   │
│     Long     │   [+ long]   │                 │
├──────────────┼──────────────┴─────────────────┤
│   Overlong   │          [+ overlong]          │
└──────────────┴────────────────────────────────┘
```

```
Rule Example: Compensatory Lengthening

V > [+long] / _C#       ;; A vowel becomes long before a consonant at the end of a word
C > * / V:[+long]_#     ;; A consonant at the end of a word before a long vowel elides

(or by using variable substitution)

V=1 C > 1:[+long] / _#
```

### Tone
Tone has a unique syntax within matrices. That is, `[tone: X]`, where `X` is the tone numbers.  
As of yet, tone cannot be used with alpha notation; nor can it be 'negated'.

```
Rule Example: Mandarin 3rd Tone Sandhi

%:[tone: 214] > [tone:35] / _%[tone: 214] 
(3rd tone becomes 2nd tone before another 3rd tone)
```

```
Rule Example: Middle Chinese Tonogenesis

% > [tone: 33]                       (平 and 入)
V > [tone: 35], [tone: 51] / _ʔ, _s  (上 then 去)
ʔ , s > * / _$                       (Phonemicisation)
```

## Groupings

Groupings can be used as shorthand to match often used parts of speech.
```
C -> Consonants (obstruents and sonorants)          (equiv. to [-syll])
O -> Obstruents (plosives, fricatives, affricates)  (equiv. to [+cons, -son, -syll])
S -> Sonorants  (nasals and liquids)                (equiv. to [+cons, +son, -syll])
P -> Plosives                                       (equiv, to [+cons, -son, -syll, -delrel, -cont])
F -> Fricatives                                     (equiv, to [+cons, -son, -syll, -approx, +cont])
L -> Liquids                                        (equiv. to [+cons, +son, -syll, +approx])
N -> Nasals                                         (equiv. to [+cons, +son, -syll, -approx, +nasal])
G -> Glides                                         (equiv. to [-cons, +son, -syll])
V -> Vowels                                         (equiv. to [-cons, +son, +syll])
```
Note that purely glottalic consonants such as `/h/ and /ʔ/` are considered `[-cons, -son, -syll]` and are therefore not captured by any grouping other than `C`. 

## Sets
Sets are defined between curly brackets `{}` and can contain IPA, Groups, Matrices, Syllables, or Boundaries.  
Currently, sets cannot contain sequences (i.e. cannot have `{nd, NC, %%}`).

```
p, t, k > b, d, g       (3 Rules)   
{p, t, k} > {b, d, g}   (1 Rule)
```
A set in the output, if matched to a set in the input, must contain the same number of segments. 
```
{p, t} > {b, d, g}      (ERROR)
```
A set in the input or output cannot contain word boundaries.

## Environment Sets

It can be necessary to check for multiple environmental matches in a single pass.
This is especially true of exception clauses and, in some cases, propagation. 
For example, in the English 'Great Vowel Shift', /uː/ does not shift if it is followed by a labial consonant, or preceded by /j/.
Representing this normally would be rather tricky as, in this case, consecutive environments would override each other:

```
u:[+long] => əw | _C:[+lab], j_

duːt => dəwt, suːp => səwp, juːθ => jəwθ (does not work!)
```

This is fixed by placing the two environments inside an environment set, which is delimited with `:{` and `}:` (Note: this is not the same as a regular set, which is delimited by `{` and `}`, though regular sets are valid inside an environment set).

```
u:[+long] => əw | :{ _C:[+lab], j_ }:

(/u:/ becomes /əw/ everywhere except after /j/ or before a labial consonant)

duːt => dəwt (doubt)
suːp => suːp (soup)
juːθ => juːθ (youth)
```
These sets can be used as part of condensed rules, and are valid in substitution, deletion, and metathesis rules.
They are currently not allowed in insertion rules, however this will change in further updates.

## Gemination
Syllable final consonant gemination is as simple as making a vowel long.

```
C > [+long] / V:[-long]_#
(A consonant is geminated at the end of a word, before a short vowel)
```

To geminate across a syllable boundary, we can do one of a few things (not exhaustive): 

```
Insertion with a Variable (see below)

* > 1 / V:[-long, +str] _ $ C=1 ('lu.ka => 'luk.ka, 'lu:.ka => 'lu:.ka)
```

```
Insertion with Structure Matching (see below)

* > 1 / ⟨..V:[-long]⟩:[+str] _ ⟨C=1...⟩ ('lu.ka => 'luk.ka, 'lu:.ka => 'lu:.ka)
```


## Optional Segments
Optional Segments are declared as `(S, M:N)` where: 
```
S = the segment(s) to be repeated
M = the minimum number of iterations (optional, default = 0)
N = the maximum number of iterations (inclusive). N must be greater than or equal to M.
```
For example, ```(C,5)_```  matches up to 5 consonants preceding the target. This will lazily target environments of `_`, `C_`, `CC_`, `CCC_`, `CCCC_`, and `CCCCC_`.

`(C,3:5)` matches `CCC_`, `CCCC_`, and `CCCCC_`.

`(C,0)_` matches any number of consonants preceding the target. This is equivalent in use to regex’s Lazy-Zero-Or-More operator (*?)

`(C)_` matches zero or one consonant preceding the target. This is the same as `(C,1)_` or `(C,0:1)_`

`([])_` matches zero or one of *any* segment preceding the target. This is equal to regex’s Zero-Or-One operator with a wildcard (.?)

`([],0)_` matches zero or more of *any* segment preceding the target. This is equal to regex’s Lazy-Zero-Or-More operator with a wildcard (.*?)

## Alpha Notation

Take these two rules:
```
[+son] > [-nasal] / [-nasal]_
[+son] > [+nasal] / [+nasal]_
``` 
Both are identical, except both `nasal` features are positive in one and negative in the other. These rules will also be applied sequentially.
We can replace these with a single rule, which is only applied once, by replacing the +/- with a greek character `α..ω`. If greek characters are not available, latin capitals `A..Z` can be used instead.

```
Rule Example: Malay Nasalisation

[+son] > [α nasal] / [α nasal]_
```
```
Rule Example: Turkish Suffix Vowel Harmony

V:[+hi] > [αbk, βfr, γrnd] / V:[αbk, βfr, γrnd] (C,0) _ (C) #
```

Alphas are processed first in the input, then the context, and lastly the output. Alphas are done left to right within each block. 
Any alpha in the output must be prior set in either the input or context.

### Nodes and Subnodes

A node alpha carries all the features within it. This can be very useful for assimilation rules.

```
Rule Example: Nasal Assimilation

[+cons, +nasal] > [α PLACE] / _[+cons, αPLACE] 
(A nasal consonant takes the place of a following consonant i.e. [nk] > [ŋk])
```
When an alpha first assigned to a node is used on a binary feature, it is coerced positive or negative. The place node is positive when the segment has any place subnode.

```
Random Example: 

P:[α DOR] > [α round]
(Any dorsal plosive becomes +round, any non-dorsal plosive becomes -round)
```

### Inversion
Imagine we have two debuccalisation rules, one for plosives and one for fricatives
```
O:[-voi, -cont] > [-cons, -c.g., -place] / _#           (pat > paʔ)
O:[-voi, +cont] > [-cons, +s.g., -place, -strid] / _#   (pas > pah)
```
It would be nice if we were able to join them into one rule. To accomplish this, we can use inversion:
```
O:[-voi, Acont] > [-cons, As.g., -Ac.g., -place, -strid] / _#
(pat, pas > paʔ, pah)
```
The above means that when matching an obstruent that is `[-cont]` the output becomes `[-s.g., +c.g.]`, while when the obstruent is `[+cont]`, the output is `[+s.g., -c.g.]`

This can be used with nodes for conditional clustering:
```
ə$ > * / P:[-nas, αPLACE]_N:[-αPLACE]
(pə.no > pno, pə.mo > pə.mo)
```
In the rule above, plosives and nasals cluster only if they are of a different place of articulation.

## Variables
Variables allow us to invoke the value of a previously matched element. Variables are declared by using the `=` operator, followed by a number. This number can then be used later in the rule to invoke the variable.
Currently; matrices, groups, and syllables can be assigned to a variable.

Using variables, we can implement metathesis without need of the `&` operator.
```
Old English R metathesis (hros > hors)
[+rho]=1 V=2 > 2 1 / _s
```

It can also be used to define a simple haplology rule.
```
%=1 > * / 1_ (A syllable is deleted if preceded by an identical syllable)
```
Despite the name, variables cannot be reassigned. However, they can be modified with a feature matrix as if they were a segment or syllable.

## Syllable Structure Matching

Sometimes it can be useful to match a syllable based on the segments within. We can do this by using a Structure. 

Structures are defined between angle brackets `⟨ ⟩` or less-than/greater-than signs `< >`. They can contain segments, matrices, variables, or ellipses.
Ellipses are useful for only matching a certain part of the syllable, such as the onset or coda.
```
⟨..P:[-voi]⟩ => [tone: 35]
(A closed syllable ending with a voiceless plosive gains rising tone)
```

```
Example: Latin Stress Rule using Structures

% > [+str] / #_#                (If there is only one syllable, it is stressed)
⟨...V[+long]⟩ > [+stress] / _%# (A penult syllable ending with a long vowel becomes stressed)
⟨...VC⟩ > [+stress] / _%#       (A penult syllable ending with a consonant becomes stressed)
% > [+stress] / _ %:[-str]%#    (If the penult is unstressed, the antepenult becomes stressed)

(Like the previous Latin stress example, rules 2 and 3 can be condensed, but slightly differently)

⟨...V[+long]⟩, ⟨...VC⟩ > [+stress] / _%#
```

Structures can also be used to insert whole syllables:
```
Example: Expletive infixation

* > ⟨blʉw⟩:[+sec.stress] ⟨mɪn⟩ / %_%:[+stress] (absolutely => abso-bloomin'-lutely)
```

```
Example: Conditional Reduplication

* > 1:[-stress] / <CV>:[+stress]=1 _ (A stressed CV syllable is reduplicated)

/'to.ka/ => /'to.to.ka/, /'ton.ka/ => /'ton.ka/
```

This is also useful for inserting copy vowels at the beginning of a word:
```
Example: Word Initial Copy Vowel Insertion

* > <1> / #_CV=1

/'de.no/ => /e'de.no/
```

## Propagation 
As ASCA changes all matching environments in a word sequentially, left-to-right harmonies naturally propagate.

```
Example: Left-to-Right Vowel Backness Harmony

V > [α front, β back] > V:[α front, β back] (C) _ (Vowels assimilate in backness to that of the preceding vowel) 

/ki.to.le.nu/ becomes /ki.tø.le.ny/, not /ki.tø.lɤ.ny/
```

### Faux Right-to-left Propagation

To achieve right to left harmonies in a "left-to-right" context, we can use a fixed harmonic trigger, which in this case is the last vowel in the word. Like with [hyperthesis](#metathesis-rules), we can place an `...` in the environment between `_` and the trigger to denote "skipping" the inbetween segments. 

```
V > [α front, β back] / _CV:[α front, β back]
/ki.to.le.nu/ becomes /kɯ.tø.lɤ.nu/, no propagation

V > [α front, β back] / _...V:[α front, β back]#
/ki.to.le.nu/ becomes /kɯ.to.lɤ.nu/, as expected
```

This works for the above example, where there is at least one non-matching segment between the trigger and the last matching segment. However, if the /h/ were not present, the /e/ would not assimilate. This is because `...` matches *at least* one segment. Using the special zero-or-more [optional](#optional-segments) `([],0)` in its place, we can match in the case of zero intermediate segments as well.

```
V > [α front, β back] / _...V:[α front, β back]#
/ki.to.leu/ becomes /kɯ.to.leu/

V > [α front, β back] / _ ([],0) V:[α front, β back]#
/ki.to.leu/ becomes /kɯ.to.lɤu/
```

### True Right-to-left Propagation

The above will work in many cases; However, to have true right-to-left harmony we can use the tilde operator `~` or `~>`. This is used in the place of the normal arrow between the input and output and denotes to asca that the rule should be inverted and applied from the end of the word to the beginning.

We can now implement the previous example more simply as just a mirror image of its left-to-right version:
```
Previous Example without Anchoring:

V ~ [α front, β back] / _ (C) V:[α front, β back]

/ki.to.leu/ becomes /kɯ.to.lɤu/
/ki.to.le.nu/ becomes /kɯ.to.lɤ.nu/
```

``` 
Rule Example: Secondary Stressing

% > [+str] / #_#, _%#           ;; The penultimate (or ultimate if none) syllable is stressed.
% ~ [+sec.str] / _%%:[+str]     ;; Every other syllable before another stressed syllable has secondary stress.

/sa.me.ka.se.ne.ta.ni.lo.ti.ne/ becomes /ˌsa.meˌka.seˌne.taˌni.loˈti.ne/
Without '~', only the last would match, becoming /sa.me.ka.se.ne.taˌni.loˈti.ne/
```

### Blocking

We can achieve blocking with an exception clause. For this example, plosives will block the spread such that the /t/ will block the first vowel /i/ from assimilating:

```
Faux Right-to-Left:

V > [α front, β back] / _ ([],0) V:[α front, β back]# | _ ([],0) P ([],0) V:[α front, β back]#

/ki.to.le.nu/ becomes /ki.to.lɤ.nu/
```

```
True Right-to-Left:

V ~ [α front, β back] / _ ([]) V:[α front, β back] | _ P

/ki.to.leu/ becomes /ki.to.lɤu/
/ki.to.le.nu/ becomes /ki.to.lɤ.nu/
```

Blocking can also be achieved in non-explicit ways:

```
Example: Regressive Nasal Vowel-Consonant Harmony that is blocked by obstruents and is transparent through sonorants

V > [+nasal] / _ ([+son],0) [+nasal]    ;; Faux
V ~ [+nasal] / _ ([+son]) [+nasal]      ;; True

/amakan/ becomes /ãmakãn/
/palanawasan/ becomes /pãlãnawasãn/
```

## Considerations

### Syllable Stress
Currently, when a syllable is inserted to the beginning of a word, the added syllable steals the stress/tone of the previously initial syllable.
This is because the current implementation cannot differentiate between it and the scenario of adding a syllable to the end, or middle, of a word. 

Take this copy vowel insertion rule: 
```
* > 1$ / #_CV=1
('de.no > 'e.de.no NOT e'de.no)
```
To fix this, we can either use a syllable instead of a boundary and alpha notation to 'save' the stress:
```
* > 1:[-str]%:[Astr] / #_CV:[Astr]=1
(e'de.no as expected)
```
Or insert with a [structure](#syllable-structure-matching)
```
* > <1> / #_CV=1
(e'de.no as expected)
```
### Substituting Long IPA

When doing IPA substitution, you may come across behaviour such as this
```
a > e

hat  > het (expected, current behaviour)
ha:t > het (unexpected, current behaviour)
```
This doesn't happen with matrices.
```
a > [+fr, -lo, +tns]

hat  > het  (expected, current behaviour)
ha:t > he:t (expected, current behaviour)
```
This is a consequence of how we currently iterate through a word, and what we consider a single segment. 
Whether/How this behaviour will change in future releases is being debated. 
For now, it is best to think of any ipa character in the output as being inherently `[-long]`. 

The 'fix' for this is to use alpha notation:

```
a:[Along] > e:[Along]   ([Along, Boverlong] if you have overlong vowels)
hat  > het
ha:t > he:t
```