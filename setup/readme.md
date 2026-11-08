
# SegDef

Simple-ish python script to convert a human readable/editable segment format into compact JSON

## Format

Each line defines a new segment as follows:
```
FEATURES = GRAPHEMES
```
Single line comments are delimited by `#` These comments can appear at the end of a line or take up the full line.

### Features

`FEATURES` is a representation of the segment's distinctive feature matrix, with each node
separated by a pipe `|`. Spaces in this section are ignored:
```
ROOT | MANNER | LARYNGEAL | LABIAL | CORONAL | DORSAL | PHARYNGEAL
```
Each node is then represented by a series of plusses and minuses of a set length based on the node:
```
Length of each node:

ROOT       = 3 [cons, son, syll]
MANNER     = 9 [cont, approx, lateral, nasal, delrel, strident, rhotic, click, double]     # Double coming next release
LARYNGEAL  = 3 [voice, sg, cg]

LABIAL     = 2 [labiodental, round]
CORONAL    = 2 [anterior, distributed]
DORSAL     = 6 [front, back, high, low, tense, reduced]
PHARYNGEAL = 2 [atr, rtr]
```

The first 3 nodes (ROOT, MANNER, LARYNGEAL) are the so called "class" nodes and each define the segment's
major class, manner of articulation, and voicing/glottal state respectfully.

The following 4 nodes are the segments "place" nodes, defining place of articulation. The place nodes, 
unlike the class nodes, can be set `null`; This is denoted by setting each feature in the node to zero. 

### Graphemes

`GRAPHEMES` is a space separated list of unicode clusters that are assigned to and reference the underlying segment.
The first grapheme in the list is canonical: each subsequent grapheme is accepted, but will be coerced into the former.

#### Unicode Escapes
Unicode character escapes can be used to insert valid unicode scalar characters such as diacritics.
These take the form of `{....}` where `....` are hex digits `0-F`. Leading zero(es) can be omitted.

For example, `r{325}` becomes `r̥`

Combining diacritic marks of unicode block `03__` can represented just by the meaningful bit. 
Therefore, `r{25}` is equal to `r{325}`.

#### Character Sets
A choice of several characters can be defined within square brackets.
The first character in the set will be used first, and is therefore canonical.

For example, `= a[bc]` is equivalent to the sequence `= ab ac`.

Multiple sets can be used in a grapheme, for example `= [bd]a[bd]` which renders `= bab bad dab dad`.

Unicode escapes can be used inside a character set. This is useful for diacritic alternatives:
```
p[{2A}{46}]  => p̪ p͆

p[{61}{5C}]ɸ => p͡ɸ p͜ɸ

[ᵐⁿᶯᶮᵑᶰ]b    => ᵐb ⁿb ᶯb ᶮb ᵑb ᶰb
```