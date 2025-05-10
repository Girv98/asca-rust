```
conf = spacing seq+ eof
seq = ident rules

ident = pipe? tag COLON
pipe = inputlist ARROW
inputlist =  literal (" " literal)*
tag = literal

rules = entry+
entry = literal filter? SEMIC

filter = filtertype filterlist
filtertype = "!" | "~"
filterlist = string (COMMA string)* COMMA?

string = "\"" !((eol | eof) .)+ "\"" spacing
literal = (!(reserved | <whitespace> | eol | eof) .)+ spacing

COLON = ":" spacing
SEMIC = ";" spacing
ARROW = ">" spacing
COMMA = "," spacing

reserved = ">" | "!" | "~" | "," | "#" | ":" | ";" | "\""
spacing = (<whitespace> | comment | eol)*
comment = '#' (! ( eol | eof) .)* eol
eol = "\u{000A}" | "\u{2028}" | "\u{2029}"
eof = !.
```