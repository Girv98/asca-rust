```peg
Conf ← Spacing Seq+ Eof
Seq  ← Ident Rules

Ident       ← Pipe? Tag Colon
Pipe        ← InputList Arrow
InputList   ← Literal (" " Literal)*
Tag         ← Literal

Rules ← Entry+
Entry ← Literal Filter? SemiColon

Filter      ← FilterType FilterList
FilterType  ← "!" / "~"
FilterList  ← String (Comma String)* Comma?

String  ← "\"" !((Eol / Eof) .)+ "\"" Spacing
Literal ← (!(Reserved / <whitespace> / Eol / Eof) .)+ Spacing

Colon       ← ":" Spacing
SemiColon   ← ";" Spacing
Arrow       ← ">" Spacing
Comma       ← "," Spacing

Reserved    ← ">" / "!" / "~" / "," / "#" / ":" / ";" / "\""
Spacing     ← (<whitespace> / Comment / Eol)*
Comment     ← '#' (! ( Eol / Eof) .)* Eol
Eol         ← "\u{000A}" / "\u{2028}" / "\u{2029}"
Eof         ← !.
```