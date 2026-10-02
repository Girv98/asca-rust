### Promised/Wanted Features

- Aliasing
    - [ ] Environments i.e. `s > Σ / #_`
    - [ ] Syllable boundary insertion
- Cli
    - [ ] Better rule file `.rsca` syntax
        - [ ] Rule toggling in rule files with `!`
    - [ ] `sketch` (?) command
- Web
    - Reverse rule tracing
        - i.e. see which words have been effected by a give rule, rather than which rules have been applied to a given word
- Internal Changes:
    - Join Root, Manner, and Voice (like with place) in order to allow for more Manner DFs   
- Segments:
    - [x] `f,v => [-strid]`
    - [ ] Add prenasalised implosives
    - [ ] Add prenasalised fricatives
    - [ ] Add rhotic affricates i.e. `/d͡r/`
    - [ ] Encode voiceless segments with tails or place diacritics with `U+030A Combining Ring Above` rather than `U+0325 Combining Ring Below`
- Rules
    - Allow sets to be negated 
    - Allow syllables to be negated 
    - Allow boundaries to be negated i.e. `i > j / _ -$ V` equiv. to `i > j / _ <(..)_V(..)>`


ᶴ for post-alveolar? 

### Known Bugs

- Rules
    - Syllable supra stealing. See [here](/doc/doc.md#syllable-stress)
- Cli
    - `asca conv asca` does not conserve comments in word files
