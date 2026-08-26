### Promised/Wanted Features

- Aliasing
    - [ ] Environments i.e. `s > Σ / #_`
    - [ ] Syllable boundary insertion
- Cli
    - [ ] Better rule file `.rsca` syntax
        - [ ] Rule toggling in rule files with `!`
    - [x] `trace` command
    - [ ] `sketch` (?) command
- Web
    - Reverse rule tracing
        - i.e. see which words have been effected by a give rule, rather than which rules have been applied to a given word
- Internal Changes:
    - Join Root, Manner, and Voice (like with place) in order to allow for more Manner DFs    
- Rules
    - Allow sets to be negated 
    - Set Narrowing using negation
        - i.e. `S:-N ;; Sonorants but not nasals (equiv. to S:[-nas] i guess)`
        - i.e. `C:-{s, t} ;; Consonants but not s and t`
        - i.e. `[+strid]:-z ;; Stridents but not z`


ᶴ for post-alveolar? 

### Known Bugs

- Rules
    - Syllable supra stealing. See [here](/doc/doc.md#syllable-stress)
- Cli
    - `asca conv asca` does not conserve comments in word files
