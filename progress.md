### Promised/Wanted Features

- Aliasing
    - Environments i.e. `s > Σ / #_`
    - Syllable boundary insertion
- Cli
    - [ ] Better rule file `.rsca` syntax
- Rule Features
    - [x] Underline in structures 
        - i.e. `N > * / <..V_>` instead of `N > * / V_$` to guarantee same syllable
    - Sets
        - [ ] Allow sequences of items
        - [x] Allow entire set to be modified
            - i.e. `{i, e}:[+long]` rather than `{i:[+long], e:[+long]}`
- Web:
    - Reverse rule tracing
        - i.e. see which words have been effected by a give rule, rather than which rules have been applied to a given word
- Internal Changes:
    - Join Root, Manner, and Voice (like with place) in order to allow for more Manner DFs    

### Known Bugs

- Insertion Rules
    - Edge case infinite loops such as:
        - `* > e / ()_`
        - `V > * / [Aplace]_[-Aplace]`
    - Syllable supra stealing. See [here](/doc/doc.md#syllable-stress)
- Cli
    - `asca conv asca` does not conserve comments in word files

ᶴ for post-alveolar? 