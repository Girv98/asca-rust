### Promised/Wanted Features

- Aliasing
    - Environments i.e. `s > Σ / #_`
    - Syllable boundary insertion
- Cli
    - [ ] Better rule file `.rsca` syntax
- Rule Features
    - [x] Underline in structures 
        - i.e. `N > * / <..V_>` instead of `N > * / V_$` to guarantee same syllable
        - ideal for syllable fixing e.g. `* > $ / <VC_CV>`
    - Sets
        - [ ] Allow sequences of items
        - [x] Allow entire set to be modified
            - i.e. `{i, e}:[+long]` rather than `{i:[+long], e:[+long]}`
- Web:
    - Reverse rule tracing
        - i.e. see which words have been effected by a give rule, rather than which rules have been applied to a given word
- Internal Changes:
    - Join Root, Manner, and Voice (like with place) in order to allow for more Manner DFs    

ᶴ for post-alveolar? 

### Known Bugs

- Rules
    - Ellipses in `&` rules don't work as a user might expect when the two sides are uneven
        - e.g. `pf..s > &` results in `pfas > sfap` rather than `pfas > sapf` or `pfas > safp`
        - This is because the pivot point is actually `f` and `..` is currently ignored, used only for initial matching
    - Edge case infinite loops such as:
        - `* > e / ()_`
        - `* > e / ([])_`
        - May have to actively check if the rule is self replicating, i.e. it creates the environment where it can apply
    - Syllable supra stealing. See [here](/doc/doc.md#syllable-stress)
- Cli
    - `asca conv asca` does not conserve comments in word files
