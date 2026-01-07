### Promised/Wanted Features

- Aliasing
    - Environments i.e. `s > Σ / #_`
    - Syllable boundary insertion
- Cli
    - Better rule file `.rsca` syntax
- Rule Features
    - Underline in structures 
        - i.e. `N > * / <..V_>` instead of `N > * / V_$` to guarantee same syllable
    - Sets
        - Allow sequences of items
        - Allow entire set to be modified
            - i.e. `{i, e}:[+long]` rather than `{i:[+long], e:[+long]}`
            - This will clash with lexing of `:{...}:`


### Known Bugs

- Insertion Rules
    - Edge case infinite loops such as `* > e / ()_`
    - Syllable supra stealing. See [here](/doc/doc.md#syllable-stress)
- Cli
    - `asca conv asca` does not conserve comments in word files