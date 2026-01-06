### Promised/Wanted Features

- Aliasing
    - Environments i.e. `s > Σ / #_`
    - Syllable boundary insertion
- Cli
    - Better rule file `.rsca` syntax
- Rule Features
    - Underline in structures i.e. `N > * / <..V_>` instead of `N > * / V_$` to guarantee same syllable
    - Sets to contain sequences of items


### Known Bugs

- Insertion Rules
    - Edge case infinite loops such as `* > e / ()_`
    - Syllable supra stealing. See [here](/doc/doc.md#syllable-stress)