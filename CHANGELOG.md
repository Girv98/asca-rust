NEXT RELEASE
==================
[0.9.3...latest](https://github.com/Girv98/asca-rust/compare/0.9.3...master)

Breaking:
* Lib: 
    * Rules such as `a > e` now preserve length
        * Previously, this rule would cause a word such as `ha:t` to become `het`. 
        * This change now brings the behaviour of IPA Literals in line with Groupings and Matrices.
        * Previous behaviour is preserved when the input is matched by length e.g. `a:[+lng] > e` which is equiv. to `a:[+lng] > e:[-lng]`
        * See [here](https://github.com/Girv98/asca-rust/blob/ed82cfc45e556addda79bdcbfed7b0bf08e1ba8d/doc/doc.md#substituting-long-ipa) for an archived explanation for the previous behaviour
    * Library API changes
        * `Word` and `Phrase` functions now split into variants with and without alias parameters
            * i.e. `Word::new` && `Word::with`, `Word::render` && `Word::render_with`
        * Functions now generic over types that implement `AsRef<str>`

Features:
* Cli:
    * Add `<`, `>`, `<=`, `>=` rule filter operators
        * Allows inclusion of only rules before or after a given rule in a file
        * e.g. `setup > pg-spirant: pgmc/pre.rsca; pgmc/early.rsca <= "Germanic Spirant's Law";`
* Lib:
    * Combined Alpha Suprasegmentals
        * Allows for multi-dimensional suprasegmentals (i.e. stress and length) to be referenced with one alpha.
        * `[A long, B overlong]` can be replaced with just `[A length]`
        * `[A stress, B sec.stress]` can be replaced with just `[A anystress]`
    * Allow for the underline to be placed within an environment structure.
        * e.g. `VN > [+nas] / <(..)_>` instead of `VN > [+nas] / _$` guarantees `V` and `N` are of the same syllable and `N` is coda.
    * Allow for sets, as a whole, to be modified by a matrix
        * i.e. `{i, e}:[+long]` rather than `{i:[+long], e:[+long]}`
    * Add aliases `J` and `W` for `ʝ` and `ɯ` respectively
    * New insertion implementation which is hopefully more stable

Fixes:
* Lib:
    * Rules such as `$ < a$` no longer hang
    * Corrected structure location error highlighting

Tweaks:
* Cli: (Potentially Breaking) Update file string sanitation so that special characters and punctuation is now omitted rather than being replaced with `-`
* Lib: Removed redundant error locations from error messages

Perf: 20-30% average execution speed up of example configs

0.9.3
==================
[0.9.2...0.9.3](https://github.com/Girv98/asca-rust/compare/0.9.2...0.9.3)

Fixes: 
* Cli: Core ASCA errors return with exit code 1 ([#7](https://github.com/Girv98/asca-rust/issues/7))
* Lib: 
    * Avoid infinite loop after lengthening a segment in certain situations at the end of a word
    * Enforce use of commas within a set i.e. `{p,t,k}` not `{ptk}` (trailing comma allowed)

0.9.2
==================
[0.9.1...0.9.2](https://github.com/Girv98/asca-rust/compare/0.9.1...0.9.2)

Fixes:
* Lib: 
    * Trying to swap syllables between words i.e. `% ## % > &` now works
    * Leading spaces before a phrase do not affect output

Perf: Rule parsing and application is now parallelised

0.9.1
==================
[0.9.0...0.9.1](https://github.com/Girv98/asca-rust/compare/0.9.0...0.9.1)

Feats: 
* Lib: Structures and References now allowed inside sets

Fixes:
* Lib: 
    * Syllable boundaries match the end of the word just as they currently match the start of the word (use `| :{#_,_#}:` to only match internal syllable boundaries)
    * Sets with syllable boundaries work properly in substitution rules
    * Long segment to syllable boundary substitution works properly
    * Substituting a syllable boundary with a Structure works as it should
    * `$ > $` no longer hangs

0.9.0
==================
[0.8.4...0.9.0](https://github.com/Girv98/asca-rust/compare/0.8.4...0.9.0)

Features:
* Lib: 
    * Cross Boundary Interactions with `##` allowing for:
        * Word Boundary Metathesis/Rebracketing e.g. `/ə ˈnei.pɹən/ > /ən ˈei.pɹən/`
        * Affixation e.g. `/pəɹ ˈsɛnt/ > /pəɹˈsɛnt/`
        * External Sandhi e.g. `an bean > an bhean`
    * Added Retroflex diacritic alias `"r`
    * References can now be modified with diacritics
    * (Potentially Breaking) New substitution implementation, allowing for:
        * Meaningful Ellipses e.g. `p..t > pf..s` vs `p..t > p..fs`
        * Implicit Boundary Insertion/Deletion e.g. `V$N > VN`

Fixes:
* Lib: 
    * Removed edge cases in segment rendering
    * Retroflex diacritic works as expected
    * Diacritic payload is applied correctly
    * Long segments are correctly deleted and metathesised
* Cli:
    * ANSI escape codes for coloured output render correctly on Windows 10
    * Blank lines are omitted from the start of the printed output

Meta: `Variables` renamed to `References`

0.8.4
==================
[0.8.3...0.8.4](https://github.com/Girv98/asca-rust/compare/0.8.3...0.8.4)

Fixes:
* Lib:
    * `[-long]` matrices match correctly after an ellipsis
    * A comment after a deletion rule with no environment no longer errors
    * Special environment checking works properly with a following comment

0.8.3
==================
[0.8.2...0.8.3](https://github.com/Girv98/asca-rust/compare/0.8.2...0.8.3)

Features:
* Lib:
    * Sets and Options now allowed within non-output Structures
    * Ellipses can now be wrapped in brackets `(..)` to skip 'zero or more' segments instead of 'one or more' segments.

0.8.2
==================
[0.8.1...0.8.2](https://github.com/Girv98/asca-rust/compare/0.8.1...0.8.2)

Fixes:
* Lib: 
    * Fixed infinite loop caused by erroneous use of `^` in certain situations.
    * Fixed `qǀ`, `qǃ`, `qǁ`, `q‼`, and `qǂ` being falsely encoded as `+voice`

0.8.1
==================
[0.8.0...0.8.1](https://github.com/Girv98/asca-rust/compare/0.8.0...0.8.1)

Features:
* Wasm: 
    * Pass parsed input phrases to WasmResult
    * Pass boolean representing a success/error to WasmResult
* Lib: Add some more feature shorthands

Fixes:
* Lib: Remove floating `'` from the end of some line positions within certain error messages
* Cli: 
    * Blank lines are omitted from the end of the printed output
    * Progress and info messages are sent to stderr instead of stdout

0.8.0
==================
[0.7.7...0.8.0](https://github.com/Girv98/asca-rust/compare/0.7.7...0.8.0)

Breaking:
* Lib:
    * Diacritic aliases are now preceded by `"` instead of `^` to remove ambiguity between `ʔ͡h` and `ʔʰ` which are now `?^h` and `?"h` respectively.
    * Removed dedicated americanist aliases in favour of custom aliasing. The following can be used as a replacement:
        * Into IPA: `ł, ñ, ¢, ƛ, λ => ɬ, ɲ, t͡s, t͡ɬ, d͡ɮ`
        * From IPA: `ɬ, ɲ, t͡s, t͡ɬ, d͡ɮ => ł, ñ, ¢, ƛ, λ`
    * Fix: Prenasalised palatal and retroflex consonants now use consistent diacritics (`ᶮC` and `ᶯC` respectively)

Features:
* Lib: More inbuilt diacritic aliases using the new `"` prefix ([see doc](doc/doc.md#inbuilt-aliases))

0.7.7
==================
[0.7.5...0.7.7](https://github.com/Girv98/asca-rust/compare/0.7.5...0.7.7)

Fixes:
* Lib: Fixes bug introduced in 0.7.5 where words in a phrase are output without spaces

0.7.5
==================
[0.7.4...0.7.5](https://github.com/Girv98/asca-rust/compare/0.7.4...0.7.5)

Features:
* Wasm: Pass traced rule indices to WasmResult

0.7.4
==================
[0.7.3...0.7.4](https://github.com/Girv98/asca-rust/compare/0.7.3...0.7.4)

Features:
* Lib: Inbuilt aliases for common diacritics

Fixes:
* Lib: A `$` or `*` alone in the output of a deromanisation rule is now allowed and no longer crashes

0.7.3
==================
[0.7.1...0.7.3](https://github.com/Girv98/asca-rust/compare/0.7.1...0.7.3)

Features:
* Lib: Right-to-left propagation with the tilde `~ | ~>` operator for easy regressive harmony ([see documentation](./doc/doc.md#true-right-to-left-propagation))

Fixes:
* Lib/Web: Aliases now work as expected when tracing

0.7.1
==================
[0.7.0...0.7.1](https://github.com/Girv98/asca-rust/compare/0.7.0...0.7.1)

Features:
* Lib: Debug representation of `Segment` values and `try_as_grapheme` function

Fixes:
* Lib: 
    * Double slash `//` for exception block no longer errors in certain cases
    * Fixed formatting of errors that have two underlines

0.7.0
==================
[0.6.1...0.7.0](https://github.com/Girv98/asca-rust/compare/0.6.1...0.7.0)

Breaking:
* Cli:
    * New config file syntax (conversion is available through `asca conv config`) ([#6](https://github.com/Girv98/asca-rust/issues/6))
    * Extensions of files (if any) used within a config must now be specified ([#6](https://github.com/Girv98/asca-rust/issues/6))
* Lib:
    * Removes unused `AliasRuntimeError`
    * Module structure changes

Features:
* Cli:
    * Multiple word files can be passed to `asca run` ([#5](https://github.com/Girv98/asca-rust/issues/5))
    * Word files can be passed by stdin to `asca run` ([#5](https://github.com/Girv98/asca-rust/issues/5))
    * More freedom with sequence input ([#6](https://github.com/Girv98/asca-rust/issues/6)):
        * Each sequence can now have multiple tags piped to it
        * Piped tags and word files can be in any order
* Lib: 
    * `ParsedRules` struct 
    * `Rule` tracing
    * Structs such as `Word` and their methods are now available to use

Fixes:
* Cli: 
    * `into` aliases work as expected ([#4](https://github.com/Girv98/asca-rust/issues/4))
    * No longer an issue if there are multiple configs within one directory
* Lib(aliases): Segment length character `ː` is correctly omitted when removing a long segment


0.6.1
==================
[0.6.0...0.6.1](https://github.com/Girv98/asca-rust/compare/0.6.0...0.6.1)

Meta:
* Update to Rust 2024 Edition

Fixes:
* Lib: Infinite loop when inserting after a boundary in certain cases
* Minor: trace formatting error

0.6.0
==================
[0.5.3...0.6.0](https://github.com/Girv98/asca-rust/compare/0.5.3...0.6.0)

Features:
* Lib: A given word or phrase can now be traced throughout its evolution
* Lib: Multiple words can now be placed on the same line separated by a space

0.5.3
==================
[0.5.2...0.5.3](https://github.com/Girv98/asca-rust/compare/0.5.2...0.5.3)

Features:
* Lib: Rules can now have comments, delimited by `;;`
* Lib: Multiple contiguous underlines are now allowed in environments

0.5.2
==================
[0.5.1...0.5.2](https://github.com/Girv98/asca-rust/compare/0.5.1...0.5.2)

Features:
* Lib: Environments can now be grouped so they can be checked all at once, similarly to a set ([see documentation](doc/doc.md#environment-sets))

Fixes:
* Aliases: Segments will no longer by double printed when using the `+` operator on a `+long` matrix

0.5.1
==================
[0.5.0...0.5.1](https://github.com/Girv98/asca-rust/compare/0.5.0...0.5.1)

Fixes:
* Alphas and Variables used after an ellipsis or an optional work as intended

Performance:
* Better unicode normalisation (thanks to [@j624364](https://github.com/j624364))

0.5.0
==================
[0.4.4...0.5.0](https://github.com/Girv98/asca-rust/compare/0.4.4...0.5.0)

Features:
* Structure Matching [(see docs)](/doc/doc.md#syllable-structure-matching)
    * Allows for matching a syllable based upon its segments
    * Easy insertion of whole syllables
* Syllables can now be substituted in the place of a segment

Fixes:
* Modifying a syllable `%` with secondary stress is no longer a syntax error 🙃🙃🙃
* Insertion between segments propagates properly after a previous "no match"
* Inserting before a syllable boundary where a pre-environment is specified now works as expected
    * Previously, `* > ? / V_$C` would not work as expected, while `* > ? / _$C` would
* Applying a negative feature to a node that is not present does not attach the node
    * For example, assigning `[-round]` to `/t/` would make it `[+lab, -ldental, -rnd]` when it should have no effect.

0.4.4
==================
[0.4.3...0.4.4](https://github.com/Girv98/asca-rust/compare/0.4.3...0.4.4)

Fixes:
* Aliases: Non-node segment features are now correctly applied.
* Aliases: Applying stress or tone with the plus operator no longer sets the segment length to 1.

Features:
* Cli: Terminal output is now aligned for easier comprehension

0.4.3
==================
[0.4.2...0.4.3](https://github.com/Girv98/asca-rust/compare/0.4.2...0.4.3)

Fixes:
* Aliases: Matching multiple segments with the plus operator will no longer overwrite the tail segments

Tweaks:
* Performance
    * Reduction in unnecessary allocations
* Changes to tone
    * Capped at 4 digits
    * Tone of `0` is now equivalent to no tone, matching documentation
    * Improvements to how tone is dealt with when merging syllables

0.4.2
==================
[0.4.1...0.4.2](https://github.com/Girv98/asca-rust/compare/0.4.1...0.4.2)

Features:
* Lib: Allow for groups and matrices to be used in alias rules
* Lib: Introduce character addition in alias rules

Fixes:
* Cli: Bug introduced in 0.4.0 whereby tags were not being recognised

Tweaks:
* Lib: Word-initial stress characters are removed when aliasing syllable boundaries

0.4.1
==================
[0.4.0...0.4.1](https://github.com/Girv98/asca-rust/compare/0.4.0...0.4.1)

Features:
* Lib: Allow unicode escapes within alias rules
* Lib: Adds several named escapes for common diacritics

0.4.0
==================
[0.3.1...0.4.0](https://github.com/Girv98/asca-rust/compare/0.3.1...0.4.0)

Features: 
* Lib: Ability to add custom aliases/romanisation
* Cli: Introduce alias file type
* Cli: Apply alias files to sequences

0.3.1
==================
[0.3.0...0.3.1](https://github.com/Girv98/asca-rust/compare/0.3.0...0.3.1)

Features: 
* Lib: Add `ᶻ` diacritic as a voiced alternative to the stridentised diacritic `ˢ`.

0.3.0
==================
[0.2.1...0.3.0](https://github.com/Girv98/asca-rust/compare/0.2.1...0.3.0)

Features: 
* Cli: Ability to convert a config tag into a web-asca json file 
* Cli: Ability to generate tab completions for a given shell
* Cli: Print available config tags when no match found

Tweaks:
* Cli: More consistent errors
* Lib: Change wasm function to `run_wasm`
* Lib: Re-export Error module

0.2.1
==================
[0.2.0...0.2.1](https://github.com/Girv98/asca-rust/compare/0.2.0...0.2.1)

Bug fixes:
* Fix bug whereby IPA literals with a feature alpha modifier were not correctly matching.
    * For example, `l:[Asyll] > r:[Asyll]` would not match the positive case.