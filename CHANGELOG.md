0.6.1
==================
Meta:
* Update to Rust 2024 Edition

Fixes:
* Lib: Infinite loop when inserting after a boundary in certain cases
* Minor: trace formatting error

0.6.0
==================
Features:
* Lib: A given word or phrase can now be traced throughout its evolution
* Lib: Multiple words can now be placed on the same line separated by a space

0.5.3
==================
Features:
* Lib: Rules can now have comments, delimited by `;;`
* Lib: Multiple contiguous underlines are now allowed in environments

0.5.2
==================
Features:
* Lib: Environments can now be grouped so they can be checked all at once, similarly to a set ([see documentation](doc/doc.md#environment-sets))

Fixes:
* Aliases: Segments will no longer by double printed when using the `+` operator on a `+long` matrix

0.5.1
==================

Fixes:
* Alphas and Variables used after an ellipsis or an optional work as intended

Performance:
* Better unicode normalisation (thanks to [@j624364](https://github.com/j624364))

0.5.0
==================
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
Fixes:
* Aliases: Non-node segment features are now correctly applied.
* Aliases: Applying stress or tone with the plus operator no longer sets the segment length to 1.

Features:
* Cli: Terminal output is now aligned for easier comprehension

0.4.3
==================

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

Features:
* Lib: Allow for groups and matrices to be used in alias rules
* Lib: Introduce character addition in alias rules

Fixes:
* Cli: Bug introduced in 0.4.0 whereby tags were not being recognised

Tweaks:
* Lib: Word-initial stress characters are removed when aliasing syllable boundaries

0.4.1
==================

Features:
* Lib: Allow unicode escapes within alias rules
* Lib: Adds several named escapes for common diacritics

0.4.0
==================

Features: 
* Lib: Ability to add custom aliases/romanisation
* Cli: Introduce alias file type
* Cli: Apply alias files to sequences

0.3.1
==================

Features: 
* Lib: Add `ᶻ` diacritic as a voiced alternative to the stridentised diacritic `ˢ`.

0.3.0
==================

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

Bug fixes:
* Fix bug whereby IPA literals with a feature alpha modifier were not correctly matching.
    * For example, `l:[Asyll] > r:[Asyll]` would not match the positive case.