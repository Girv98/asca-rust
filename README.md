# ASCA

[ASCA](https://asca.girv.dev) is a Sound Change Applier written in Rust with WASM.

Repo for the web UI is [here](https://github.com/Girv98/asca)

## Notable Features
- Out of the box Distinctive Features and Alpha Notation
- Manipulation of Syllables, Stress, and Tone
- Cross Word-Boundary Interactions
- Syllable Structure Matching
- Left-to-Right and Right-to-Left Propagation
- Metathesis and Hyperthesis (Long Range Metathesis)
- Optional/Repeating Segments
- References
- Syntax which adheres to conventional standard notation
- Digraph and Diacritic Support
- Romanisation/Deromanisation

User guide can be found [here](./doc/doc.md).

[Changelog](./CHANGELOG.md)

## Cli

A cli specific user guide can be found [here](./doc/doc-cli.md).

### Installation

[Precompiled binaries are available for Linux, Windows, and macOS.](https://github.com/Girv98/asca-rust/releases)
Add it to your path to have the `asca` command available in your terminal.

Alternatively, if you have **Rust** installed, asca can be installed with `cargo`.


```bash
cargo install asca
```