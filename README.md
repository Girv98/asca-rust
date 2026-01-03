# ASCA

[ASCA](https://asca.girv.dev) is a Sound Change Applier written in Rust, with WASM for Web.

Repo for the web UI is [here](https://github.com/Girv98/asca)

## Notable Features
- Predefined phonemic segments (no need to define categories) 
- Out of the box Distinctive Features and Alpha Notation
- Manipulation of Syllables, Stress, and Tone
- Digraph and Diacritic Support
- Syntax which adheres to conventional notation
- [Romanisation/Deromanisation](./doc/doc.md#custom-aliasing--deromanisation)
- [Metathesis and Hyperthesis (Long Range Metathesis)](./doc/doc.md#metathesis-rules)
- [Cross Word-Boundary Interactions](./doc/doc.md#cross-word-boundary-operations)
- [Syllable Structure Matching](./doc/doc.md#syllable-structure-matching)
- [Left-to-Right and Right-to-Left Propagation](./doc/doc.md#propagation)
- [Optional/Repeating Segments](./doc/doc.md#optional-segments)
- [References](./doc/doc/#references)

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