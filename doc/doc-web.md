# ASCA Web Documentation

### Quick Links
* [Rules](#rules)
    * [Adding Rules](#adding-rules)
    * [Removing Rules](#removing-rules)
    * [Reordering Rules](#reordering-rules)
    * [Cloning Rules](#cloning-rules)
    * [Temporarily Disabling Rules](#temporarily-disabling-rules)

* [Debugging](#debugging)
    * [Word Tracing](#word-tracing)
    * [Unknown Segments](#unknown-segments)
* [Aliasing/Romanisation](#aliasingromanisation)
* [Saving and Loading](#saving-and-loading)
* [Hotkeys and Keyboard Navigation](#hotkeys-and-keyboard-navigation)

## Rules

ASCA organises rules into independent blocks. This allows you to group a multi-stage sound change (such as a chain shift) together, 
and clearly demarcate it from the potentially long list of sound changes you may have. These rule blocks can then be collapsed so that 
you can forget about them and treat them like a black box. The title and description areas of each rule block allow you to optionally 
document your sound changes in situ. In the future, this could be used to automatically generate documentation from your work in ASCA. 

### Adding Rules
The `Add Rule` button is used to create a new black rule block. The block can be added to the bottom or the top of the current list, 
toggled using the arrow to the right of the button.

### Removing Rules
A single rule block can be removed with the `x` button present on each block, or all blocks can be removed with the `Clear All` button.

### Reordering Rules
The block structure allows you to easily reorder rules to test certain orderings without accidentally reordering dependant sub-rules. A 
rule can be dragged, much like a window, by holding any part of the 'title' section of a rule block. This can be done both when the rule 
is collapsed or expanded. Rule blocks can also be moved with the keyboard (see [Hotkeys and Keyboard Navigation](#hotkeys-and-keyboard-navigation)).

### Cloning Rules
As a language can go through the same sound change multiple times, it is useful to not have to rewrite the same rules again and again. 
A rule block can be duplicated using the copy widget on the block. This has the effect of copying the block below itself. This rule block 
can then be moved to the desired location with the drag and drop feature.

### Temporarily Disabling Rules
A rule block can be disabled using the toggle switch widget present on each block. This makes the block transparent to ASCA and therefore 
will not be appied to the input.
Blocks can be globally toggled on and off with the `Disable/Enable All` button.

## Debugging
### Word Tracing
The evolution of a given input phrase can be shown using the trace dropdown above the input box. This will output each rule that is applied to 
the phrase and how the phrase changes due to it. The applied rule blocks are also highlighted for easier discernment.

### Unknown Segments
When ASCA is unable to represent a given segment, `�` will be printed in its place. If this occurs, a table of each unique unknown segment and its 
value will be shown at the end of the output. You can use this to cross reference with the values spreadsheet to diagnose the issue. Each unique 
unknown is color-coded (and hoverable) so that you can easily discern each value.

## Aliasing/Romanisation
Aliases can be defined through the `aliases` button, which will bring up a modal containing the input fields,see [Custom aliasing](./doc.md#custom-aliasing--deromanisation) 
for more

Note that aliases, like the input, are not cleared with the `Clear All` button.

## Saving and Loading

Input words and rules can be saved to desktop and loaded into asca using JSON format:

``` JSON
{
    "into" : ["deromanisation rules here"],
    "from" : ["romanisation rules here"],
    "words": ["words", "go", "here"],
    "rules": [
        {
            "name": "Rule 1",
            "rule": ["First subrule", "Second subrule"],
            "description": "Does something"
        },
        {
            "name": "Rule 2",
            "rule": ["First subrule", "Second subrule"],
            "description": "Does something"
        }
    ]
}
```
On each run, the current state is saved to local storage. This affords you the ability to quit out and not lose progess.


## Hotkeys and Keyboard Navigation

Global:
* `Shift+Enter` Run
* `Alt+R` Focus first rule block
* `Alt+W` Focus input textbox
* `Alt+A` Add Rule
* `Alt+Q` Change direction
* `Alt+C` Collapse/Expand All
* `Alt+X` Clear Rules
* `Alt+Z` Toggle All Rules
* `Alt+L` Open Aliases Modal
* `Alt+S` Save
<!-- * `Alt+O` Load -->

Within a text box:
* `Alt+↑` Move line(s) above
* `Alt+↓` Move line(s) below
* `Alt+Shift+↑` Duplicate line(s) above
* `Alt+Shift+↓` Duplicate line(s) below
* `Ctrl+/` or `Ctrl+;` Comment out/uncomment line (rules box only)
* `Shift+Delete` Cut line (rules box only)

When an outer rule block is focused:
* `Enter`   Collapse and expand focused block
* `Delete`  Remove focused block
* `Shift+Home` Focus first rule block
* `Shift+End`  Focus last rule block
* `Shift+A` Focus rule title
* `Shift+S` Focus rule input
* `Shift+D` Focus description
* `Shift+↑` Focus previous block
* `Shift+↓` Focus next block
* `Shift+T` Toggle focused block
* `Alt+↑`   Move block up (wraps)
* `Alt+↓`   Move block down (wraps)
* `Shift+Alt+↑`   Duplicate focused block above
* `Shift+Alt+↓`   Duplicate focused block below

When any part of a rule block is focused:
* `Shift+Backspace` Focus outer rule block