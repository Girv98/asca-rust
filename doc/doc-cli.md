# ASCA Command Line Documentation

### Quick Links
* [File Formats](#file-formats)
* [Sequences](#sequences)
    * [The Config File](#the-config-file)
    * [Filters](#filters)
    * [Pipelines](#pipelines)
    * [Using Alias Files](#using-alias-files)
* [Usage](#usage)
    * [Run](#run-command)
    * [Seq](#seq-command)
    * [Conv](#conv-command)
* [Shell Completions](#shell-completions)

## File Formats
Asca-cli differs from the web implementation by using three file formats that define a set of input words, a set of rules, and a set of romanisations.
An existing web json file can be converted into these formats (and vice-versa) using the [conv command](#conv-command).

### Word file (.wsca)
Words are defined similarly as to the input field on [asca-web](doc.md). That is, each word is declared on a new line in ipa form,
with optional comments marked by `#`. These comments do not appear in the output.

See [pie-uvular](../examples/indo-european/pie-uvular-common.wsca) for an example.

### Rule file (.rsca)

***NOTE: This format is to be updated by the next minor release. Like the 0.7.0 config format changes, automatic conversion to the new format will be provided through `asca conv` ([see usage](#conv-command))***

The rule file mimics the structure of rules in the web implementation.
Each rule group is defined as follows:

- A rule title, preceded by `@`.
- A newline delimited list of sub rules.
    - Can be indented for clarity
    - Empty lines are allowed
- A rule description, preceded by `#`
    - Can be multiple lines, with each line starting with `#`.

Example from a [proto-germanic](../examples/indo-european/germanic/pgmc/early.rsca) implementation.
``` diff
@ Grimms Law 
    [+cons, -son, -cont, -voice] > [+cont]
    [+cons, -son, -cont, +voice, -sg] > [-voice]
    [+cons, +voice, +sg] > [-sg]
    
    [+cons, -son, +voice] > [+cont] / {V:[-red], G}_{V, G}
# Chain shift of the three series of plosives.
# Voiceless plosives become fricatives
# Voiced plosives are devoiced
# Aspirated plosives become voiced plosives or fricatives.

@ Verners Law
    [-voice, +cont] > [+voice] / V:[-stress]([+son])_
# Voiceless fricatives are voiced when preceded by an unaccented vowel.
# Including cases where the vowel and fricative are separated by a sonorant.
```

### Romanisation file (.alias)
An alias file has two sections, `into` and `from`. These describe deromanisation and romanisation, respectively.

Each section is introduced with a tag preceded by `@`, then followed by a newline delimited list of alias rules (similiar to a rule file).

Example:
```
@into
    汉 > xan:[tone: 51] 
    语 >   y:[tone:214]
    
@from
    xan:[tone: 51] => 汉
      y:[tone:214] => 语
    $ > *
```

## Sequences
The seq command allows you to run language family projects, defined within a `.asca` file, letting you create a pipeline from parent to daughter languages and optionally generating output lexicons at each stage.

For an example project, see the indo-european example folder [here](../examples/indo-european/germanic).

### The Config File
***NOTE: This is the updated syntax as of version 0.7.0. Automatic conversion of old configs can be done through `asca conv config` ([see usage](#conv-command))***

Each unit is called a sequence. 
A simple sequence consists of a sequence tag followed by a semicolon separated list of paths to rule files which describe the sequence. 
(Note: All paths used in a sequence are relative to the config file. 
Files also do not need an asca specified file extension e.g. "rules.txt" will also be valid as long as it parses correctly)

``` diff
# This is a comment
alpha:
    rules1.rsca;
    rules2.rsca;

# This is also valid
alpha: rules1.rsca; rules2.rsca;
```

A given tag can be run with `seq -t <tagname>`. 
With `--output | -o`, the result will be stored in the directory `./out/<tagname>/*`.

Word files can be specified for each sequence by listing them before the tag, followed by `>`.
These word files will be appended together and used as input. 
The word file list is optional, but they then must be passed on the command line with `seq -w <path>...`.

``` diff
foo.wsca bar.wsca > alpha: 
    rules1.rsca;
    rules2.rsca;

# This means "With the name 'alpha',
# Apply the sound changes within './rules1.rsca' and then './rules2.rsca'
# on the words within './foo.wsca' and './bar.wsca'
```

#### Filters
A filter can be placed after each rule file to select specific rules within them. 
Filters are case insensitive, but they must elsewise be identical in name to a rule defined within the file (apostrophes and such). 
Multiple rules for each filter can be specified, separated by commas. 

`!` means to exclude the following rules from the sequence. 

`~` means to select only the following rules from the sequence (NOTE: They will be applied in the order they appear in the filter, not as they are within the rule file). 
This is useful for defining commonly used sound changes within a 'global' sound change file and reusing them when needed. 

```diff
foo.wsca bar.wsca > beta:
    rules1.rsca ! "Glottal Deletion";
    rules2.rsca ~ "Cluster Simplification", "Hap(lo)logy";

# The above sequence tagged 'beta' means:
# - Using the words defined in './foo.wsca' and './bar.wsca',
# - Apply ./rules1.wsca WITHOUT 'Glottal Deletion'.
# - Then, apply ONLY 'Cluster Simplification' and 'Hap(lo)logy', in that order, from './rules2.wsca'.
```

#### Pipelines

Instead of word files, another defined sequence can be referenced with its tag. This will run the referenced sequence and use the resulting words as input. 
This is useful for defining daughter languages or branches. ASCA will assume an input element is a tag when there is no file extension and (in the case of extension-less files) there is no file of that name in the config directory. 

``` diff
example-lex.wsca > latin:
    foo.rsca;
    bar.rsca;

# Sequences do not need to be defined in the order of pipeline use.
old-spanish > spanish:
    baz.rsca;
    foo.rsca;

latin > old-spanish:
    foo.rsca; bar.rsca; baz.rsca;
```

A sequence can have multiple piped inputs if so needed. Word files can also be used alongside piped tags. 
This has the same effect as using multiple word files, with the contents being appended together.
This allows for you to add forms at an intermediate sequence (i.e. loanwords or neologisms).

``` diff
alpha beta foo.wsca > gamma:
    rules.rsca;

# With the tag 'gamma'
# Using the outputs of 'alpha' and 'beta' followed by './foo.wsca'
# Apply './rules.rsca'
```

#### Using Alias Files

A romanisation file is added to a sequence in the same way as a word file or piped tag. The only difference is that a sequence can only have one alias file in its direct input.

``` diff
gamma aliases.alias > delta:
    rules.rsca;

# Order does not matter, as long as there is only one
aliases.alias gamma > delta:
    rules.rsca;
```

## Usage
```
Usage: asca [-v | --version] [-h | --help] <command>

Commands:
    run     Run basic cli
    seq     Run an asca config
    conv    Convert between different formats, such as the asca-web json file and the cli wsca/rsca format.
```
### Run command
```
usage: asca run [WORDS]... ([-j | --from-json <path>] | [-r | --rules <path>]) [-c | --compare <path>]
                            [-o | --output <path>]      [-l | --alias <path>]  [-h | --help]
Arguments:
    [WORDS]...  Paths to wsca files containing the input words.
                If -j is provided, these words with be used instead of the words listed in the json file

Options:
    -j  <path>  Path to an asca-web json file, mutually exclusive with -r.
                - If -w is supplied, those words will be used instead of those defined in the json.
    -r  <path>  Path to a rsca file containing the rules to be applied, mutually exclusive with -j.
                - If neither -j nor -r are provided, asca will look for a file in the current directory.
    -l  <path>  Path to an alias file containing romanisations to and from.
    -c  <path>  Path of a wsca file to compare with the result
    -o  <path>  Desired path of the output file
                - If a directory is provided, asca will create an out.wsca file in that directory
                - If not provided, asca will not write to any file
    -h          Print help
```
### Seq command
```
usage: asca seq [PATH] [-t | --tag <tag>]  [-w | --words <path>] [-a | --all-steps] 
                       [-y | --overwrite]  [-n | --no-overwrite] [-o | --output]   
                       [-i | --output-all] [-h | --help]
Arguments:
    [PATH]  Path to the config file or the directory it is within.
Options:
    -t  <tag>   Run a given tag in the config file.
                - If not provided, all tags in the config will be run.
    -w  <path>  Paths to wsca files.
                - If provided, these will be used instead of the word files defined in the config.
    -a          Print all intermediate steps in a sequence.
    -o          When given, asca will create an out folder within the [PATH] directory.
    -y          Accept cases where an output file would be overwritten.
    -n          Reject cases where an output file would be overwritten.
    -i          Output all intermediate steps in a sequence.
    -h          Print help
```
### Conv command
```
usage: asca conv [-h | --help] <command>

Commands:
    asca    Convert a word file and rule file into an asca-web json file
    json    Convert a json file into separate word and rule files
    tag     Convert a tag within a config file into an asca-web json file
    config  Convert an old config file into the new format


usage: asca conv asca [-w | --words <path>] [-r | --rules <path>] 
                      [-a | --alias <path>] [-o | --output <path>]
                      [-h | --help]
Options:
    -w  <path>  Path to the word file to convert
                - If not provided, asca will look for a file in the current directory
    -r  <path>  Path to the rule file to convert
                - If not provided, asca will look for a file in the current directory
    -a  <path>  Path to an optional alias file to convert.
    -o  <path>  The desired path of the output json file
                - If not provided, asca will create a file in the current directory
    -h          Print help


usage: asca conv json [-p | --path <path>]  [-r | --rules <path>]
                      [-w | --words <path>] [-a | --alias <path>]
                      [-h | --help]
Options:
    -p  <path>  Path to the Json file to convert
                - If not provided, asca will look for a file in the current directory
    -w  <path>  The desired path of the output word file
                - If not provided, asca will create a file in the current directory
    -a  <path>  The desired path of the output alias file, if applicable.
                - If not provided, asca will create a file in the current directory.
    -r  <path>  The desired path of the output rule file
                - If not provided, asca will create a file in the current directory
    -h          Print help


Usage: asca conv tag [TAG]  [-p | --path <path>] [-o | --output <path>] [-r | --recurse]
                            [-h | --help]
Arguments:
    [TAG]   The tag within the config file to be converted.
Options:
    -p  <path>  Path to the config file or the directory it is within.
                - If not provided, asca will look for a config in the current directory.
    -o  <path>  The desired path of the output rule file.
                - If not provided, asca will create a file in the current directory.
    -r          Follow a pipeline back to its root tag and generate a full linear rule history
                - Additional words added after the start of the pipeline will not be included
                - Note: Tags cannot have multiple piped inputs as there would be a split history.
    -h          Print help


Usage asca conv config [PATH] [-h | --help]

Arguments:
    [PATH]  Path to the config file or the directory it is within.
            - If not provided, asca will look for a config in the current directory.
Options:
    -h      Print help
```

## Shell Completions

ASCA provides completion script generation for Bash, Zsh, Fish, Powershell, & Elvish.

### Bash
``` bash
mkdir -p ~/.local/share/bash-completion/completions
asca --generate=bash > ~/.local/share/bash-completion/completions/asca
``` 
macOS with Homebrew:
```bash
mkdir -p $(brew --prefix)/etc/bash_completion.d
asca --generate=bash > $(brew --prefix)/etc/bash_completion.d/asca.bash-completion
```

### Zsh
```zsh
mkdir ~/.zfunc
asca --generate=zsh > ~/.zfunc/_asca
```
Add `fpath+=~/.zfunc` to your `~/.zshrc` before `compinit`
### Fish
```fish
asca --generate=fish > ~/.config/fish/completions/asca.fish
```

### PowerShell
```powershell
asca --generate=powershell >> $PROFILE.CurrentUserCurrentHost
```

### Elvish
```Elvish
asca --generate=elvish >> ~/.config/elvish/lib/completers.elv
```