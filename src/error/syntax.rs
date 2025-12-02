use std::fmt;
use colored::Colorize;

use crate :: {
    alias :: { AliasKind, AliasPosition, AliasToken, AliasTokenKind, parser::AliasItem }, 
    rule  :: { EnvItem, ParseItem, Position, Token, TokenKind }
};
use super::{get_feat_closest, ASCAError, RuleGroup};

type WordString = String;
type GroupIndex = usize;
type LineIndex = usize;
type PosIndex = usize;
type IsPlus = bool;
type FeatString = String;
type NodeString = String;

#[derive(Debug, Clone)]
pub enum WordSyntaxError {
    DiacriticBeforeSegment(WordString, PosIndex),
    NoSegmentBeforeColon  (WordString, PosIndex),
    UnknownChar           (WordString, PosIndex),
    ToneTooBig            (WordString, PosIndex),
    CouldNotParseEjective (WordString),
    CouldNotParse         (WordString),
    DiacriticDoesNotMeetPreReqsFeat(WordString, PosIndex, FeatString, IsPlus),
    DiacriticDoesNotMeetPreReqsNode(WordString, PosIndex, NodeString, IsPlus),
}

impl From<WordSyntaxError> for ASCAError {
    fn from(e: WordSyntaxError) -> Self {
        Self::WordSyn(e)
    }
}

impl From<&WordSyntaxError> for ASCAError {
    fn from(e: &WordSyntaxError) -> Self {
        Self::WordSyn(e.clone())
    }
}

impl fmt::Display for WordSyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WordSyntaxError::DiacriticBeforeSegment(..) => write!(f, "Diacritic Before Segment"),
            WordSyntaxError::NoSegmentBeforeColon  (..) => write!(f, "No Segment Before Colon"),
            WordSyntaxError::UnknownChar           (..) => write!(f, "Unknown Char"),
            WordSyntaxError::ToneTooBig            (..) => write!(f, "Tone cannot be more than 4 digits long"),
            WordSyntaxError::CouldNotParseEjective (..) => write!(f, "Unable to parse word. If you meant to have an ejective, you must use ʼ"),
            WordSyntaxError::CouldNotParse         (..) => write!(f, "Unable to parse word"),
            WordSyntaxError::DiacriticDoesNotMeetPreReqsFeat(txt, i, t, pos) |
            WordSyntaxError::DiacriticDoesNotMeetPreReqsNode(txt, i, t, pos) => {
                write!(f, "Segment does not have prerequisite properties to have diacritic `{}`. Must be [{} {}]", txt.chars().nth(*i).unwrap_or_default(), if *pos { '+' } else { '-' },t)
            },
        }
    }
}

impl WordSyntaxError {
    pub fn format(&self) -> String {
        const MARG: &str = "\n    |     ";
        let mut result = format!("{} {}", "Word Syntax Error:".bright_red().bold(), self.to_string().bold());
        let (arrows, text) = match self {
            Self::CouldNotParse(text) => (
                "^".repeat(text.chars().count()) + "\n", 
                text
            ),
            Self::CouldNotParseEjective(text) => (
                " ".repeat(text.chars().count() - 1) + "^\n",
                text
            ),
            Self::DiacriticDoesNotMeetPreReqsFeat(text, i, ..) |
            Self::DiacriticDoesNotMeetPreReqsNode(text, i, ..) |
            Self::DiacriticBeforeSegment         (text, i    ) |
            Self::NoSegmentBeforeColon           (text, i    ) |
            Self::UnknownChar                    (text, i    ) |
            Self::ToneTooBig                     (text, i    ) => (
                " ".repeat(*i) + "^" + "\n", 
                text
            ),
        };
        result.push_str(&format!("{}{}{}{}",  
            MARG.bright_cyan().bold(), 
            text, 
            MARG.bright_cyan().bold(), 
            arrows.bright_red().bold()
        ));

        result
    }
}

#[derive(Debug, Clone)]
pub enum RuleSyntaxError {
    ExpectedAlphabetic(char, GroupIndex, LineIndex, PosIndex),
    ExpectedCharColon (char, GroupIndex, LineIndex, PosIndex),
    ExpectedCharArrow (char, GroupIndex, LineIndex, PosIndex),
    MalformedComment  (char, GroupIndex, LineIndex, PosIndex),
    UnknownCharacter  (char, GroupIndex, LineIndex, PosIndex),
    ExpectedCharDot   (char, GroupIndex, LineIndex, PosIndex),
    ExpectedNumber    (char, GroupIndex, LineIndex, PosIndex),
    ExpectedTokenFeature(Token),
    ExpectedRightBracket(Token),
    ExpectedStructElem  (Token),
    TooManyUnderlines   (Token),
    BadSyllableMatrix   (Token),
    ExpectedUnderline   (Token),
    ExpectedReference   (Token),
    UnknownGrouping     (Token),
    ExpectedSegment     (Token),
    ExpectedEndLine     (Token),
    ExpectedMatrix      (Token),
    ExpectedArrow       (Token),
    ExpectedComma       (Token),
    ExpectedColon       (Token),
    ToneTooBig          (Token),
    UnknownIPA          (Token),
    InsertErr           (Token),
    DeleteErr           (Token),
    MetathErr           (Token),
    IPARef              (Token),
    OutsideBrackets(GroupIndex, LineIndex, PosIndex),
    NestedBrackets (GroupIndex, LineIndex, PosIndex),
    WrongModTone   (GroupIndex, LineIndex, PosIndex),
    EmptyOutput    (GroupIndex, LineIndex, PosIndex),
    EmptyInput     (GroupIndex, LineIndex, PosIndex),
    EmptyEnv       (GroupIndex, LineIndex, PosIndex),
    InsertMetath(GroupIndex, LineIndex, PosIndex, PosIndex),
    InsertDelete(GroupIndex, LineIndex, PosIndex, PosIndex),
    TooManyWordBoundaries(Position),
    StuffBeforeWordBound (Position),
    StuffAfterWordBound  (Position),
    FloatingDiacritic    (Position),
    WordBoundLoc         (Position),
    OptLocError          (Position),
    EmptySet             (Position),
    UnknownEnbyFeature(String, Position),
    UnknownFeature    (String, Position),
    DiacriticDoesNotMeetPreReqsFeat(Position, Position, FeatString, IsPlus),
    DiacriticDoesNotMeetPreReqsNode(Position, Position, NodeString, IsPlus),
    UnexpectedDiacritic(Position, Position),
    UnbalancedRuleEnv(Vec<EnvItem>),
    UnbalancedRuleIO (Vec<Vec<ParseItem>>),
    UnexpectedEol(Token, char),
    OptMathError (Token, usize, usize),
}

impl From<RuleSyntaxError> for ASCAError {
    fn from(e: RuleSyntaxError) -> Self {
        Self::RuleSyn(e)
    }
}

impl fmt::Display for RuleSyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ExpectedAlphabetic(c, g, l, pos) => write!(f, "Expected ASCII character, but received '{c}' at {g}:{l}:{pos}."),
            Self::ExpectedCharColon (c, g, l, pos) => write!(f, "Expected ':', but received '{c}' at {g}:{l}:{pos}"),
            Self::ExpectedCharArrow (c, g, l, pos) => write!(f, "Expected '->', but received -'{c}' at {g}:{l}:{pos}"),
            Self::MalformedComment  (c, g, l, pos) => write!(f, "Malformed Comment: Expected ';;', but received ';{c}' at {g}:{l}:{pos}"),
            Self::UnknownCharacter  (c, g, l, pos) => write!(f, "Unknown character {c} at '{g}:{l}:{pos}'."),
            Self::ExpectedCharDot   (c, g, l, pos) => write!(f, "Expected '..', but received .'{c}' at {g}:{l}:{pos}"),
            Self::ExpectedNumber    (c, g, l, pos) => write!(f, "Expected a number, but received '{c}' at {g}:{l}:{pos}"),
            Self::ExpectedTokenFeature(token) => write!(f, "{} cannot be placed inside a matrix. An element inside `[]` must a distinctive feature", token.value),
            Self::ExpectedRightBracket(token) => write!(f, "Expected ')', but received '{}'", token.value),
            Self::ExpectedStructElem  (token) => write!(f, "Expected a Segment, Set, Option, or Ellipsis, but received '{}'", token.value),
            Self::TooManyUnderlines   (_)     => write!(f, "Cannot have multiple underlines in an environment"),
            Self::BadSyllableMatrix   (_)     => write!(f, "A syllable can only have parameters stress and tone"),
            Self::ExpectedUnderline   (token) => write!(f, "Expected '_', but received '{}'", token.value),
            Self::ExpectedReference   (token) => write!(f, "Expected number, but received {} ", token.value),
            Self::UnknownGrouping     (token) => write!(f, "Unknown grouping '{}'. Known groupings are (C)onsonant, (O)bstruent, (S)onorant, (P)losive, (F)ricative, (L)iquid, (N)asal, (G)lide, and (V)owel", token.value),
            Self::ExpectedSegment     (token) => write!(f, "Expected an IPA character, Primative or Matrix, but received '{}'", token.value),
            Self::ExpectedEndLine     (token) => write!(f, "Expected end of line, received '{}'. Did you forget a '/' between the output and environment?", token.value),
            Self::ExpectedMatrix      (token) => write!(f, "Expected '[', but received '{}'", if token.kind == TokenKind::Eol {"End Of Line"} else {&token.value}),
            Self::ExpectedArrow       (token) => write!(f, "Expected '>', '->' or '=>', but received '{}'", token.value),
            Self::ExpectedComma       (token) => write!(f, "Expected ',', but received '{}'", token.value),
            Self::ExpectedColon       (token) => write!(f, "Expected ':', but received '{}'", token.value),
            Self::ToneTooBig          (_)     => write!(f, "A tone modifier cannot be more than 4 digits long"),
            Self::UnknownIPA          (token) => write!(f, "Could not get value of IPA '{}'.", token.value),
            Self::InsertErr           (_)     => write!(f, "The input of an insertion rule must only contain `*` or `∅`"),
            Self::DeleteErr           (_)     => write!(f, "The output of a deletion rule must only contain `*` or `∅`"),
            Self::MetathErr           (_)     => write!(f, "The output of a methathis rule must only contain `&`"),
            Self::IPARef              (_)     => write!(f, "IPA Literals cannot be assigned to a reference"),
            Self::OutsideBrackets(..) => write!(f, "Features must be inside square brackets"),
            Self::NestedBrackets (..) => write!(f, "Cannot have nested brackets of the same type"),
            Self::WrongModTone   (..) => write!(f, "Tones cannot be ±; they can only be used with numeric values."),
            Self::EmptyOutput    (..) => write!(f, "Output cannot be empty. Use `*` or '∅' to indicate deletion"),
            Self::EmptyInput     (..) => write!(f, "Input cannot be empty. Use `*` or '∅' to indicate insertion"),
            Self::EmptyEnv       (..) => write!(f, "Environment cannot be empty following a seperator."),
            Self::InsertMetath (..) => write!(f, "A rule cannot be both an Insertion rule and a Metathesis rule"),
            Self::InsertDelete (..) => write!(f, "A rule cannot be both an Insertion rule and a Deletion rule"),
            Self::TooManyWordBoundaries(_) => write!(f, "Cannot have multiple word boundaries on each side of an environment"),
            Self::StuffBeforeWordBound (_) => write!(f, "Can't have segments before the beginning of a word"),
            Self::StuffAfterWordBound  (_) => write!(f, "Can't have segments after the end of a word"),
            Self::FloatingDiacritic    (_) => write!(f, "Floating diacritic. Diacritics can only be used to modify IPA Segments"),
            Self::WordBoundLoc         (_) => write!(f, "Wordboundaries are not allowed in the input or output"),
            Self::OptLocError          (_) => write!(f, "Options can only be used in Environments or Structures"),
            Self::EmptySet             (_) => write!(f, "Sets cannot be empty"),
            Self::UnknownEnbyFeature(feat, pos) => write!(f, "Feature '{feat}' has no modifier at {}:{}-{}.", pos.line, pos.start, pos.end),
            Self::UnknownFeature    (feat, pos) => write!(f, "Unknown feature '{feat}' at {}:{}-{}. Did you mean {}? ", pos.line, pos.start, pos.end, get_feat_closest(feat)),
            Self::DiacriticDoesNotMeetPreReqsFeat(.., t , pos) |
            Self::DiacriticDoesNotMeetPreReqsNode(.., t , pos) => {
                write!(f, "Segment does not have prerequisite properties to have this diacritic. Must be [{}{}]", if *pos { '+' } else { '-' },t) 
            },
            Self::UnexpectedDiacritic(..) => write!(f, "Diacritics can only be used to modify IPA Segments"),
            Self::UnbalancedRuleEnv(_) => write!(f, "Environment has too few elements"),
            Self::UnbalancedRuleIO (_) => write!(f, "Input or Output has too few elements"),
            Self::UnexpectedEol(_, c) => write!(f, "Expected `{c}`, but received End of Line"),
            Self::OptMathError (_, lo, hi) => write!(f, "An Option's second argument '{hi}' must be greater than or equal to it's first argument '{lo}'"),
        }
    }
}

impl RuleSyntaxError {
    pub fn format(&self, rules: &[RuleGroup]) -> String {
        const MARG: &str = "\n    |     ";
        let mut result = format!("{} {}", "Syntax Error:".bright_red().bold(), self.to_string().bold()); 

        let (arrows, group, line) = match self {
            Self::UnexpectedEol       (t, ..) | 
            Self::OptMathError        (t, ..) | 
            Self::ExpectedTokenFeature(t) | 
            Self::ExpectedRightBracket(t) |
            Self::ExpectedStructElem  (t) | 
            Self::TooManyUnderlines   (t) | 
            Self::ExpectedUnderline   (t) | 
            Self::ExpectedReference   (t) | 
            Self::UnknownGrouping     (t) | 
            Self::ExpectedSegment     (t) | 
            Self::ExpectedEndLine     (t) | 
            Self::ExpectedMatrix      (t) | 
            Self::ExpectedArrow       (t) | 
            Self::ExpectedComma       (t) | 
            Self::ExpectedColon       (t) | 
            Self::ToneTooBig          (t) | 
            Self::UnknownIPA          (t) | 
            Self::InsertErr           (t) | 
            Self::DeleteErr           (t) | 
            Self::MetathErr           (t) | 
            Self::IPARef              (t) | 
            Self::BadSyllableMatrix   (t) => (
                " ".repeat(t.position.start) + &"^".repeat(t.position.end-t.position.start) + "\n", 
                t.position.group,
                t.position.line
            ),
            Self::UnknownFeature(_, pos) | Self::UnknownEnbyFeature(_, pos) => (
                " ".repeat(pos.start) + &"^".repeat(pos.end-pos.start) + "\n", 
                pos.group,
                pos.line
            ),
            Self::ExpectedAlphabetic(_, group, line, pos) |
            Self::ExpectedCharArrow (_, group, line, pos) |
            Self::ExpectedCharColon (_, group, line, pos) |
            Self::MalformedComment  (_, group, line, pos) |
            Self::UnknownCharacter  (_, group, line, pos) |
            Self::ExpectedCharDot   (_, group, line, pos) |
            Self::ExpectedNumber    (_, group, line, pos) |
            Self::OutsideBrackets      (group, line, pos) |
            Self::NestedBrackets       (group, line, pos) | 
            Self::WrongModTone         (group, line, pos) |
            Self::EmptyOutput          (group, line, pos) |
            Self::EmptyInput           (group, line, pos) | 
            Self::EmptyEnv             (group, line, pos) => (
                " ".repeat(*pos) + "^" + "\n", 
                *group,
                *line
            ),
            Self::InsertDelete(group, line, pos1, pos2) | 
            Self::InsertMetath(group, line, pos1, pos2) => (
                " ".repeat(*pos1) + "^" + " ".repeat(pos2 - pos1 - 1).as_str() + "^" + "\n", 
                *group,
                *line
            ),
            Self::TooManyWordBoundaries(pos) |
            Self::StuffBeforeWordBound(pos)  | 
            Self::StuffAfterWordBound(pos)   | 
            Self::FloatingDiacritic(pos)     => (
                " ".repeat(pos.start) + "^" + "\n", 
                pos.group,
                pos.line
            ),
            Self::UnbalancedRuleEnv(items) => {
                let first_item = items.first().expect("Env should not be empty");
                let last_item = items.last().expect("Env should not be empty");
                let start = first_item.position.start;
                let end = last_item.position.end;
                (
                    " ".repeat(start) + &"^".repeat(end-start) + "\n", 
                    first_item.position.group,
                    first_item.position.line
                )
            },
            Self::WordBoundLoc(pos) |
            Self::OptLocError (pos) |
            Self::EmptySet    (pos) => (
                " ".repeat(pos.start) + &"^".repeat(pos.end-pos.start) + "\n",
                pos.group,
                pos.line
            ),
            Self::UnbalancedRuleIO(items) => {
                let first_item = items.first().expect("IO should not be empty").first().expect("IO should not be empty");
                let last_item = items.last().expect("IO should not be empty").last().expect("IO should not be empty");
                let start = first_item.position.start;
                let end = last_item.position.end;
                (
                    " ".repeat(start) + &"^".repeat(end-start) + "\n", 
                    first_item.position.group,
                    first_item.position.line
                )
            },
            Self::UnexpectedDiacritic(elm_pos, dia_pos) | 
            Self::DiacriticDoesNotMeetPreReqsFeat(elm_pos, dia_pos, ..) | 
            Self::DiacriticDoesNotMeetPreReqsNode(elm_pos, dia_pos, ..) => (
                " ".repeat(elm_pos.start) 
                    + &"^".repeat(elm_pos.end - elm_pos.start)
                    + &" ".repeat(dia_pos.start - elm_pos.end)
                    + &"^".repeat(dia_pos.end - dia_pos.start)
                    + "\n", 
                elm_pos.group,
                elm_pos.line
            ),
        };

        result.push_str(&format!("{}{}{}{}    {} Rule {}, Line {}",  
            MARG.bright_cyan().bold(), 
            rules[group].rule[line],
            MARG.bright_cyan().bold(), 
            arrows.bright_red().bold(),
            "@".bright_cyan().bold(),
            group+1,
            line+1,
        ));

        result
    }
}

#[derive(Debug, Clone)]
pub enum AliasSyntaxError {
    InvalidUnicodeEscape(String, AliasKind, LineIndex, PosIndex),
    InvalidNamedEscape  (String, AliasKind, LineIndex, PosIndex),
    ExpectedAlphabetic  (char, AliasKind, LineIndex, PosIndex),
    ExpectedRightCurly  (char, AliasKind, LineIndex, PosIndex),
    ExpectedCharArrow   (char, AliasKind, LineIndex, PosIndex),
    ExpectedCharColon   (char, AliasKind, LineIndex, PosIndex),
    ExpectedLeftCurly   (char, AliasKind, LineIndex, PosIndex),
    UnknownEscapeChar   (char, AliasKind, LineIndex, PosIndex),
    UnknownCharacter    (char, AliasKind, LineIndex, PosIndex),
    ExpectedNumber      (char, AliasKind, LineIndex, PosIndex),
    EmptyReplacements   (AliasKind, LineIndex, PosIndex),
    OutsideBrackets     (AliasKind, LineIndex, PosIndex),
    NestedBrackets      (AliasKind, LineIndex, PosIndex),
    WrongModTone        (AliasKind, LineIndex, PosIndex),
    EmptyOutput         (AliasKind, LineIndex, PosIndex),
    EmptyInput          (AliasKind, LineIndex, PosIndex),
    UnknownEnbyFeature  (String, AliasPosition),
    UnknownFeature      (String, AliasPosition),
    ExpectedTokenFeature(AliasToken),
    ExpectedEndLine     (AliasToken),
    ExpectedMatrix      (AliasToken),
    ExpectedArrow       (AliasToken),
    UnknownGroup        (AliasToken),
    UnknownIPA          (AliasToken),
    DiacriticDoesNotMeetPreReqsFeat(AliasPosition, AliasPosition, String, bool),
    DiacriticDoesNotMeetPreReqsNode(AliasPosition, AliasPosition, String, bool),
    UnexpectedEol(AliasToken, char),
    UnbalancedIO(Vec<AliasItem>),
    PlusInDerom(AliasPosition),
}

impl From<AliasSyntaxError> for ASCAError {
    fn from(e: AliasSyntaxError) -> Self {
        Self::AliasSyn(e)
    }
}

impl fmt::Display for AliasSyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidUnicodeEscape(st, kind, ln, pos) => write!(f, "Malformed unicode escape, '\\u{{{st}}}' is not valid @ '{kind}:{ln}:{pos}'."),
            Self::InvalidNamedEscape  (st, kind, ln, pos) => write!(f, "Malformed named escape, '@{{{st}}}' is not valid @ '{kind}:{ln}:{pos}'."),
            Self::ExpectedAlphabetic  (ch, kind, ln, pos) => write!(f, "Expected alphabetic character, but received '{ch}' @ '{kind}:{ln}:{pos}'."),
            Self::ExpectedRightCurly  (ch, kind, ln, pos) => write!(f, "Expected }}, but received '{ch}' @ '{kind}:{ln}:{pos}'."),
            Self::ExpectedCharArrow   (ch, kind, ln, pos) => write!(f, "Expected '->', but received -'{ch}' @ '{kind}:{ln}:{pos}'."),
            Self::ExpectedCharColon   (ch, kind, ln, pos) => write!(f, "Expected ':', but received '{ch}' @ '{kind}:{ln}:{pos}'."),
            Self::ExpectedLeftCurly   (ch, kind, ln, pos) => write!(f, "Expected {{, but received '{ch}' @ '{kind}:{ln}:{pos}'."),
            Self::UnknownEscapeChar   (ch, kind, ln, pos) => write!(f, "Unknown escape '\\{ch}' @ '{kind}:{ln}:{pos}'."),
            Self::UnknownCharacter    (ch, kind, ln, pos) => write!(f, "Unknown character {ch} @ '{kind}:{ln}:{pos}'."),
            Self::ExpectedNumber      (ch, kind, ln, pos) => write!(f, "Expected a number, but received {ch} @ '{kind}:{ln}:{pos}'."),
            Self::EmptyReplacements   (..) => write!(f, "Replacements cannot be empty"),
            Self::OutsideBrackets     (..) => write!(f, "Features must be inside square brackets"),
            Self::NestedBrackets      (..) => write!(f, "Cannot have nested brackets of the same type"),
            Self::WrongModTone        (..) => write!(f, "Tones cannot be ±; they can only be used with numeric values."),
            Self::EmptyOutput         (..) => write!(f, "Alias output cannot be empty."),
            Self::EmptyInput          (..) => write!(f, "Alias input cannot be empty."),
            Self::UnknownEnbyFeature  (feat, pos) => write!(f, "Feature '{feat}' has no modifier @ {pos}."),
            Self::UnknownFeature      (feat, pos) => write!(f, "Unknown feature '{feat}' @ {pos}'. Did you mean {}? ", get_feat_closest(feat)),
            Self::ExpectedTokenFeature(token) => write!(f, "{} cannot be placed inside a matrix. An element inside `[]` must a distinctive feature. @ {}", token.value, token.position),
            Self::ExpectedEndLine     (token) => write!(f, "Expected end of line, received '{}' @ {}.", token.value, token.position),
            Self::ExpectedMatrix      (token) => write!(f, "Expected '[', but received '{}' @ {}.", if token.kind == AliasTokenKind::Eol {"End Of Line"} else {&token.value}, token.position),
            Self::ExpectedArrow       (token) => write!(f, "Expected '>', '->' or '=>', but received '{}' @ {}.", token.value, token.position),
            Self::UnknownGroup        (token) => write!(f, "Unknown grouping '{}'. Known groupings are (C)onsonant, (O)bstruent, (S)onorant, (P)losive, (F)ricative, (L)iquid, (N)asal, (G)lide, and (V)owel @ {}.", token.value, token.position),
            Self::UnknownIPA          (token) => write!(f, "Could not get value of IPA '{}' @ {}.", token.value, token.position),
            Self::DiacriticDoesNotMeetPreReqsFeat(.., t, pos) |
            Self::DiacriticDoesNotMeetPreReqsNode(.., t, pos) => {
                write!(f, "Segment does not have prerequisite properties to have this diacritic. Must be [{}{}]", if *pos { '+' } else { '-' }, t) 
            },
            Self::UnexpectedEol(token, ch) => write!(f, "Expected `{ch}`, but received End of Line @ {}", token.position),
            Self::UnbalancedIO(_) => write!(f, "Input or Output has too few elements "),
            Self::PlusInDerom(_) => write!(f, "Deromaniser rules currently do not support addition"),
        }
    }
}

impl AliasSyntaxError {
    pub fn format(&self, into: &[String], from: &[String]) -> String {
        const MARG: &str = "\n    |     ";
        let mut result = format!("{} {}", "Syntax Error:".bright_red().bold(), self.to_string().bold()); 

        let (arrows, kind, line) = match self {
            Self::InvalidUnicodeEscape(_, kind, line, pos) |
            Self::InvalidNamedEscape  (_, kind, line, pos) |
            Self::ExpectedAlphabetic  (_, kind, line, pos) |
            Self::ExpectedRightCurly  (_, kind, line, pos) |
            Self::ExpectedCharArrow   (_, kind, line, pos) |
            Self::ExpectedCharColon   (_, kind, line, pos) |
            Self::ExpectedLeftCurly   (_, kind, line, pos) |
            Self::UnknownEscapeChar   (_, kind, line, pos) |
            Self::UnknownCharacter    (_, kind, line, pos) |
            Self::ExpectedNumber      (_, kind, line, pos) |
            Self::EmptyReplacements      (kind, line, pos) |
            Self::OutsideBrackets        (kind, line, pos) |
            Self::NestedBrackets         (kind, line, pos) |
            Self::WrongModTone           (kind, line, pos) |
            Self::EmptyOutput            (kind, line, pos) |
            Self::EmptyInput             (kind, line, pos) => (
                " ".repeat(*pos) + "^" + "\n", 
                *kind,
                *line,
            ),
            Self::PlusInDerom          (pos) |
            Self::UnknownFeature    (_, pos) |
            Self::UnknownEnbyFeature(_, pos) => (
                " ".repeat(pos.start) + &"^".repeat(pos.end-pos.start) + "\n", 
                pos.kind,
                pos.line,
            ),
            Self::ExpectedTokenFeature(token) |
            Self::ExpectedEndLine     (token) |
            Self::ExpectedMatrix      (token) |
            Self::ExpectedArrow       (token) |
            Self::UnknownGroup        (token) |
            Self::UnknownIPA          (token) |
            Self::UnexpectedEol       (token, _) => (
                " ".repeat(token.position.start) + &"^".repeat(token.position.end-token.position.start) + "\n", 
                token.position.kind,
                token.position.line
            ),
            Self::DiacriticDoesNotMeetPreReqsFeat(elm_pos, dia_pos, ..) |
            Self::DiacriticDoesNotMeetPreReqsNode(elm_pos, dia_pos, ..) => (
                " ".repeat(elm_pos.start) 
                    + &"^".repeat(elm_pos.end - elm_pos.start)
                    + &" ".repeat(dia_pos.start - elm_pos.end)
                    + &"^".repeat(dia_pos.end - dia_pos.start)
                    + "\n", 
                elm_pos.kind,
                elm_pos.line
            ),
            Self::UnbalancedIO(items) => {
                let first_item = items.first().expect("IO should not be empty");
                let last_item = items.last().expect("IO should not be empty");
                let start = first_item.position.start;
                let end = last_item.position.end;
                (
                    " ".repeat(start) + &"^".repeat(end-start) + "\n", 
                    first_item.position.kind,
                    first_item.position.line
                )
            },
        };

        let (knd, ln) = match kind {
            AliasKind::Deromaniser => ("deromaniser", &into[line]),
            AliasKind::Romaniser   => ("romaniser",   &from[line]),
        };

        result.push_str(&format!("{0}{ln}{0}{1}    {2} {knd}, line {3}",  
            MARG.bright_cyan().bold(),
            arrows.bright_red().bold(),
            "@".bright_cyan().bold(),
            line+1,
        ));

        result
    }
}