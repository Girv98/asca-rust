use colored::Colorize;

use crate ::{
    alias ::{parser::AliasItem, AliasKind, AliasPosition, AliasToken, AliasTokenKind}, 
    parser::Item, 
    lexer ::{Position, Token, TokenKind}
};

use super::{get_feat_closest, ASCAError, Error, RuleGroup};

#[derive(Debug, Clone)]
pub enum WordSyntaxError {
    DiacriticBeforeSegment(String, usize),
    NoSegmentBeforeColon  (String, usize),
    UnknownChar           (String, usize),
    ToneTooBig            (String, usize),
    CouldNotParseEjective (String),
    CouldNotParse         (String),
    DiacriticDoesNotMeetPreReqsFeat(String, usize, String, bool),
    DiacriticDoesNotMeetPreReqsNode(String, usize, String, bool),
}

impl From<WordSyntaxError> for Error {
    fn from(e: WordSyntaxError) -> Self {
        Self::WordSyn(e)
    }
}

impl ASCAError for WordSyntaxError {
    fn get_error_message(&self) -> String {
        match self {
            Self::DiacriticBeforeSegment(..) => "Diacritic Before Segment".to_string(),
            Self::NoSegmentBeforeColon  (..) => "No Segment Before Colon".to_string(),
            Self::UnknownChar           (..) => "Unknown Char".to_string(),
            Self::ToneTooBig            (..) => "Tone cannot be more than 4 digits long".to_string(),
            Self::CouldNotParseEjective (..) => "Unable to parse word. If you meant to have an ejective, you must use ʼ".to_string(),
            Self::CouldNotParse         (..) => "Unable to parse word".to_string(),
            Self::DiacriticDoesNotMeetPreReqsFeat(txt, i, t, pos) |
            Self::DiacriticDoesNotMeetPreReqsNode(txt, i, t, pos) => {
                format!("Segment does not have prerequisite properties to have diacritic `{}`. Must be [{} {}]", txt.chars().nth(*i).unwrap_or_default(), if *pos { '+' } else { '-' },t)
            },
        }
    }

    fn format_word_error(&self, _: &[String]) -> String {
        const MARG: &str = "\n    |     ";
        let mut result = format!("{} {}", "Word Syntax Error:".bright_red().bold(), self.get_error_message().bold());
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

    fn format_rule_error(&self, _: &[RuleGroup]) -> String {
        unreachable!()
    }

    fn format_alias_error(&self, _: &[String], _: &[String]) -> String {
        unreachable!()
    }

}

type GroupNum = usize;
type LineNum = usize;
type Pos = usize;

#[derive(Debug, Clone)]
pub enum RuleSyntaxError {
    ExpectedAlphabetic(char, GroupNum, LineNum, Pos),
    ExpectedCharColon (char, GroupNum, LineNum, Pos),
    ExpectedCharArrow (char, GroupNum, LineNum, Pos),
    MalformedComment  (char, GroupNum, LineNum, Pos),
    UnknownCharacter  (char, GroupNum, LineNum, Pos),
    ExpectedCharDot   (char, GroupNum, LineNum, Pos),
    ExpectedNumber    (char, GroupNum, LineNum, Pos),
    ExpectedTokenFeature(Token),
    ExpectedRightBracket(Token),
    TooManyUnderlines   (Token),
    BadSyllableMatrix   (Token),
    ExpectedUnderline   (Token),
    ExpectedVariable    (Token),
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
    OutsideBrackets(GroupNum, LineNum, Pos),
    NestedBrackets (GroupNum, LineNum, Pos),
    WrongModTone   (GroupNum, LineNum, Pos),
    EmptyOutput    (GroupNum, LineNum, Pos),
    EmptyInput     (GroupNum, LineNum, Pos),
    EmptyEnv       (GroupNum, LineNum, Pos),
    InsertMetath(GroupNum, LineNum, Pos, Pos),
    InsertDelete(GroupNum, LineNum, Pos, Pos),
    TooManyWordBoundaries(Position),
    StuffBeforeWordBound (Position),
    StuffAfterWordBound  (Position),
    WordBoundLoc         (Position),
    OptLocError          (Position),
    EmptySet             (Position),
    UnknownEnbyFeature(String, Position),
    UnknownFeature    (String, Position),
    DiacriticDoesNotMeetPreReqsFeat(Position, Position, String, bool),
    DiacriticDoesNotMeetPreReqsNode(Position, Position, String, bool),
    UnexpectedDiacritic(Position, Position),
    UnbalancedRuleEnv(Vec<Item>),
    UnbalancedRuleIO (Vec<Vec<Item>>),
    UnexpectedEol(Token, char),
    OptMathError (Token, usize, usize),
}

impl From<RuleSyntaxError> for Error {
    fn from(e: RuleSyntaxError) -> Self {
        Self::RuleSyn(e)
    }
}

impl ASCAError for RuleSyntaxError {
    fn get_error_message(&self) -> String {
        match self {
            Self::ExpectedAlphabetic(c, g, l, pos) => format!("Expected ASCII character, but received '{c}' at {g}:{l}:{pos}'."),
            Self::ExpectedCharColon (c, g, l, pos) => format!("Expected ':', but received '{c}' at {g}:{l}:{pos}"),
            Self::ExpectedCharArrow (c, g, l, pos) => format!("Expected '->', but received -'{c}' at {g}:{l}:{pos}"),
            Self::MalformedComment  (c, g, l, pos) => format!("Malformed Comment: Expected ';;', but received ';{c}' at {g}:{l}:{pos}"),
            Self::UnknownCharacter  (c, g, l, pos) => format!("Unknown character {c} at '{g}:{l}:{pos}'."),
            Self::ExpectedCharDot   (c, g, l, pos) => format!("Expected '..', but received .'{c}' at {g}:{l}:{pos}"),
            Self::ExpectedNumber    (c, g, l, pos) => format!("Expected a number, but received '{c}' at {g}:{l}:{pos}"),
            Self::ExpectedTokenFeature(token) => format!("{} cannot be placed inside a matrix. An element inside `[]` must a distinctive feature", token.value),
            Self::ExpectedRightBracket(token) => format!("Expected ')', but received '{}'", token.value),
            Self::TooManyUnderlines   (_)     => "Cannot have multiple underlines in an environment".to_string(),
            Self::BadSyllableMatrix   (_)     => "A syllable can only have parameters stress and tone".to_string(),
            Self::ExpectedUnderline   (token) => format!("Expected '_', but received '{}'", token.value),
            Self::ExpectedVariable    (token) => format!("Expected number, but received {} ", token.value),
            Self::UnknownGrouping     (token) => format!("Unknown grouping '{}'. Known groupings are (C)onsonant, (O)bstruent, (S)onorant, (P)losive, (F)ricative, (L)iquid, (N)asal, (G)lide, and (V)owel", token.value),
            Self::ExpectedSegment     (token) => format!("Expected an IPA character, Primative or Matrix, but received '{}'", token.value),
            Self::ExpectedEndLine     (token) => format!("Expected end of line, received '{}'. Did you forget a '/' between the output and environment?", token.value),
            Self::ExpectedMatrix      (token) => format!("Expected '[', but received '{}'", if token.kind == TokenKind::Eol {"End Of Line"} else {&token.value}),
            Self::ExpectedArrow       (token) => format!("Expected '>', '->' or '=>', but received '{}'", token.value),
            Self::ExpectedComma       (token) => format!("Expected ',', but received '{}'", token.value),
            Self::ExpectedColon       (token) => format!("Expected ':', but received '{}'", token.value),
            Self::ToneTooBig          (_)     => "A tone modifier cannot be more than 4 digits long".to_string(),
            Self::UnknownIPA          (token) => format!("Could not get value of IPA '{}'.", token.value),
            Self::InsertErr           (_)     => "The input of an insertion rule must only contain `*` or `∅`".to_string(),
            Self::DeleteErr           (_)     => "The output of a deletion rule must only contain `*` or `∅`".to_string(),
            Self::MetathErr           (_)     => "The output of a methathis rule must only contain `&`".to_string(),
            Self::OutsideBrackets(..) => "Features must be inside square brackets".to_string(),
            Self::NestedBrackets (..) => "Cannot have nested brackets of the same type".to_string(),
            Self::WrongModTone   (..) => "Tones cannot be ±; they can only be used with numeric values.".to_string(),
            Self::EmptyOutput    (..) => "Output cannot be empty. Use `*` or '∅' to indicate deletion".to_string(),
            Self::EmptyInput     (..) => "Input cannot be empty. Use `*` or '∅' to indicate insertion".to_string(),
            Self::EmptyEnv       (..) => "Environment cannot be empty following a seperator.".to_string(),
            Self::InsertMetath (..) => "A rule cannot be both an Insertion rule and a Metathesis rule".to_string(),
            Self::InsertDelete (..) => "A rule cannot be both an Insertion rule and a Deletion rule".to_string(),
            Self::TooManyWordBoundaries(_) => "Cannot have multiple word boundaries on each side of an environment".to_string(),
            Self::StuffBeforeWordBound (_) => "Can't have segments before the beginning of a word".to_string(),
            Self::StuffAfterWordBound  (_) => "Can't have segments after the end of a word".to_string(),
            Self::WordBoundLoc         (_) => "Wordboundaries are not allowed in the input or output".to_string(),
            Self::OptLocError          (_) => "Optionals can only be used in environments".to_string(),
            Self::EmptySet             (_) => "Sets cannot be empty".to_string(),
            Self::UnknownEnbyFeature(feat, pos) => format!("Feature '{feat}' has no modifier at {}:{}-{}'.", pos.line, pos.start, pos.end),
            Self::UnknownFeature    (feat, pos) => format!("Unknown feature '{feat}' at {}:{}-{}'. Did you mean {}? ", pos.line, pos.start, pos.end, get_feat_closest(feat)),
            Self::DiacriticDoesNotMeetPreReqsFeat(.., t , pos) |
            Self::DiacriticDoesNotMeetPreReqsNode(.., t , pos) => {
                format!("Segment does not have prerequisite properties to have this diacritic. Must be +[{}{}]", if *pos { '+' } else { '-' },t) 
            },
            Self::UnexpectedDiacritic(..) => "Diacritics can only modify IPA Segments".to_string(),
            Self::UnbalancedRuleEnv(_) => "Environment has too few elements".to_string(),
            Self::UnbalancedRuleIO (_) => "Input or Output has too few elements".to_string(),
            Self::UnexpectedEol(_, c) => format!("Expected `{c}`, but received End of Line"),
            Self::OptMathError (_, lo, hi) => format!("An Option's second argument '{hi}' must be greater than or equal to it's first argument '{lo}'"),
            
            
        }
    }

    fn format_rule_error(&self, rules: &[RuleGroup]) -> String {
        const MARG: &str = "\n    |     ";
        let mut result = format!("{} {}", "Syntax Error:".bright_red().bold(), self.get_error_message().bold()); 

        let (arrows, group, line) = match self {
            Self::UnexpectedEol       (t, ..) | 
            Self::OptMathError        (t, ..) | 
            Self::ExpectedTokenFeature(t) | 
            Self::ExpectedRightBracket(t) |
            Self::TooManyUnderlines   (t) | 
            Self::ExpectedUnderline   (t) | 
            Self::ExpectedVariable    (t) | 
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
            Self::StuffAfterWordBound(pos) => (
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

    fn format_word_error(&self, _: &[String]) -> String {
        unreachable!()
    }

    fn format_alias_error(&self, _: &[String], _: &[String]) -> String {
        unreachable!()
    }
}

#[derive(Debug, Clone)]
pub enum AliasSyntaxError {
    InvalidUnicodeEscape(String, AliasKind, LineNum, Pos),
    InvalidNamedEscape  (String, AliasKind, LineNum, Pos),
    ExpectedAlphabetic  (char, AliasKind, LineNum, Pos),
    ExpectedRightCurly  (char, AliasKind, LineNum, Pos),
    ExpectedCharArrow   (char, AliasKind, LineNum, Pos),
    ExpectedCharColon   (char, AliasKind, LineNum, Pos),
    ExpectedLeftCurly   (char, AliasKind, LineNum, Pos),
    UnknownEscapeChar   (char, AliasKind, LineNum, Pos),
    UnknownCharacter    (char, AliasKind, LineNum, Pos),
    ExpectedNumber      (char, AliasKind, LineNum, Pos),
    EmptyReplacements   (AliasKind, LineNum, Pos),
    OutsideBrackets     (AliasKind, LineNum, Pos),
    NestedBrackets      (AliasKind, LineNum, Pos),
    WrongModTone        (AliasKind, LineNum, Pos),
    EmptyInput          (AliasKind, LineNum, Pos),
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

impl From<AliasSyntaxError> for Error {
    fn from(e: AliasSyntaxError) -> Self {
        Self::AliasSyn(e)
    }
}

impl ASCAError for AliasSyntaxError {
    fn get_error_message(&self) -> String {
        match self {
            Self::InvalidUnicodeEscape(st, kind, ln, pos) => format!("Malformed unicode escape, '\\u{{{st}}}' is not valid @ '{kind}:{ln}:{pos}'."),
            Self::InvalidNamedEscape  (st, kind, ln, pos) => format!("Malformed named escape, '@{{{st}}}' is not valid @ '{kind}:{ln}:{pos}'."),
            Self::ExpectedAlphabetic  (ch, kind, ln, pos) => format!("Expected alphabetic character, but received '{ch}' @ '{kind}:{ln}:{pos}'."),
            Self::ExpectedRightCurly  (ch, kind, ln, pos) => format!("Expected }}, but received '{ch}' @ '{kind}:{ln}:{pos}'."),
            Self::ExpectedCharArrow   (ch, kind, ln, pos) => format!("Expected '->', but received -'{ch}' @ '{kind}:{ln}:{pos}'."),
            Self::ExpectedCharColon   (ch, kind, ln, pos) => format!("Expected ':', but received '{ch}' @ '{kind}:{ln}:{pos}'."),
            Self::ExpectedLeftCurly   (ch, kind, ln, pos) => format!("Expected {{, but received '{ch}' @ '{kind}:{ln}:{pos}'."),
            Self::UnknownEscapeChar   (ch, kind, ln, pos) => format!("Unknown escape '\\{ch}' @ '{kind}:{ln}:{pos}'."),
            Self::UnknownCharacter    (ch, kind, ln, pos) => format!("Unknown character {ch} @ '{kind}:{ln}:{pos}'."),
            Self::ExpectedNumber      (ch, kind, ln, pos) => format!("Expected a number, but received {ch} @ '{kind}:{ln}:{pos}'."),
            Self::EmptyReplacements   (..) => "Replacements cannot be empty".to_string(),
            Self::OutsideBrackets     (..) => "Features must be inside square brackets".to_string(),
            Self::NestedBrackets      (..) => "Cannot have nested brackets of the same type".to_string(),
            Self::WrongModTone        (..) => "Tones cannot be ±; they can only be used with numeric values.".to_string(),
            Self::EmptyInput          (..) => "Alias input cannot be empty.".to_string(),
            Self::UnknownEnbyFeature  (feat, pos) => format!("Feature '{feat}' has no modifier @ {}.", pos),
            Self::UnknownFeature      (feat, pos) => format!("Unknown feature '{feat}' @ {}'. Did you mean {}? ", pos, get_feat_closest(feat)),
            Self::ExpectedTokenFeature(token) => format!("{} cannot be placed inside a matrix. An element inside `[]` must a distinctive feature. @ {}", token.value, token.position),
            Self::ExpectedEndLine     (token) => format!("Expected end of line, received '{}' @ {}.", token.value, token.position),
            Self::ExpectedMatrix      (token) => format!("Expected '[', but received '{}' @ {}.", if token.kind == AliasTokenKind::Eol {"End Of Line"} else {&token.value}, token.position),
            Self::ExpectedArrow       (token) => format!("Expected '>', '->' or '=>', but received '{}' @ {}.", token.value, token.position),
            Self::UnknownGroup        (token) => format!("Unknown grouping '{}'. Known groupings are (C)onsonant, (O)bstruent, (S)onorant, (P)losive, (F)ricative, (L)iquid, (N)asal, (G)lide, and (V)owel @ {}.", token.value, token.position),
            Self::UnknownIPA          (token) => format!("Could not get value of IPA '{}' @ {}.", token.value, token.position),
            Self::DiacriticDoesNotMeetPreReqsFeat(.., t, pos) |
            Self::DiacriticDoesNotMeetPreReqsNode(.., t, pos) => {
                format!("Segment does not have prerequisite properties to have this diacritic. Must be [{}{}]", if *pos { '+' } else { '-' }, t) 
            },
            Self::UnexpectedEol(token, ch) => format!("Expected `{ch}`, but received End of Line @ {}", token.position),
            Self::UnbalancedIO(_) => "Input or Output has too few elements ".to_string(),
            Self::PlusInDerom(_) => "Deromaniser rules currently do not support addition".to_string(),
        }
    }

    fn format_alias_error(&self, into: &[String], from: &[String]) -> String {
        const MARG: &str = "\n    |     ";
        let mut result = format!("{} {}", "Syntax Error:".bright_red().bold(), self.get_error_message().bold()); 

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

        match kind {
            AliasKind::Deromaniser => {
                result.push_str(&format!("{}{}{}{}    {} deromaniser, line {}",  
                    MARG.bright_cyan().bold(),
                    into[line],
                    MARG.bright_cyan().bold(),
                    arrows.bright_red().bold(),
                    "@".bright_cyan().bold(),
                    line+1,
                ));
            },
            AliasKind::Romaniser => {
                result.push_str(&format!("{}{}{}{}    {} romaniser, line {}",  
                    MARG.bright_cyan().bold(),
                    from[line],
                    MARG.bright_cyan().bold(),
                    arrows.bright_red().bold(),
                    "@".bright_cyan().bold(),
                    line+1,
                ));
            },
        }

        result
    }

    fn format_word_error(&self, _: &[String]) -> String {
        unreachable!()
    }

    fn format_rule_error(&self, _: &[RuleGroup]) -> String {
        unreachable!()
    }
}