use colored::Colorize;
use crate  :: {
    alias  :: {parser::AliasItem, AliasKind, AliasPosition, AliasToken, AliasTokenKind}, 
    lexer  :: {Position, Token, TokenKind}, 
    parser :: Item, RuleGroup
};

pub trait ASCAError: Clone {
    fn get_error_message(&self) -> String;
    // Fixme: This really isn't the correct solution
    fn format_word_error(&self, _: &[String]) -> String;
    fn format_rule_error(&self, _: &[RuleGroup]) -> String;
    fn format_alias_error(&self, _: &[String], _: &[String]) -> String;
}

#[derive(Debug, Clone)]
pub enum Error {
    WordSyn(WordSyntaxError),
    RuleSyn(RuleSyntaxError),
    WordRun(WordRuntimeError),
    RuleRun(RuleRuntimeError),
    AliasSyn(AliasSyntaxError),
    AliasRun(AliasRuntimeError),
}

impl ASCAError for Error {
    fn get_error_message(&self) -> String {
        match self {
            Self::WordSyn(e) => e.get_error_message(),
            Self::RuleSyn(e) => e.get_error_message(),
            Self::WordRun(e) => e.get_error_message(),
            Self::RuleRun(e) => e.get_error_message(),
            Self::AliasSyn(e) => e.get_error_message(),
            Self::AliasRun(e) => e.get_error_message(),
        }
    }

    fn format_word_error(&self, s: &[String]) -> String {
        match self {
            Self::WordSyn(e) => e.format_word_error(s),
            Self::WordRun(e) => e.format_word_error(s),
            _ => unreachable!()
        }
    }

    fn format_rule_error(&self, s: &[RuleGroup]) -> String {
        match self {
            Self::RuleSyn(e) => e.format_rule_error(s),
            Self::RuleRun(e) => e.format_rule_error(s),
            _ => unreachable!(),
        }
    }

    fn format_alias_error(&self, into: &[String], from: &[String]) -> String {
        match self {
            Self::AliasSyn(e) => e.format_alias_error(into, from),
            Self::AliasRun(e) => e.format_alias_error(into, from),
            _ => unreachable!()
        }
    }
}

impl From<WordRuntimeError> for Error {
    fn from(e: WordRuntimeError) -> Self {
        Self::WordRun(e)
    }
}

impl From<RuleRuntimeError> for Error {
    fn from(e: RuleRuntimeError) -> Self {
        Self::RuleRun(e)
    }
}

impl From<WordSyntaxError> for Error {
    fn from(e: WordSyntaxError) -> Self {
        Self::WordSyn(e)
    }
}

impl From<RuleSyntaxError> for Error {
    fn from(e: RuleSyntaxError) -> Self {
        Self::RuleSyn(e)
    }
}

impl From<AliasSyntaxError> for Error {
    fn from(e: AliasSyntaxError) -> Self {
        Self::AliasSyn(e)
    }
}

impl From<AliasRuntimeError> for Error {
    fn from(e: AliasRuntimeError) -> Self {
        Self::AliasRun(e)
    }
}

#[derive(Debug, Clone)]
pub enum WordSyntaxError {
    UnknownChar(String, usize),
    NoSegmentBeforeColon(String, usize),
    DiacriticBeforeSegment(String, usize),
    DiacriticDoesNotMeetPreReqsFeat(String, usize, String, bool),
    DiacriticDoesNotMeetPreReqsNode(String, usize, String, bool),
    CouldNotParse(String),
    CouldNotParseEjective(String)
}

impl ASCAError for WordSyntaxError {
    fn get_error_message(&self) -> String {
        match self {
            Self::UnknownChar(_, _)                      => "Unknown Char".to_string(),
            Self::NoSegmentBeforeColon(_, _)             => "No Segment Before Colon".to_string(),
            Self::DiacriticBeforeSegment(_, _)           => "Diacritic Before Segment".to_string(),
            Self::CouldNotParse(_)                       => "Unable to parse word".to_string(),
            Self::CouldNotParseEjective(_)               => "Unable to parse word. If you meant to have an ejective, you must use ʼ".to_string(),
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
            Self::UnknownChar(text, i)            |
            Self::NoSegmentBeforeColon(text, i)   |
            Self::DiacriticBeforeSegment(text, i) |
            Self::DiacriticDoesNotMeetPreReqsFeat(text, i, ..) |
            Self::DiacriticDoesNotMeetPreReqsNode(text, i, ..) => (
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

#[derive(Debug, Clone)]
pub enum WordRuntimeError {
    UnknownSegment(String, usize,  usize), // (Segs before, Word Pos in list, Segment Pos in Words)
}

impl ASCAError for WordRuntimeError {
    fn get_error_message(&self) -> String {
        match self {
            Self::UnknownSegment(buf, ..) => format!("Unknown Segment `{}`", buf)
        }
    }

    fn format_word_error(&self, words: &[String]) -> String {
        const MARG: &str = "\n    |     ";
        let mut result = format!("{} {}", "Runtime Error:".bright_red().bold(), self.get_error_message().bold());

        match self {
            Self::UnknownSegment(buffer, word, seg) => {
                let arrows = " ".repeat(words[*word].len() + seg) + "^" + "\n";
                result.push_str(&format!("{}{} => {}{}{}",  
                    MARG.bright_cyan().bold(), 
                    words[*word], 
                    buffer,
                    MARG.bright_cyan().bold(), 
                    arrows.bright_red().bold()
                ));
            },
        }

        result
    }

    fn format_rule_error(&self, _: &[RuleGroup]) -> String {
        unreachable!()
    }

    fn format_alias_error(&self, _: &[String], _: &[String]) -> String {
        unreachable!()
    }
}

#[derive(Debug, Clone)]
pub enum RuleRuntimeError { 
    DeletionOnlySeg,
    DeletionOnlySyll,
    LonelySet(Position),
    UnevenSet(Position, Position),
    UnknownVariable(Token),
    InsertionNoContextOrException(Position),
    InsertionMatrix(Position),
    AlphaUnknown(Position),
    AlphaUnknownInv(Position),
    AlphaNodeAssignInv(Position),
    AlphaIsNotSameNode(Position),
    AlphaIsNotNode(Position),
    NodeCannotBeSome(String, Position),
    NodeCannotBeNone(String, Position),
    NodeCannotBeSet(String, Position),
    MetathSyllSegment(Position, Position),
    MetathSyllBoundary(Position, Position),
    SubstitutionSyll(Position),
    SubstitutionSyllBound(Position, Position),
    SubstitutionSylltoMatrix(Position, Position),
    SubstitutionSylltoBound(Position, Position),
    SubstitutionSegtoSyll(Position, Position),
    SubstitutionBoundMod(Position, Position),
    WordBoundSetLocError(Position),
    OverlongPosLongNeg(Position),
    SecStrPosStrNeg(Position),
}

impl ASCAError for RuleRuntimeError {
    fn get_error_message(&self) -> String {
        match self {
            Self::LonelySet(_)                     => "A Set in output must have a matching Set in input".to_string(),
            Self::UnevenSet(..)                    => "Two matched sets must have the same number of elements".to_string(),
            Self::DeletionOnlySeg                  => "Can't delete a word's only segment".to_string(),
            Self::DeletionOnlySyll                 => "Can't delete a word's only syllable".to_string(),
            Self::UnknownVariable(token)           => format!("Unknown variable '{}' at {}", token.value, token.position.start),
            Self::InsertionNoContextOrException(_) => "Insertion rules must have a context".to_string(),
            Self::InsertionMatrix(_)               => "An incomplete matrix cannot be inserted".to_string(),
            Self::AlphaUnknown(_)                  => "Alpha has not be assigned before applying".to_string(),
            Self::AlphaUnknownInv(_)               => "First occurence of a node alpha must not be inverted.".to_string(),
            Self::AlphaNodeAssignInv(_)            => "Node alphas cannot be assigned inverse. First occurrence of a node alpha must be positive.".to_string(),
            Self::AlphaIsNotSameNode(_)            => "Node alphas must only be used on the same node.".to_string(),
            Self::AlphaIsNotNode(_)                => "Node alphas cannot be used on binary features".to_string(),
            Self::NodeCannotBeSome(node, _)        => format!("{} node cannot arbitrarily positive", node),
            Self::NodeCannotBeNone(node, _)        => format!("{} node cannot be removed", node),
            Self::NodeCannotBeSet(node, _)         => format!("{} node cannot be assigned using PLACE alpha", node),
            Self::MetathSyllSegment(..)            => "Cannot swap a syllable with a segment".to_string(),
            Self::MetathSyllBoundary(..)           => "Cannot swap a syllable with a syllable".to_string(),
            Self::SubstitutionSyll(_)              => "Syllables cannot be in substitution output. If you wish to modify a syllable, use a matrix.".to_string(),
            Self::SubstitutionSyllBound(..)        => "Syllable boundaries cannot be substituted.".to_string(),
            Self::SubstitutionSylltoMatrix(..)     => "Syllables and boundaries cannot be substituted by a segment".to_string(),
            Self::SubstitutionSylltoBound(..)      => "Syllables cannot be substituted by a boundary".to_string(),
            Self::SubstitutionSegtoSyll(..)        => "Segments cannot be substituted by a syllable or a boundary".to_string(),
            Self::SubstitutionBoundMod(..)         => "Syllable boundaries cannot be modified by a matrix.".to_string(),
            Self::WordBoundSetLocError(_)          => "Word Boundaries cannot be in the input or output".to_string(),
            Self::OverlongPosLongNeg(_)            => "A segment cannot be both [+overlong] and [-long]".to_string(),
            Self::SecStrPosStrNeg(_)               => "A syllable cannot be both [+sec.stress] and [-stress]".to_string(),
        }
    }

    fn format_rule_error(&self, rules: &[RuleGroup]) -> String {
        const MARG: &str = "\n    |     ";
        let mut result = format!("{} {}", "Runtime Error:".bright_red().bold(), self.get_error_message().bold());
        
        let (arrows, group , line) =  match self {
            Self::DeletionOnlySyll | Self::DeletionOnlySeg => return result,
            Self::UnknownVariable(t) => (
                " ".repeat(t.position.start) + &"^".repeat(t.position.end-t.position.start) + "\n", 
                t.position.group,
                t.position.line
            ),
            Self::InsertionNoContextOrException(pos) => (
                " ".repeat(pos.end) + "^" + "\n", 
                pos.group,
                pos.line
            ),
            Self::WordBoundSetLocError(pos) |
            Self::SubstitutionSyll(pos)     |
            Self::AlphaUnknown(pos)         |
            Self::AlphaUnknownInv(pos)      |
            Self::AlphaNodeAssignInv(pos)   |
            Self::LonelySet(pos)            | 
            Self::NodeCannotBeSome(_, pos)  |
            Self::NodeCannotBeNone(_, pos)  |
            Self::NodeCannotBeSet(_, pos)   |
            Self::AlphaIsNotSameNode(pos)   |
            Self::AlphaIsNotNode(pos)       |
            Self::OverlongPosLongNeg(pos)   |
            Self::SecStrPosStrNeg(pos)      |
            Self::InsertionMatrix(pos)  =>  (
                " ".repeat(pos.start) + &"^".repeat(pos.end-pos.start) + "\n",
                pos.group,
                pos.line
            ),
            Self::UnevenSet (a, b) |
            Self::MetathSyllSegment (a, b) |
            Self::MetathSyllBoundary(a, b) |
            Self::SubstitutionSegtoSyll (a, b) |
            Self::SubstitutionSyllBound (a, b) |
            Self::SubstitutionSylltoBound (a, b) |
            Self::SubstitutionSylltoMatrix(a, b) |
            Self::SubstitutionBoundMod(a, b) => (
                   " ".repeat(a.start) + &"^".repeat(a.end - a.start) 
                + &" ".repeat(b.start) + &"^".repeat(b.end - b.start) + "\n",
                a.group,
                a.line
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

type GroupNum = usize;
type LineNum = usize;
type Pos = usize;

#[derive(Debug, Clone)]
pub enum RuleSyntaxError {
    OptLocError(Position),
    OptMathError(Token, usize, usize),
    UnknownIPA(Token),
    UnknownGrouping(Token),
    UnknownCharacter(char, GroupNum, LineNum, Pos),
    ExpectedCharColon(char, GroupNum, LineNum, Pos),
    ExpectedAlphabetic(char, GroupNum, LineNum, Pos),
    ExpectedCharArrow(char, GroupNum, LineNum, Pos),
    ExpectedCharDot(char, GroupNum, LineNum, Pos),
    ExpectedNumber(char, GroupNum, LineNum, Pos),
    TooManyUnderlines(Token),
    UnexpectedEol(Token, char),
    ExpectedEndL(Token),
    ExpectedArrow(Token),
    ExpectedComma(Token),
    ExpectedColon(Token),
    ExpectedMatrix(Token),
    ExpectedSegment(Token),
    ExpectedTokenFeature(Token),
    ExpectedVariable(Token),
    ExpectedUnderline(Token),
    ExpectedRightBracket(Token),
    BadSyllableMatrix(Token),
    WrongModTone(GroupNum, LineNum, Pos),
    OutsideBrackets(GroupNum, LineNum, Pos),
    NestedBrackets(GroupNum, LineNum, Pos),
    InsertErr(Token),
    DeleteErr(Token),
    MetathErr(Token),
    EmptyInput(GroupNum, LineNum, Pos),
    EmptyOutput(GroupNum, LineNum, Pos),
    EmptyEnv(GroupNum, LineNum, Pos),
    EmptySet(Position),
    InsertMetath(GroupNum, LineNum, Pos, Pos),
    InsertDelete(GroupNum, LineNum, Pos, Pos),
    StuffAfterWordBound(Position),
    StuffBeforeWordBound(Position),
    TooManyWordBoundaries(Position),
    UnknownFeature(String, Position),
    UnknownEnbyFeature(String, Position),
    UnbalancedRuleIO(Vec<Vec<Item>>),
    UnbalancedRuleEnv(Vec<Item>),
    DiacriticDoesNotMeetPreReqsFeat(Position, Position, String, bool),
    DiacriticDoesNotMeetPreReqsNode(Position, Position, String, bool),
    UnexpectedDiacritic(Position, Position),
    WordBoundLoc(Position),
}

impl ASCAError for RuleSyntaxError {
    fn get_error_message(&self) -> String {
        match self {
            Self::OptLocError(_)                  => "Optionals can only be used in environments".to_string(),
            Self::OptMathError(_, low, high)      => format!("An Option's second argument '{high}' must be greater than or equal to it's first argument '{low}'"),
            Self::UnknownIPA(token)               => format!("Could not get value of IPA '{}'.", token.value),
            Self::UnknownGrouping(token)          => format!("Unknown grouping '{}'. Known groupings are (C)onsonant, (O)bstruent, (S)onorant, (P)losive, (F)ricative, (L)iquid, (N)asal, (G)lide, and (V)owel", token.value),
            Self::UnknownFeature(feat, pos)       => format!("Unknown feature '{feat}' at {}:{}-{}'. Did you mean {}? ", pos.line, pos.start, pos.end, get_feat_closest(feat)),
            Self::UnknownEnbyFeature(feat, pos)   => format!("Feature '{feat}' has no modifier at {}:{}-{}'.", pos.line, pos.start, pos.end),
            Self::ExpectedAlphabetic(c, g, l, pos)=> format!("Expected ASCII character, but received '{c}' at {g}:{l}:{pos}'."),
            Self::ExpectedCharColon(c, g, l, pos) => format!("Expected ':', but received '{c}' at {g}:{l}:{pos}"),
            Self::ExpectedCharArrow(c, g, l, pos) => format!("Expected '->', but received -'{c}' at {g}:{l}:{pos}"),
            Self::ExpectedCharDot(c, g, l, pos)   => format!("Expected '..', but received .'{c}' at {g}:{l}:{pos}"),
            Self::ExpectedNumber(c, g, l, pos)    => format!("Expected a number, but received '{c}' at {g}:{l}:{pos}"),
            Self::UnknownCharacter(c, g, l, pos)  => format!("Unknown character {c} at '{g}:{l}:{pos}'."),
            Self::TooManyUnderlines(_)            => "Cannot have multiple underlines in an environment".to_string(),
            Self::StuffAfterWordBound(_)          => "Can't have segments after the end of a word".to_string(),
            Self::StuffBeforeWordBound(_)         => "Can't have segments before the beginning of a word".to_string(),
            Self::TooManyWordBoundaries(_)        => "Cannot have multiple word boundaries on each side of an environment".to_string(),
            Self::UnexpectedEol(_, c)             => format!("Expected `{c}`, but received End of Line"),
            Self::ExpectedEndL(token)             => format!("Expected end of line, received '{}'. Did you forget a '/' between the output and environment?", token.value),
            Self::ExpectedArrow(token)            => format!("Expected '>', '->' or '=>', but received '{}'", token.value),
            Self::ExpectedComma(token)            => format!("Expected ',', but received '{}'", token.value),
            Self::ExpectedColon(token)            => format!("Expected ':', but received '{}'", token.value),
            Self::ExpectedUnderline(token)        => format!("Expected '_', but received '{}'", token.value),
            Self::ExpectedRightBracket(token)     => format!("Expected ')', but received '{}'", token.value),
            Self::ExpectedMatrix(token)           => format!("Expected '[', but received '{}'", if token.kind == TokenKind::Eol {"End Of Line"} else {&token.value}),
            Self::ExpectedSegment(token)          => format!("Expected an IPA character, Primative or Matrix, but received '{}'", token.value),
            Self::ExpectedTokenFeature(token)     => format!("{} cannot be placed inside a matrix. An element inside `[]` must a distinctive feature", token.value),
            Self::ExpectedVariable(token)         => format!("Expected number, but received {} ", token.value),
            Self::BadSyllableMatrix(_)            => "A syllable can only have parameters stress and tone".to_string(),
            Self::WrongModTone(..)                => "Tones cannot be ±; they can only be used with numeric values.".to_string(),
            Self::NestedBrackets(..)              => "Cannot have nested brackets of the same type".to_string(),
            Self::OutsideBrackets(..)             => "Features must be inside square brackets".to_string(),
            Self::InsertErr(_)                    => "The input of an insertion rule must only contain `*` or `∅`".to_string(),
            Self::DeleteErr(_)                    => "The output of a deletion rule must only contain `*` or `∅`".to_string(),
            Self::MetathErr(_)                    => "The output of a methathis rule must only contain `&`".to_string(),
            Self::EmptyInput(..)                  => "Input cannot be empty. Use `*` or '∅' to indicate insertion".to_string(),
            Self::EmptyOutput(..)                 => "Output cannot be empty. Use `*` or '∅' to indicate deletion".to_string(),
            Self::EmptyEnv(..)                    => "Environment cannot be empty following a seperator.".to_string(),
            Self::EmptySet(..)                    => "Sets cannot be empty".to_string(),
            Self::InsertMetath(..)                => "A rule cannot be both an Insertion rule and a Metathesis rule".to_string(),
            Self::InsertDelete(..)                => "A rule cannot be both an Insertion rule and a Deletion rule".to_string(),
            Self::UnbalancedRuleIO(_)             => "Input or Output has too few elements".to_string(),
            Self::UnbalancedRuleEnv(_)            => "Environment has too few elements".to_string(),
            Self::DiacriticDoesNotMeetPreReqsFeat(.., t , pos) |
            Self::DiacriticDoesNotMeetPreReqsNode(.., t , pos) => {
                format!("Segment does not have prerequisite properties to have this diacritic. Must be +[{}{}]", if *pos { '+' } else { '-' },t) 
            },
            Self::UnexpectedDiacritic(..) => "Diacritics can only modify IPA Segments".to_string(),
            Self::WordBoundLoc(_)     => "Wordboundaries are not allowed in the input or output".to_string(),
        }
    }

    fn format_rule_error(&self, rules: &[RuleGroup]) -> String {
        const MARG: &str = "\n    |     ";
        let mut result = format!("{} {}", "Syntax Error:".bright_red().bold(), self.get_error_message().bold()); 

        let (arrows, group, line) = match self {
            Self::OptMathError(t, _, _)   | 
            Self::UnknownGrouping(t)      | 
            Self::TooManyUnderlines(t)    | 
            Self::UnexpectedEol(t, _)     | 
            Self::ExpectedEndL(t)         | 
            Self::ExpectedArrow(t)        | 
            Self::ExpectedComma(t)        | 
            Self::ExpectedColon(t)        | 
            Self::ExpectedMatrix(t)       | 
            Self::ExpectedSegment(t)      | 
            Self::ExpectedTokenFeature(t) | 
            Self::ExpectedVariable(t)     | 
            Self::ExpectedUnderline(t)    | 
            Self::ExpectedRightBracket(t) |
            Self::UnknownIPA(t)           | 
            Self::InsertErr(t)            | 
            Self::DeleteErr(t)            |
            Self::MetathErr(t)            |
            Self::BadSyllableMatrix(t)  => (
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
            Self::OptLocError(pos)  |
            Self::EmptySet(pos)  => (
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

#[derive(Debug, Clone)]
pub enum AliasRuntimeError {
    NodeCannotBeNone  (String, AliasPosition),
    NodeCannotBeSome  (String, AliasPosition),
    IndefiniteFeatures(AliasPosition),
    OverlongPosLongNeg(AliasPosition),
    SecStrPosStrNeg   (AliasPosition),
    LengthNoSegment   (AliasPosition),
    EmptySyllable     (AliasPosition),
}

impl ASCAError for AliasRuntimeError {
    fn get_error_message(&self) -> String {
        match self {
            Self::NodeCannotBeSome(node, _) => format!("{} node cannot arbitrarily positive", node),
            Self::NodeCannotBeNone(node, _) => format!("{} node cannot be removed", node),
            Self::IndefiniteFeatures(_) => "Cannot create a segment from a limited list of features. If you would like to assign to the previous segment, use '+'.".to_string(),
            Self::OverlongPosLongNeg(_) => "A segment cannot be both [+overlong] and [-long]".to_string(),
            Self::SecStrPosStrNeg   (_) => "A syllable cannot be both [+sec.stress] and [-stress]".to_string(),
            Self::LengthNoSegment   (_) => "Cannot apply length. If you would like to assign to the previous segment, use '+'.".to_string(),
            Self::EmptySyllable     (_) => "Cannot add at the start of a syllable".to_string()
        }
    }

    fn format_alias_error(&self, into: &[String], from: &[String]) -> String {
        const MARG: &str = "\n    |     ";
        let mut result = format!("{} {}", "Syntax Error:".bright_red().bold(), self.get_error_message().bold()); 

        let (arrows, kind, line) = match self {
            Self::NodeCannotBeNone(_, pos) |
            Self::NodeCannotBeSome(_, pos) |
            Self::IndefiniteFeatures (pos) |
            Self::OverlongPosLongNeg (pos) |
            Self::SecStrPosStrNeg    (pos) |
            Self::LengthNoSegment    (pos) |
            Self::EmptySyllable      (pos) => (
                " ".repeat(pos.start) + &"^".repeat(pos.end-pos.start) + "\n",
                pos.kind,
                pos.line
            ),
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


const FEAT_VARIANTS: [&str; 171] = [ 
    "root", "rut", "rt", 
    "consonantal", "consonant", "cons" , "cns",          
    "sonorant", "sonor", "son" , "snrt", "sn",
    "syllabic", "syllab", "syll" , "syl",          
    // Manner Node Features
    "manner", "mann", "man", "mnnr" , "mnr" ,
    "continuant", "contin", "cont" , "cnt",
    "approximant", "approx", "appr", "app",
    "lateral", "latrl", "ltrl", "lat",
    "nasal", "nsl", "nas",
    "delayedrelease", "delrel", "d.r.", "del.rel.", "delayed", "dl", "dlrl", "dr", "delay", "del.rel", "drel",
    "strident", "strid", "stri", "stridnt",
    "rhotic", "rhot", "rho", "rhtc", "rh",
    "click", "clik", "clk", "clck",
    // Laryngeal Node Features
    "laryngeal", "laryng", "laryn", "lar",
    "voice", "voi", "vce", "vc",
    "spreadglottis", "spreadglot", "spread", "s.g.", "s.g", "sg",
    "constrictedglottis", "constricted", "constglot", "constr", "c.g.", "c.g", "cg",
    // Place Node Feature
    "place", "plce", "plc",   
    // Labial Place Node Features
    "labial", "lbl", "lab",
    "labiodental", "ldental", "labiodent", "labdent", "lbdntl", "ldent", "ldl",
    "round", "rund", "rnd", "rd",
    // Coronal Place Node Features
    "coronal", "coron", "crnl", "cor",
    "anterior", "anter", "antr", "ant",
    "distributed", "distrib", "dist", "dis" , "dst",
    // Dorsal Place Node Features
    "dorsal", "drsl", "dors", "dor",
    "front", "frnt", "fnt", "fro", "frt", "fr",
    "back", "bck", "bk",
    "high", "hgh", "hi",
    "low", "lw", "lo",
    "tense", "tens", "tns", "ten",
    "reduced", "reduc", "redu", "rdcd", "red",
    // Pharyngeal Place Node Features
    "pharyngeal", "pharyng", "pharyn", "phar", "phr",
    "advancedtongueroot", "a.t.r.", "a.t.r", "a.tr", "at.r", "atr",
    "retractedtongueroot", "r.t.r.", "r.t.r", "r.tr", "rt.r", "rtr",
    // Suprasegmental Features
    "long", "lng",
    "overlong", "overlng", "ovrlng", "vlng",
    "stress", "str",
    "secondarystress", "sec.stress", "secstress", "sec.str.", "sec.str", "secstr", "sec"
];

fn get_feat_closest(s: &str) -> &'static str {
    let mut best_lev = usize::MAX;
    let mut best_str= "";

    for var in FEAT_VARIANTS {
        let len = lev(s, var);
        if len < best_lev {
            best_str = var;
            best_lev = len;
        }
    }

    best_str
} 

fn lev(a: &str, b: &str) -> usize {
    let mut dist = 0;

    if a == b { return dist }

    let a_len = a.chars().count();
    let b_len = b.chars().count();

    debug_assert!(a_len > 0);
    debug_assert!(b_len > 0);

    let mut cache: Vec<usize> = (1..).take(a_len).collect();

    for (bi, b_ch) in b.chars().enumerate() {
        dist = bi;
        let mut a_dist = bi;

        for (ai, a_ch) in a.chars().enumerate() {
            let b_dist = a_dist + (a_ch != b_ch) as usize;

            a_dist = cache[ai];

            dist = if a_dist > dist {
                if b_dist > dist {
                    dist + 1
                } else {
                    b_dist
                }
            } else if b_dist > a_dist {
                a_dist + 1
            } else {
                b_dist
            };

            cache[ai] = dist;
        }
    }

    dist
}