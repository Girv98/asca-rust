use colored::Colorize;

use crate::{alias::{AliasKind, AliasPosition}, lexer::{Position, Token}};

use super::{ASCAError, Error, RuleGroup};

#[derive(Debug, Clone)]
pub enum WordRuntimeError {
    UnknownSegment(String, usize,  usize), // (Segs before, Word Pos in list, Segment Pos in Words)
}

impl From<WordRuntimeError> for Error {
    fn from(e: WordRuntimeError) -> Self {
        Self::WordRun(e)
    }
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
    SubstitutionSylltoMatrix(Position, Position),
    SubstitutionSylltoBound (Position, Position),
    SubstitutionSyllBound   (Position, Position),
    SubstitutionSylltoSeg   (Position, Position),
    SubstitutionBoundMod    (Position, Position),
    MetathSyllBoundary      (Position, Position),
    MetathSyllSegment       (Position, Position),
    UnevenSet               (Position, Position),
    NodeCannotBeSome(String, Position),
    NodeCannotBeNone(String, Position),
    NodeCannotBeSet (String, Position),
    WordBoundSetLocError(Position),
    SubstitutionEllipsis(Position),
    SyllVarInsideStruct (Position),
    InsertionGroupedEnv (Position),
    AlphaNodeAssignInv  (Position),
    OverlongPosLongNeg  (Position),
    AlphaIsNotSameNode  (Position),
    SubstitutionMatrix  (Position),
    InsertionEllipsis   (Position),
    SubstitutionSyll    (Position),
    SecStrPosStrNeg     (Position),
    AlphaUnknownInv     (Position),
    InsertionMatrix     (Position),
    AlphaIsNotNode      (Position),
    InsertionNoEnv      (Position),
    AlphaUnknown        (Position),
    LonelySet           (Position),
    UnknownVariable(Token),
    DeletionOnlySeg,
    DeletionOnlySyll,
}

impl From<RuleRuntimeError> for Error {
    fn from(e: RuleRuntimeError) -> Self {
        Self::RuleRun(e)
    }
}

impl ASCAError for RuleRuntimeError {
    fn get_error_message(&self) -> String {
        match self {
            Self::SubstitutionSylltoMatrix(..) => "Syllables and boundaries cannot be substituted by a segment".to_string(),
            Self::SubstitutionSylltoBound (..) => "Syllables cannot be substituted by a boundary".to_string(),
            Self::SubstitutionSyllBound   (..) => "Syllable boundaries cannot be substituted.".to_string(),
            Self::SubstitutionSylltoSeg   (..) => "Segments cannot be substituted by a syllable or a boundary".to_string(),
            // Self::SubstitutionSegtoSyll   (..) => "Segments cannot be substituted by a syllable or a boundary".to_string(),
            Self::SubstitutionBoundMod    (..) => "Syllable boundaries cannot be modified by a matrix.".to_string(),
            Self::MetathSyllBoundary      (..) => "Cannot swap a syllable with a syllable".to_string(),
            Self::MetathSyllSegment       (..) => "Cannot swap a syllable with a segment".to_string(),
            Self::UnevenSet               (..) => "Two matched sets must have the same number of elements".to_string(),
            Self::NodeCannotBeSome(node, _) => format!("{} node cannot arbitrarily positive", node),
            Self::NodeCannotBeNone(node, _) => format!("{} node cannot be removed", node),
            Self::NodeCannotBeSet (node, _) => format!("{} node cannot be assigned using PLACE alpha", node),
            Self::WordBoundSetLocError(_) => "Word Boundaries cannot be in the input or output".to_string(),
            Self::SubstitutionEllipsis(_) => "An ellipsis cannot be substituted".to_string(),
            Self::SyllVarInsideStruct (_) => "Variables assigned to syllables cannot be used inside a structure".to_string(),
            Self::InsertionGroupedEnv (_) => "Grouped Environments cannot (yet) be used in insertion rules".to_string(),
            Self::AlphaNodeAssignInv  (_) => "Node alphas cannot be assigned inverse. First occurrence of a node alpha must be positive.".to_string(),
            Self::OverlongPosLongNeg  (_) => "A segment cannot be both [+overlong] and [-long]".to_string(),
            Self::AlphaIsNotSameNode  (_) => "Node alphas must only be used on the same node.".to_string(),
            Self::SubstitutionMatrix  (_) => "A matrix cannot be used inside a structure when substituting".to_string(),
            Self::InsertionEllipsis   (_) => "An ellipsis cannot be inserted".to_string(),
            Self::SubstitutionSyll    (_) => "Blank syllables cannot be used in substitution output.".to_string(),
            Self::SecStrPosStrNeg     (_) => "A syllable cannot be both [+sec.stress] and [-stress]".to_string(),
            Self::AlphaUnknownInv     (_) => "First occurence of a node alpha must not be inverted.".to_string(),
            Self::InsertionMatrix     (_) => "An incomplete matrix cannot be inserted".to_string(),
            Self::AlphaIsNotNode      (_) => "Node alphas cannot be used on binary features".to_string(),
            Self::InsertionNoEnv      (_) => "Insertion rules must have a context".to_string(),
            Self::AlphaUnknown        (_) => "Alpha has not be assigned before applying".to_string(),
            Self::LonelySet           (_) => "A Set in output must have a matching Set in input".to_string(),
            Self::UnknownVariable(token)  => format!("Unknown variable '{}' at {}", token.value, token.position.start),
            Self::DeletionOnlySyll => "Can't delete a word's only syllable".to_string(),
            Self::DeletionOnlySeg  => "Can't delete a word's only segment".to_string(),
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
            Self::InsertionNoEnv(pos) => (
                " ".repeat(pos.end) + "^" + "\n", 
                pos.group,
                pos.line
            ),
            Self::WordBoundSetLocError(pos) |
            Self::SubstitutionEllipsis(pos) |
            Self::SyllVarInsideStruct (pos) |
            Self::InsertionGroupedEnv (pos) |
            Self::AlphaNodeAssignInv  (pos) |
            Self::OverlongPosLongNeg  (pos) |
            Self::AlphaIsNotSameNode  (pos) |
            Self::SubstitutionMatrix  (pos) |
            Self::InsertionEllipsis   (pos) |
            Self::SubstitutionSyll    (pos) |
            Self::SecStrPosStrNeg     (pos) |
            Self::AlphaUnknownInv     (pos) |
            Self::InsertionMatrix     (pos) |
            Self::AlphaIsNotNode      (pos) |
            Self::AlphaUnknown        (pos) |
            Self::LonelySet           (pos) | 
            Self::NodeCannotBeSome (_, pos) |
            Self::NodeCannotBeNone (_, pos) |
            Self::NodeCannotBeSet  (_, pos) => (
                " ".repeat(pos.start) + &"^".repeat(pos.end-pos.start) + "\n",
                pos.group,
                pos.line
            ),
            Self::SubstitutionSylltoMatrix(a, b) |
            Self::SubstitutionSylltoBound (a, b) |
            Self::SubstitutionSylltoSeg   (a, b) |
            Self::SubstitutionSyllBound   (a, b) |
            Self::SubstitutionBoundMod    (a, b) |
            Self::MetathSyllBoundary      (a, b) |
            Self::MetathSyllSegment       (a, b) |
            Self::UnevenSet               (a, b) => (
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

impl From<AliasRuntimeError> for Error {
    fn from(e: AliasRuntimeError) -> Self {
        Self::AliasRun(e)
    }
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