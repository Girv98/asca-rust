use std::fmt;
use colored::Colorize;

use crate :: { 
    alias :: { AliasKind, AliasPosition }, 
    rule  :: { Position, Reference }
};
use super :: { ASCAError, RuleGroup };

#[derive(Debug, Clone)]
pub enum RuleRuntimeError { 
    BoundaryInsideUnderlineStruct(Position, Position),
    SyllbleInsideUnderlineStruct (Position, Position),
    SyllRefInsideUnderlineStruct (Position, Position),
    StructInsideUnderlineStruct  (Position, Position),
    SubstitutionSylltoMatrix     (Position, Position),
    SubstitutionSylltoBound      (Position, Position),
    SubstitutionWordBound        (Position, Position),
    SubstitutionBoundtoSyll      (Position, Position),
    SubstitutionSylltoSeg        (Position, Position),
    SubstitutionBoundMod         (Position, Position),
    MetathWordBoundary           (Position, Position),
    MetathSyllBoundary           (Position, Position),
    MetathSyllSegment            (Position, Position),
    UnevenSet                    (Position, Position),
    GroupSuprIsInverted(&'static str, Position),
    GroupSuprIsBinary  (&'static str, Position),
    NodeCannotBeSome(String, Position),
    NodeCannotBeNone(String, Position),
    NodeCannotBeSet (String, Position),
    WordBoundSetLocError(Position),
    SubstitutionEllipsis(Position),
    BoundaryInsideStruct(Position),
    SyllbleInsideStruct (Position),
    SyllRefInsideStruct (Position),
    InsertionGroupedEnv (Position),
    AlphaIsNotSuprGroup (Position),
    StructInsideStruct  (Position),
    AlphaNodeAssignInv  (Position),
    OverlongPosLongNeg  (Position),
    AlphaIsNotSameNode  (Position),
    SubstitutionMatrix  (Position),
    InsertionEllipsis   (Position),
    SubstitutionSyll    (Position),
    SubstitutionSet     (Position),
    SubstitutionOpt     (Position),
    SecStrPosStrNeg     (Position),
    AlphaUnknownInv     (Position),
    InsertionMatrix     (Position),
    AlphaIsNotNode      (Position),
    InsertionNoEnv      (Position),
    InsertionSet        (Position),
    InsertionOpt        (Position),
    AlphaUnknown        (Position),
    LonelySet           (Position),
    UnknownReference(Reference),
    DeletionOnlySeg,
    DeletionOnlySyll,
    UnevenEllipsis(Vec<Position>),
}

impl From<RuleRuntimeError> for ASCAError {
    fn from(e: RuleRuntimeError) -> Self {
        Self::RuleRun(e)
    }
}

impl fmt::Display for RuleRuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BoundaryInsideUnderlineStruct(..) => write!(f, "A boundary cannot be used inside a structure"),
            Self::SyllbleInsideUnderlineStruct (..) => write!(f, "A syllable cannot be used inside a structure"),
            Self::SyllRefInsideUnderlineStruct (..) => write!(f, "A reference assigned to a syllable cannot be used inside a structure"),
            Self::StructInsideUnderlineStruct  (..) => write!(f, "A structure cannot be used inside a structure"),
            Self::SubstitutionSylltoMatrix(..) => write!(f, "Syllables and boundaries cannot be substituted by a segment"),
            Self::SubstitutionSylltoBound (..) => write!(f, "Syllables cannot be substituted by a boundary"),
            Self::SubstitutionWordBound   (..) => write!(f, "Word boundaries cannot be substituted."),
            Self::SubstitutionBoundtoSyll (..) => write!(f, "Syllable boundaries cannot be substituted by a syllable."),
            Self::SubstitutionSylltoSeg   (..) => write!(f, "Segments cannot be substituted by a blank syllable or a boundary"),
            // Self::SubstitutionSegtoSyll   (..) write!(f, => "Segments cannot be substituted by a syllable or a boundary"),
            Self::SubstitutionBoundMod    (..) => write!(f, "Syllable boundaries cannot be modified by a matrix."),
            Self::MetathWordBoundary      (..) => write!(f, "Cannot swap a word boundary with a syllable boundary"),
            Self::MetathSyllBoundary      (..) => write!(f, "Cannot swap a syllable with a syllable boundary"),
            Self::MetathSyllSegment       (..) => write!(f, "Cannot swap a syllable with a segment"),
            Self::UnevenSet               (..) => write!(f, "Two matched sets must have the same number of elements"),
            Self::GroupSuprIsInverted(supr, _) => write!(f, "Grouped Suprasegmental '{supr}' cannot be inverted"),
            Self::GroupSuprIsBinary  (supr, _) => write!(f, "Grouped Suprasegmental '{supr}' cannot be binary"),
            Self::NodeCannotBeSome(node, _) => write!(f, "{node} node cannot arbitrarily positive"),
            Self::NodeCannotBeNone(node, _) => write!(f, "{node} node cannot be removed"),
            Self::NodeCannotBeSet (node, _) => write!(f, "{node} node cannot be assigned using PLACE alpha"),
            Self::WordBoundSetLocError(_) => write!(f, "Word Boundaries cannot be in the input or output"),
            Self::SubstitutionEllipsis(_) => write!(f, "An ellipsis cannot be substituted"),
            Self::BoundaryInsideStruct(_) => write!(f, "A boundary cannot be used inside a structure"),
            Self::SyllbleInsideStruct (_) => write!(f, "A syllable cannot be used inside a structure"),
            Self::SyllRefInsideStruct (_) => write!(f, "A reference assigned to a syllable cannot be used inside a structure"),
            Self::AlphaIsNotSuprGroup (_) => write!(f, "This type of alpha cannot be used on 'length'"),
            Self::StructInsideStruct  (_) => write!(f, "A structure cannot be used inside a structure"),
            Self::InsertionGroupedEnv (_) => write!(f, "Grouped Environments cannot (yet) be used in insertion rules"),
            Self::AlphaNodeAssignInv  (_) => write!(f, "Node alphas cannot be assigned inverse. First occurrence of a node alpha must be positive."),
            Self::OverlongPosLongNeg  (_) => write!(f, "A segment cannot be both [+overlong] and [-long]"),
            Self::AlphaIsNotSameNode  (_) => write!(f, "Node alphas must only be used on the same node."),
            Self::SubstitutionMatrix  (_) => write!(f, "A matrix cannot be used inside a structure when substituting"),
            Self::InsertionEllipsis   (_) => write!(f, "An ellipsis cannot be inserted"),
            Self::SubstitutionSyll    (_) => write!(f, "Blank syllables cannot be used in substitution output."),
            Self::SubstitutionSet     (_) => write!(f, "Sets cannot be used in structures the in output"),
            Self::SubstitutionOpt     (_) => write!(f, "Options cannot be used in structures the in output"),
            Self::SecStrPosStrNeg     (_) => write!(f, "A syllable cannot be both [+sec.stress] and [-stress]"),
            Self::AlphaUnknownInv     (_) => write!(f, "First occurence of a node alpha must not be inverted."),
            Self::InsertionMatrix     (_) => write!(f, "An incomplete matrix cannot be inserted"),
            Self::AlphaIsNotNode      (_) => write!(f, "Node alphas cannot be used on binary features"),
            Self::InsertionNoEnv      (_) => write!(f, "Insertion rules must have a context"),
            Self::InsertionSet        (_) => write!(f, "Sets cannot be used in insertion output"),
            Self::InsertionOpt        (_) => write!(f, "Options cannot be used in insertion output"),
            Self::AlphaUnknown        (_) => write!(f, "Alpha has not be assigned before applying"),
            Self::LonelySet           (_) => write!(f, "A Set in output must have a matching Set in input"),
            Self::UnknownReference(token) => write!(f, "Unknown reference '{}'", token.value),
            Self::DeletionOnlySyll => write!(f, "Can't delete a word's only syllable"),
            Self::DeletionOnlySeg  => write!(f, "Can't delete a word's only segment"),
            Self::UnevenEllipsis      (_) => write!(f, "Uneven number of ellipses in input and output"),
        }
    }
}

impl RuleRuntimeError {
    pub fn format(&self, rules: &[RuleGroup]) -> String {
        const MARG: &str = "\n    |     ";
        let mut result = format!("{} {}", "Runtime Error:".bright_red().bold(), self.to_string().bold());
        
        let (arrows, group , line) =  match self {
            Self::UnevenEllipsis      (positions) => {
                let mut asdf = String::new();
                let mut prev_end = 0;
                for pos in positions {
                    asdf += &" ".repeat(pos.start - prev_end);
                    asdf += &"^".repeat(pos.end-pos.start);
                    prev_end = pos.end;
                }

                asdf += "\n";
                (asdf, positions[0].group, positions[0].line)
            }
            Self::DeletionOnlySyll | Self::DeletionOnlySeg => return result,
            Self::UnknownReference(t) => (
                " ".repeat(t.position.start) + &"^".repeat(t.position.end-t.position.start) + "\n", 
                t.position.group,
                t.position.line
            ),
            Self::InsertionNoEnv(pos) => (
                " ".repeat(pos.end) + "^" + "\n", 
                pos.group,
                pos.line
            ),
            Self::WordBoundSetLocError  (pos) |
            Self::SubstitutionEllipsis  (pos) |
            Self::BoundaryInsideStruct  (pos) |
            Self::SyllRefInsideStruct   (pos) |
            Self::SyllbleInsideStruct   (pos) |
            Self::AlphaIsNotSuprGroup   (pos) |
            Self::StructInsideStruct    (pos) |
            Self::InsertionGroupedEnv   (pos) |
            Self::AlphaNodeAssignInv    (pos) |
            Self::OverlongPosLongNeg    (pos) |
            Self::AlphaIsNotSameNode    (pos) |
            Self::SubstitutionMatrix    (pos) |
            Self::InsertionEllipsis     (pos) |
            Self::SubstitutionSyll      (pos) |
            Self::SubstitutionSet       (pos) |
            Self::SubstitutionOpt       (pos) |
            Self::SecStrPosStrNeg       (pos) |
            Self::AlphaUnknownInv       (pos) |
            Self::InsertionMatrix       (pos) |
            Self::AlphaIsNotNode        (pos) |
            Self::InsertionSet          (pos) |
            Self::InsertionOpt          (pos) |
            Self::AlphaUnknown          (pos) |
            Self::LonelySet             (pos) | 
            Self::GroupSuprIsInverted(_, pos) |
            Self::GroupSuprIsBinary  (_, pos) |
            Self::NodeCannotBeSome   (_, pos) |
            Self::NodeCannotBeNone   (_, pos) |
            Self::NodeCannotBeSet    (_, pos) => (
                " ".repeat(pos.start) + &"^".repeat(pos.end-pos.start) + "\n",
                pos.group,
                pos.line
            ),
            Self::BoundaryInsideUnderlineStruct(a, b) | 
            Self::SyllbleInsideUnderlineStruct (a, b) | 
            Self::SyllRefInsideUnderlineStruct (a, b) | 
            Self::StructInsideUnderlineStruct  (a, b) | 
            Self::SubstitutionSylltoMatrix     (a, b) |
            Self::SubstitutionSylltoBound      (a, b) |
            Self::SubstitutionSylltoSeg        (a, b) |
            Self::SubstitutionWordBound        (a, b) |
            Self::SubstitutionBoundtoSyll      (a, b) |
            Self::SubstitutionBoundMod         (a, b) |
            Self::MetathWordBoundary           (a, b) |
            Self::MetathSyllBoundary           (a, b) |
            Self::MetathSyllSegment            (a, b) |
            Self::UnevenSet                    (a, b) => (
                   " ".repeat(a.start) + &"^".repeat(a.end - a.start) 
                + &" ".repeat(b.start - a.end) + &"^".repeat(b.end - b.start) + "\n",
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
    GroupedSupras     (AliasPosition),
}

impl From<AliasRuntimeError> for ASCAError {
    fn from(e: AliasRuntimeError) -> Self {
        Self::AliasRun(e)
    }
}

impl fmt::Display for AliasRuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NodeCannotBeSome(node, _) => write!(f, "{node} node cannot arbitrarily positive"),
            Self::NodeCannotBeNone(node, _) => write!(f, "{node} node cannot be removed"),
            Self::IndefiniteFeatures(_) => write!(f, "Cannot create a segment from a limited list of features. If you would like to assign to the previous segment, use '+'."),
            Self::OverlongPosLongNeg(_) => write!(f, "A segment cannot be both [+overlong] and [-long]"),
            Self::SecStrPosStrNeg   (_) => write!(f, "A syllable cannot be both [+sec.stress] and [-stress]"),
            Self::LengthNoSegment   (_) => write!(f, "Cannot apply length. If you would like to assign to the previous segment, use '+'."),
            Self::EmptySyllable     (_) => write!(f, "Cannot add at the start of a syllable"),
            Self::GroupedSupras     (_) => write!(f, "Grouped Suprasegmentals cannot be used with aliases"),
        }
    }
}

impl AliasRuntimeError {
    pub fn format(&self, into: &[String], from: &[String]) -> String {
        const MARG: &str = "\n    |     ";
        let mut result = format!("{} {}", "Syntax Error:".bright_red().bold(), self.to_string().bold()); 

        let (arrows, kind, line) = match self {
            Self::NodeCannotBeNone(_, pos) |
            Self::NodeCannotBeSome(_, pos) |
            Self::IndefiniteFeatures (pos) |
            Self::OverlongPosLongNeg (pos) |
            Self::SecStrPosStrNeg    (pos) |
            Self::LengthNoSegment    (pos) |
            Self::EmptySyllable      (pos) |
            Self::GroupedSupras      (pos) => (
                " ".repeat(pos.start) + &"^".repeat(pos.end-pos.start) + "\n",
                pos.kind,
                pos.line
            ),
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