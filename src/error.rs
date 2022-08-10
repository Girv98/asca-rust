use std::fmt;

use crate::{lexer::Token, parser::Item};


#[derive(Debug, Clone)]
pub enum RuntimeError { 

}

#[derive(Debug, Clone)]
pub enum SyntaxError {
    OptMathError(Token, usize, usize),
    UnknownIPA(Token),
    UnknownChar(Token),
    UnknownVariable(Token),
    ExpectedEndL(Token),
    ExpectedArrow(Token),
    ExpectedComma(Token),
    ExpectedColon(Token),
    ExpectedMatrix(Token),
    ExpectedSegment(Token),
    ExpectedFeature(Token),
    ExpectedVariable(Token),
    ExpectedUnderline(Token),
    ExpectedRightBracket(Token),
    AlreadyInitialisedVariable(Item, Item, usize),
    InsertErr,
    DeleteErr,
    EmptyInput,
    EmptyOutput,
    EmptyEnv,
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::OptMathError(_, l, h)  => write!(f, "Second argument '{}' must be >= first argument '{}'", h, l),
            Self::UnknownIPA(token)           => write!(f, "Could not get value of IPA '{}'.", token.value),
            Self::UnknownChar(token)          => write!(f, "No known primative '{}'. Known primatives are (C)onsonant, (O)bstruent, (S)onorant, (L)iquid, (N)asal, (G)lide, and (V)owel", token.value),
            Self::UnknownVariable(token)      => write!(f, "Unknown variable '{}'", token.value),
            Self::ExpectedEndL(token)         => write!(f, "Expected end of line, received '{}'. Did you forget a '/' between the output and environment?", token.value),
            Self::ExpectedArrow(token)        => write!(f, "Expected '>', '->' or '=>', but received '{}'", token.value),
            Self::ExpectedComma(token)        => write!(f, "Expected ',', but received '{}'", token.value),
            Self::ExpectedColon(token)        => write!(f, "Expected ':', but received '{}'", token.value),
            Self::ExpectedMatrix(token)       => write!(f, "Expected '[', but received '{}'", token.value),
            Self::ExpectedSegment(token)      => write!(f, "Expected an IPA character, Primative or Matrix, but received '{}'", token.value),
            Self::ExpectedFeature(token)      => write!(f, "{} cannot be placed inside a matrix. An element inside `[]` must a distinctive feature", token.value),
            Self::ExpectedVariable(token)      => write!(f, "Expected number, but received {} ", token.value),
            Self::ExpectedUnderline(token)    => write!(f, "Expected '_', but received '{}'", token.value),
            Self::ExpectedRightBracket(token) => write!(f, "Expected ')', but received '{}'", token.value),
            Self::AlreadyInitialisedVariable(set_tkn, new_tkn, num) => write!(f, "Variable '{}' is already set", num),
            Self::InsertErr   => write!(f, "The input of an insertion rule must only contain `*` or `∅`"),
            Self::DeleteErr   => write!(f, "The output of a deletion rule must only contain `*` or `∅`"),
            Self::EmptyInput  => write!(f, "Input cannot be empty. Use `*` or '∅' to indicate insertion"),
            Self::EmptyOutput => write!(f, "Output cannot be empty. Use `*` or '∅' to indicate deletion"),
            Self::EmptyEnv    => write!(f, "Environment cannot be empty following a seperator."),
        }
    }
}