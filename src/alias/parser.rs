use crate::{AliasSyntaxError, BinMod, FType, ModKind, Modifiers, Mods, NodeType, Segment, SupraType, CARDINALS_MAP, DIACRITS};

use super::{AliasKind, AliasPosition, AliasToken, AliasTokenKind, FeatType};


#[derive(Debug, Clone)]
pub(crate) struct Transformation {
    pub(crate) kind: AliasKind,
    pub(crate) input: AliasItem,
    pub(crate) output: AliasItem
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum AliasParseElement {
    Empty,
    SyllBound,
    Replacement(String),
    Ipa        (Segment, Option<Modifiers>),
    Matrix     (Modifiers, Option<usize>),
}
impl AliasParseElement {
    fn as_matrix(&self) -> Option<&Modifiers> {
        if let Self::Matrix(v, _) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AliasItem {
    pub(crate) kind: AliasParseElement,
    pub(crate) position: AliasPosition,
}

impl AliasItem {
    pub(crate) fn new(k: AliasParseElement, p: AliasPosition) -> Self {
        Self { kind: k, position: p }
    }
}

pub(crate) struct AliasParser {
    kind: AliasKind,
    token_list: Vec<AliasToken>,
    line: usize,
    pos: usize,
    curr_tkn: AliasToken,
}

impl AliasParser {
    pub(crate) fn new(kind: AliasKind, token_list: Vec<AliasToken>, line: usize) -> Self {
        let mut s = Self {
            kind,
            token_list, 
            line,
            pos: 0, 
            curr_tkn: AliasToken { kind: AliasTokenKind::Eol, value: String::new(), position: AliasPosition::new(line, 0, 1 ) },
        };
        s.curr_tkn = s.token_list[s.pos].clone();

        s
    }

    fn advance(&mut self) {
        self.pos += 1;
        self.curr_tkn = if self.has_more_tokens() {
            self.token_list[self.pos].clone()
        } else {
            AliasToken { kind: AliasTokenKind::Eol, value: String::new(), position: AliasPosition::new(self.line, self.pos, self.pos+1) }
        }
    }

    fn has_more_tokens(&self) -> bool { self.pos < self.token_list.len() }

    fn peek_expect(&self, knd: AliasTokenKind) -> bool { self.curr_tkn.kind == knd }

    fn expect(&mut self, knd: AliasTokenKind) -> bool {
        if self.curr_tkn.kind == knd {
            self.advance();
            true
        } else {
            false
        }
    }

    fn eat(&mut self) -> AliasToken {
        let token = self.curr_tkn.clone();
        self.advance();
        token
    }

    fn eat_expect(&mut self, knd: AliasTokenKind) -> Option<AliasToken> {
        if self.curr_tkn.kind == knd {
            Some(self.eat())
        } else {
            None
        }
    }

    fn get_empty(&mut self) -> Option<AliasItem> {
        if !self.peek_expect(AliasTokenKind::Star) && !self.peek_expect(AliasTokenKind::EmptySet) {
            return None
        }
        let token = self.eat();

        Some(AliasItem::new(AliasParseElement::Empty, token.position))
    }

    fn get_replacement_term(&mut self) -> Option<AliasItem> {
        // RPL_TRM ← EMP / [Alphabetic-Unicode-Char]+
        let emp = self.get_empty();
        if emp.is_some() { return emp }

        if !self.peek_expect(AliasTokenKind::String) {
            return None
        }
        let token = self.eat();

        Some(AliasItem::new(AliasParseElement::Replacement(token.value), token.position))
    }

    fn get_replacements(&mut self) -> Result<Vec<AliasItem>, AliasSyntaxError> {
        // REPL ← RPL_TRM ( ',' RPL_TRM )*
        let mut replacements = Vec::new();

        if let Some(r) = self.get_replacement_term() {
            replacements.push(r);
            loop {
                if !self.expect(AliasTokenKind::Comma) { 
                    break;
                }
                match self.get_replacement_term() {
                    Some(r) => replacements.push(r),
                    None => break
                }
            }
        }
        if replacements.is_empty() {
            return Err(AliasSyntaxError::EmptyReplacements(self.line, self.token_list[self.pos].position.start))
        }

        Ok(replacements)
    }

    fn get_syll_bound(&mut self) -> Option<AliasItem> {
        if let Some(token) = self.eat_expect(AliasTokenKind::SyllBoundary) {
            return Some(AliasItem::new(AliasParseElement::SyllBound, token.position))
        }
        None
    }

    fn ipa_to_vals(&mut self, ipa: AliasToken) -> Result<Segment, AliasSyntaxError> {
        match CARDINALS_MAP.get(&ipa.value) {
            Some(z) => Ok(*z),
            None => Err(AliasSyntaxError::UnknownIPA(ipa))
        }
    }

    fn is_feature(&self) -> bool{ matches!(self.curr_tkn.kind, AliasTokenKind::Feature(_)) }

    fn curr_token_to_modifier(&self) -> (FeatType, Mods) {
        // returns ARG ← ('+' / '-') [a-zA-Z]+ / TONE  
        match self.curr_tkn.kind {
            AliasTokenKind::Feature(feature) => {
                let value = &self.curr_tkn.value;
                match value.as_str() {
                    "+" => (feature, Mods::Binary(BinMod::Positive)),
                    "-" => (feature, Mods::Binary(BinMod::Negative)),
                    _ if feature == FeatType::Supr(SupraType::Tone) => (feature, Mods::Number(value.to_owned())),
                    _ => {
                        unreachable!();
                    }
                }
            },
            _ => unreachable!(),
        }
    }

    fn get_param_args(&mut self) -> Result<Modifiers, AliasSyntaxError> {
        // returns (ARG (',' ARG)*)? ']' 
        let mut args = Modifiers::new();
        while self.has_more_tokens() {
            if self.expect(AliasTokenKind::RightSquare) {
                break;
            }
            if self.expect(AliasTokenKind::Comma) {
                continue;
            }
            if self.is_feature() {
                let (ft, mods) = self.curr_token_to_modifier();
                match ft {
                    FeatType::Node(t) => args.nodes[t as usize] = if let Mods::Binary(b) = mods {
                        Some(ModKind::Binary(b))
                    } else { unreachable!() },
                    FeatType::Feat(t) => args.feats[t as usize] = if let Mods::Binary(b) = mods {
                        Some(ModKind::Binary(b))
                    } else { unreachable!() },
                    FeatType::Supr(t) => match mods {
                        Mods::Alpha(_)  => unreachable!(),
                        Mods::Number(n) => args.suprs.tone = Some(n),
                        Mods::Binary(b) => match t {
                            SupraType::Long => args.suprs.length[0] = Some(ModKind::Binary(b)),
                            SupraType::Overlong => args.suprs.length[1] = Some(ModKind::Binary(b)),
                            SupraType::Stress => args.suprs.stress[0] = Some(ModKind::Binary(b)),
                            SupraType::SecStress => args.suprs.stress[1] = Some(ModKind::Binary(b)),
                            SupraType::Tone => unreachable!("Tone cannot be `+/-`"),
                        },
                    }
                }
                self.advance();
                continue;
            }
            if self.curr_tkn.kind == AliasTokenKind::Eol {
                return Err(AliasSyntaxError::UnexpectedEol(self.curr_tkn.clone(), ']'))
            }
            return Err(AliasSyntaxError::ExpectedTokenFeature(self.curr_tkn.clone()))
        }
        Ok(args)
    }

    fn get_params(&mut self) -> Result<AliasItem, AliasSyntaxError> {
        // returns PARAMS ← '[' ARG (',' ARG)* ']'  
        let start = self.token_list[self.pos-1].position.start;
        let args = self.get_param_args()?;
        let end = self.token_list[self.pos-1].position.end;
        
        Ok(AliasItem::new(AliasParseElement::Matrix(args, None), AliasPosition::new(self.line, start, end)))
    }

    fn get_ipa(&mut self) -> Result<AliasItem, AliasSyntaxError> {
        // returns IPA (':' PARAMS)?
        
        let mut ipa = self.ipa_to_vals(self.curr_tkn.clone())?;
        let pos = self.curr_tkn.position;
        self.advance();


        while matches!(self.curr_tkn.kind, AliasTokenKind::Diacritic(_)) {
            let dia = self.eat();
            let d = dia.kind.as_diacritic().unwrap();
            if let Err((mod_index, is_node)) = ipa.check_and_apply_diacritic(&DIACRITS[*d as usize]) {
                if !is_node {
                    let ft = FType::from_usize(mod_index);
                    let positive = match &DIACRITS[*d as usize].prereqs.feats[mod_index].unwrap() {
                        ModKind::Binary(bin_mod) => *bin_mod == BinMod::Positive,
                        _ => unreachable!(),
                    };
                    return Err(AliasSyntaxError::DiacriticDoesNotMeetPreReqsFeat(pos, dia.position, ft.to_string(), positive))
                } else {
                    let nt = NodeType::from_usize(mod_index);
                    let positive = match &DIACRITS[*d as usize].prereqs.nodes[mod_index].unwrap() {
                        ModKind::Binary(bin_mod) => *bin_mod == BinMod::Positive,
                        _ => unreachable!(),
                    };
                    return Err(AliasSyntaxError::DiacriticDoesNotMeetPreReqsNode(pos, dia.position, nt.to_string(), positive))
                };
            }
        }
        if !self.expect(AliasTokenKind::Colon) {
            return Ok(AliasItem::new(AliasParseElement::Ipa(ipa, None), AliasPosition::new(self.line, pos.start, self.token_list[self.pos-1].position.end)))
        }
        if !self.expect(AliasTokenKind::LeftSquare) {
            return Err(AliasSyntaxError::ExpectedMatrix(self.curr_tkn.clone()))
        }

        let params = self.get_params()?;
        let joined_kind = AliasParseElement::Ipa(ipa, Some(params.kind.as_matrix().unwrap().clone()));
        
        Ok(AliasItem::new(joined_kind, AliasPosition::new(self.line, pos.start, params.position.end)))
    }

    fn get_segment(&mut self) -> Result<Option<AliasItem>, AliasSyntaxError> {
        if self.peek_expect(AliasTokenKind::Cardinal) {
            return Ok(Some(self.get_ipa()?))
        }

        Ok(None)
    }

    fn get_input_term(&mut self) -> Result<Option<AliasItem>, AliasSyntaxError> {
        
        let s_bound = self.get_syll_bound();
        if s_bound.is_some() { return Ok(s_bound) }

        self.get_segment()
    } 

    fn get_input(&mut self) -> Result<Vec<AliasItem>, AliasSyntaxError> {

        let mut inputs = Vec::new();

        if let Some(trm) = self.get_input_term()? {
            inputs.push(trm);
            loop {
                if !self.expect(AliasTokenKind::Comma) { 
                    break;
                }
                match self.get_input_term()? {
                    Some(trm) => inputs.push(trm),
                    None => break
                }
            }
        }

        if inputs.is_empty() {
            return Err(AliasSyntaxError::EmptyInput(self.line, self.token_list[self.pos].position.start))
        }


        Ok(inputs)
    }

    // INTO
    fn get_deromaniser(&mut self) -> Result<Vec<Transformation>, AliasSyntaxError> {
        todo!()
    }

    // FROM
    fn get_romaniser(&mut self) -> Result<Vec<Transformation>, AliasSyntaxError> {
        // returns FROM ← F_TERMS ARR REPLACE EOL

        // F_TERMS
        let input_terms = self.get_input()?;
        // ARR
        if !self.expect(AliasTokenKind::Arrow) && !self.expect(AliasTokenKind::GreaterThan) {
            return Err(AliasSyntaxError::ExpectedArrow(self.curr_tkn.clone()))
        }
        // REPLACE
        let output_terms = self.get_replacements()?;
        // !EOL
        if !self.expect(AliasTokenKind::Eol) {
            return Err(AliasSyntaxError::ExpectedEndLn(self.curr_tkn.clone()))
        }

        // Split into individual transformations
        let max = std::cmp::max(input_terms.len(), output_terms.len());

        if input_terms.len()  != max && input_terms.len()  != 1 { return Err(AliasSyntaxError::UnbalancedIO(input_terms.clone())) }
        if output_terms.len() != max && output_terms.len() != 1 { return Err(AliasSyntaxError::UnbalancedIO(output_terms.clone())) }

        let mut transformations = Vec::new();
        for i in 0..max {
            let input  = if  input_terms.len() == 1 {  input_terms[0].clone() } else {  input_terms[i].clone() };
            let output = if output_terms.len() == 1 { output_terms[0].clone() } else { output_terms[i].clone() };

            transformations.push(Transformation { kind: self.kind, input, output });
        }

        Ok(transformations)
    }

    pub(crate) fn parse(&mut self) -> Result<Vec<Transformation>, AliasSyntaxError> {
        if self.curr_tkn.kind == AliasTokenKind::Eol {
            Ok(Vec::new())
        } else {
            Ok(
                match self.kind {
                    AliasKind::Deromaniser => self.get_deromaniser()?,
                    AliasKind::Romaniser => self.get_romaniser()?,
                }
            )
        }
    }

}



#[cfg(test)]
mod parser_tests {
    use crate::{alias::lexer::AliasLexer, SupraSegs};

    use super::*;

    fn setup_derom(test_str: &str) -> Vec<AliasToken> { AliasLexer::new(AliasKind::Deromaniser, &String::from(test_str).chars().collect::<Vec<_>>(),0).get_line().unwrap() }
    fn setup_roman(test_str: &str) -> Vec<AliasToken> { AliasLexer::new(AliasKind::Romaniser,   &String::from(test_str).chars().collect::<Vec<_>>(),0).get_line().unwrap() }



    #[test]
    fn test_romanisation_simple() {
        let maybe_result = AliasParser::new(AliasKind::Romaniser, setup_roman("ʃ > sh"), 0).parse();
        assert!(maybe_result.is_ok());

        let result = maybe_result.unwrap();

        assert_eq!(result.len(), 1);

        assert_eq!(result[0].input , AliasItem::new(AliasParseElement::Ipa(CARDINALS_MAP.get("ʃ").unwrap().clone(), None), AliasPosition::new(0, 0, 1)));
        assert_eq!(result[0].output, AliasItem::new(AliasParseElement::Replacement("sh".to_string()),                      AliasPosition::new(0, 4, 6)));
    }

    #[test]
    fn test_romanisation_mods() {
        let maybe_result = AliasParser::new(AliasKind::Romaniser, setup_roman("a:[+str] > á"), 0).parse();
        assert!(maybe_result.is_ok());

        let result = maybe_result.unwrap();

        assert_eq!(result.len(), 1);

        let mut x = Modifiers::new();
        x.suprs = SupraSegs { stress: [Some(ModKind::Binary(BinMod::Positive)), None], length: [None, None], tone: None };

        assert_eq!(result[0].input , AliasItem::new(AliasParseElement::Ipa(CARDINALS_MAP.get("a").unwrap().clone(), Some(x)), AliasPosition::new(0,  0,  8)));
        assert_eq!(result[0].output, AliasItem::new(AliasParseElement::Replacement("á".to_string()),                          AliasPosition::new(0, 11, 12)));
    }
}