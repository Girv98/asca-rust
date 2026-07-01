use std::collections::VecDeque;

use super::{
    Action, ActionKind, MatchElement, Payload, SubRule
};

use crate :: {
    error :: RuleRuntimeError, 
    rule  :: { ParseElement, ParseItem, RuleType },
    word  :: { Phrase, SegPos, StressKind, Syllable, Word }
};


#[derive(Debug, Clone, Copy)]
enum Meta<'a> {
    /// An element to be swapped
    Some(&'a MatchElement), 
    /// An element only used as a reference, i.e. to insert/delete another element after/before this element
    Ref(&'a MatchElement)   
}

#[derive(Debug, Clone, Copy)]
struct MetaGroup<'l> (Meta<'l>, Meta<'l>);


impl SubRule {
    fn metathesis_gen_actions(&self, phrase: &Phrase, matched_els: Vec<MetaGroup>) -> Result<Vec<Action>, RuleRuntimeError> {
        let mut right_res: Vec<Action> = Vec::new();
        let mut left_res: Vec<Action> = Vec::new();

        // Bad name but
        // Some(true) => Insert
        // Some(false) => Delete
        // None => Substitute
        let mut maybe_insert: Option<bool> = None;
        
        for els in matched_els.iter() {
            match &(els.0, els.1) {
                &(Meta::Some(&MatchElement::Segment(left_pos, left_err_pos)), Meta::Some(&MatchElement::Segment(right_pos, right_err_pos))) => {
                    right_res.push(Action { 
                        kind: ActionKind::ReplaceSegment(
                            (Self::ONE, Self::ONE), 
                            Payload::Segment(phrase.get_seg_at(left_pos).unwrap(), None), 
                            left_err_pos
                        ), 
                        pos: right_pos 
                    });

                    left_res.push(Action { 
                        kind: ActionKind::ReplaceSegment(
                            (Self::ONE, Self::ONE), 
                            Payload::Segment(phrase.get_seg_at(right_pos).unwrap(), None), 
                            right_err_pos
                        ), 
                        pos: left_pos 
                    });
                }
                &(Meta::Some(&MatchElement::Segment(left_pos, left_err_pos)), Meta::Some(&MatchElement::LongSegment(right_pos, right_err_pos))) => {
                    right_res.push(Action { 
                        kind: ActionKind::ReplaceSegment(
                            (Self::non_zero_len(phrase.seg_length_at(right_pos) as u8), Self::ONE), 
                            Payload::Segment(phrase.get_seg_at(left_pos).unwrap(), None), 
                            left_err_pos
                        ), 
                        pos: right_pos 
                    });

                    left_res.push(Action { 
                        kind: ActionKind::ReplaceSegment(
                            (Self::ONE, Self::non_zero_len(phrase.seg_length_at(right_pos) as u8)), 
                            Payload::Segment(phrase.get_seg_at(right_pos).unwrap(), None), 
                            right_err_pos
                        ), 
                        pos: left_pos 
                    });
                }
                &(Meta::Some(&MatchElement::LongSegment(left_pos, left_err_pos)), Meta::Some(&MatchElement::Segment(right_pos, right_err_pos))) => {
                    right_res.push(Action { 
                        kind: ActionKind::ReplaceSegment(
                            (Self::ONE, Self::non_zero_len(phrase.seg_length_at(left_pos) as u8)), 
                            Payload::Segment(phrase.get_seg_at(left_pos).unwrap(), None), 
                            left_err_pos
                        ), 
                        pos: right_pos 
                    });

                    left_res.push(Action { 
                        kind: ActionKind::ReplaceSegment(
                            (Self::non_zero_len(phrase.seg_length_at(left_pos) as u8), Self::ONE), 
                            Payload::Segment(phrase.get_seg_at(right_pos).unwrap(), None), 
                            right_err_pos
                        ), 
                        pos: left_pos 
                    });
                }
                &(Meta::Some(&MatchElement::LongSegment(left_pos, left_err_pos)), Meta::Some(&MatchElement::LongSegment(right_pos, right_err_pos))) => {
                    right_res.push(Action { 
                        kind: ActionKind::ReplaceSegment(
                            (Self::non_zero_len(phrase.seg_length_at(right_pos) as u8), Self::non_zero_len(phrase.seg_length_at(left_pos) as u8)), 
                            Payload::Segment(phrase.get_seg_at(left_pos).unwrap(), None), 
                            left_err_pos
                        ), 
                        pos: right_pos 
                    });

                    left_res.push(Action { 
                        kind: ActionKind::ReplaceSegment(
                            (Self::non_zero_len(phrase.seg_length_at(left_pos) as u8), Self::non_zero_len(phrase.seg_length_at(right_pos) as u8)), 
                            Payload::Segment(phrase.get_seg_at(right_pos).unwrap(), None), 
                            right_err_pos
                        ), 
                        pos: left_pos 
                    });
                }
                
                &(Meta::Some(&MatchElement::Syllable(left_word_pos, left_syll_pos, _)), Meta::Some(&MatchElement::Syllable(right_word_pos, right_syll_pos, _))) => {
                    right_res.push(Action { 
                        kind: ActionKind::ReplaceSyllable(phrase[left_word_pos].syllables[left_syll_pos].clone()), 
                        pos: SegPos { word_index: right_word_pos, syll_index: right_syll_pos, seg_index: 0 }
                    });

                    left_res.push(Action { 
                        kind: ActionKind::ReplaceSyllable(phrase[right_word_pos].syllables[right_syll_pos].clone()), 
                        pos: SegPos { word_index: left_word_pos, syll_index: left_syll_pos, seg_index: 0 }
                    });
                }
                &(Meta::Some(&MatchElement::SyllBound(..)), Meta::Some(&MatchElement::SyllBound(..))) |
                &(Meta::Some(&MatchElement::WordBound(..)), Meta::Some(&MatchElement::WordBound(..))) => {/* Do nothing */},
                // s$ > &
                &(Meta::Some(&MatchElement::Segment(left_pos, left_err_pos)), Meta::Some(&MatchElement::SyllBound(right_word_index, right_boundary_pos, _))) => {
                    let b_pos = SegPos { word_index: right_word_index, syll_index: right_boundary_pos, seg_index: 0 };

                    if !phrase.in_bounds(b_pos) && right_res.is_empty() {
                        let mut syll = Syllable::new();
                        syll.segments.push_back(phrase.get_seg_at(left_pos).unwrap());
                        
                        right_res.push(Action { 
                            kind: ActionKind::InsertSyllable(syll), 
                            pos: b_pos
                        });
                    } else {
                        right_res.push(Action {
                            kind: ActionKind::InsertSegment(
                                Self::ONE, 
                                phrase.get_seg_at(left_pos).unwrap(), 
                                None, 
                                left_err_pos
                            ),
                            pos: b_pos,
                        });
                    }

                    left_res.push(Action { 
                        kind: ActionKind::DeleteSegment(Self::ONE), 
                        pos: left_pos 
                    });
                }
                // s:[+long]$ > &
                &(Meta::Some(&MatchElement::LongSegment(left_pos, left_err_pos)), Meta::Some(&MatchElement::SyllBound(right_word_index, right_boundary_pos, _))) => {
                    let seg_len = Self::non_zero_len(phrase.seg_length_at(left_pos) as u8);
                    let b_pos = SegPos { word_index: right_word_index, syll_index: right_boundary_pos, seg_index: 0 };
                    
                    if !phrase.in_bounds(b_pos) && right_res.is_empty() {
                        let mut syll = Syllable::new();
                        let seg = phrase.get_seg_at(left_pos).unwrap();

                        for _ in 0..seg_len.get() {
                            syll.segments.push_back(seg);
                        }
                        
                        right_res.push(Action { 
                            kind: ActionKind::InsertSyllable(syll), 
                            pos: b_pos
                        });
                    } else {
                        right_res.push(Action {
                            kind: ActionKind::InsertSegment(
                                seg_len, 
                                phrase.get_seg_at(left_pos).unwrap(), 
                                None, 
                                left_err_pos
                            ),
                            pos: SegPos { word_index: right_word_index, syll_index: right_boundary_pos, seg_index: 0 },
                        });
                    }
                    
                    left_res.push(Action { 
                        kind: ActionKind::DeleteSegment(seg_len), 
                        pos: left_pos 
                    });
                }
                // $s > &
                &(Meta::Some(&MatchElement::SyllBound(left_word_index, left_boundary_pos, _)), Meta::Some(&MatchElement::Segment(right_pos, right_err_pos))) => {
                    let seg = phrase.get_seg_at(right_pos).expect("segment has not been moved");

                    right_res.push(Action { 
                        kind: ActionKind::DeleteSegment(Self::ONE), 
                        pos: right_pos 
                    });
                    
                    if left_boundary_pos == 0 {
                        let mut syll = Syllable::new();
                        syll.segments.push_back(seg);

                        left_res.push(Action { 
                            kind: ActionKind::InsertSyllable(syll), 
                            pos: SegPos { word_index: left_word_index, syll_index: 0, seg_index: 0 } 
                        });
                    } else {
                        let mut insert_pos = SegPos { word_index: left_word_index, syll_index: left_boundary_pos, seg_index: 0 };
                        insert_pos.decrement(phrase);
                        insert_pos.seg_index += 1;
    
                        left_res.push(Action {
                            kind: ActionKind::InsertSegment(
                                Self::ONE, 
                                seg, 
                                None, 
                                right_err_pos
                            ),
                            pos: insert_pos,
                        });
                    }
                }
                // $s:[+long] > &
                &(Meta::Some(&MatchElement::SyllBound(left_word_index, left_boundary_pos, _)), Meta::Some(&MatchElement::LongSegment(right_pos, right_err_pos))) => {
                    let seg = phrase.get_seg_at(right_pos).expect("segment has not been moved");
                    let seg_len = Self::non_zero_len(phrase.seg_length_at(right_pos) as u8);
                    
                    right_res.push(Action { 
                        kind: ActionKind::DeleteSegment(seg_len), 
                        pos: right_pos 
                    });

                    if left_boundary_pos == 0 {
                        let mut syll = Syllable::new();
                        for _ in 0..seg_len.get() {
                            syll.segments.push_back(seg);
                        }

                        left_res.push(Action { 
                            kind: ActionKind::InsertSyllable(syll), 
                            pos: SegPos { word_index: left_word_index, syll_index: 0, seg_index: 0 } 
                        });
                    } else {
                        let mut insert_pos = SegPos { word_index: left_word_index, syll_index: left_boundary_pos, seg_index: 0 };
                        insert_pos.decrement(phrase);
                        insert_pos.seg_index += 1;

                        left_res.push(Action {
                            kind: ActionKind::InsertSegment(
                                seg_len, 
                                seg, 
                                None, 
                                right_err_pos
                            ),
                            pos: insert_pos,
                        });
                    }
                }
                // n ## > &
                &(Meta::Some(&MatchElement::Segment(left_pos, left_err_pos)), Meta::Some(&MatchElement::WordBound(word_index, _))) => {
                    let word = Word { syllables: vec![Syllable{ segments: VecDeque::from(vec![phrase.get_seg_at(left_pos).unwrap()]), stress: StressKind::Unstressed, tone: 0 }].into() };

                    if word_index+1 >= phrase.len() {
                        // NOTE: This is will never happen because of line 120 in self.apply()
                        right_res.push(Action { 
                            kind: ActionKind::InsertWord(word), 
                            pos: SegPos { word_index: word_index+1, syll_index: 0, seg_index: 0 },
                        })
                    } else {
                        right_res.push(Action {
                            kind: ActionKind::InsertSegment(
                                Self::ONE, 
                                phrase.get_seg_at(left_pos).unwrap(), 
                                None, 
                                left_err_pos
                            ),
                            pos: SegPos { word_index: word_index+1, syll_index: 0, seg_index: 0 },
                        });
                    }

                    left_res.push(Action { 
                        kind: ActionKind::DeleteSegment(Self::ONE), 
                        pos: left_pos 
                    });
                }
                // n:[+long] ## > &
                &(Meta::Some(&MatchElement::LongSegment(left_pos, left_err_pos)), Meta::Some(&MatchElement::WordBound(word_index, _))) => {
                    let seg = phrase.get_seg_at(left_pos).unwrap();
                    let seg_len = Self::non_zero_len(phrase.seg_length_at(left_pos) as u8);
                    let mut word = Word::new("").unwrap();
                    word.syllables.push_back(Syllable::new());
                    for _ in 0..seg_len.get() {
                        word.syllables[0].segments.push_back(seg);
                    }

                    if word_index+1 >= phrase.len() {
                        // NOTE: This is will never happen because of line 120 in self.apply()
                        right_res.push(Action { 
                            kind: ActionKind::InsertWord(word), 
                            pos: SegPos { word_index: word_index+1, syll_index: 0, seg_index: 0 },
                        })
                    } else {
                        right_res.push(Action {
                            kind: ActionKind::InsertSegment(
                                seg_len, 
                                phrase.get_seg_at(left_pos).unwrap(), 
                                None, 
                                left_err_pos
                            ),
                            pos: SegPos { word_index: word_index+1, syll_index: 0, seg_index: 0 },
                        });
                    }

                    left_res.push(Action { 
                        kind: ActionKind::DeleteSegment(seg_len), 
                        pos: left_pos 
                    });
                }
                // ## n > & i.e. a napron => an apron
                &(Meta::Some(&MatchElement::WordBound(word_index, _)), Meta::Some(&MatchElement::Segment(right_pos, right_err_pos))) => {
                    right_res.push(Action { 
                        kind: ActionKind::DeleteSegment(Self::ONE), 
                        pos: right_pos 
                    });

                    let mut insert_pos = SegPos { word_index: word_index+1, syll_index: 0, seg_index: 0 };
                    insert_pos.word_decrement(phrase);
                    insert_pos.seg_index += 1;

                    left_res.push(Action {
                        kind: ActionKind::InsertSegment(
                            Self::ONE, 
                            phrase.get_seg_at(right_pos).unwrap(), 
                            None, 
                            right_err_pos
                        ),
                        pos: insert_pos,
                    });
                }
                // ## n:[+long] > &
                &(Meta::Some(&MatchElement::WordBound(word_index, _)), Meta::Some(&MatchElement::LongSegment(right_pos, right_err_pos))) => {
                    let seg_len = Self::non_zero_len(phrase.seg_length_at(right_pos) as u8);
                    
                    right_res.push(Action { 
                        kind: ActionKind::DeleteSegment(seg_len), 
                        pos: right_pos 
                    });

                    let mut insert_pos = SegPos { word_index: word_index +1, syll_index: 0, seg_index: 0 };
                    insert_pos.word_decrement(phrase);
                    insert_pos.seg_index += 1;

                    left_res.push(Action {
                        kind: ActionKind::InsertSegment(
                            seg_len, 
                            phrase.get_seg_at(right_pos).unwrap(), 
                            None, 
                            right_err_pos
                        ),
                        pos: insert_pos,
                    });
                }
                // % ## > &
                &(Meta::Some(&MatchElement::Syllable(left_word_pos, left_syll_pos, _)), Meta::Some(&MatchElement::WordBound(word_index, _))) => {
                    right_res.push(Action {
                        kind: ActionKind::InsertSyllable(phrase[left_word_pos].syllables[left_syll_pos].clone()),
                        pos: SegPos { word_index: word_index+1, syll_index: 0, seg_index: 0 },
                    });

                    left_res.push(Action { 
                        kind: ActionKind::DeleteSyllable, 
                        pos: SegPos { word_index: left_word_pos, syll_index: left_syll_pos, seg_index: 0 }
                    });
                }
                // ## % > &
                &(Meta::Some(&MatchElement::WordBound(word_index, _)), Meta::Some(&MatchElement::Syllable(right_word_pos, right_syll_pos, _))) => {
                    right_res.push(Action { 
                        kind: ActionKind::DeleteSyllable, 
                        pos: SegPos { word_index: right_word_pos, syll_index: right_syll_pos, seg_index: 0 } 
                    });

                    let mut insert_pos = SegPos { word_index: word_index+1, syll_index: 0, seg_index: 0 };
                    insert_pos.word_decrement(phrase);
                    insert_pos.seg_index += 1;

                    left_res.push(Action {
                        kind: ActionKind::InsertSyllable(phrase[right_word_pos].syllables[right_syll_pos].clone()),
                        pos: insert_pos,
                    })
                }
                
                // s% > &
                &(Meta::Some(&MatchElement::Segment(left_pos, _)), Meta::Some(&MatchElement::Syllable(right_word_pos, right_syll_pos, right_err_pos))) => {
                    let mut left_syll = Syllable::new();
                    left_syll.segments.push_back(phrase.get_seg_at(left_pos).unwrap());

                    right_res.push(Action { 
                        kind: ActionKind::ReplaceSyllable(left_syll), 
                        pos: SegPos { word_index: right_word_pos, syll_index: right_syll_pos, seg_index: 0 }
                    });

                    left_res.push(Action { 
                        kind: ActionKind::ReplaceSegment(
                            (Self::ONE, Self::ONE),
                            Payload::Syllable(phrase[right_word_pos].syllables[right_syll_pos].clone()),
                            right_err_pos
                        ), 
                        pos: left_pos
                    });
                }
                //%s > &
                &(Meta::Some(&MatchElement::LongSegment(left_pos, _)), Meta::Some(&MatchElement::Syllable(right_word_pos, right_syll_pos, right_err_pos))) => {
                    let mut left_syll = Syllable::new();
                    let seg_len = phrase.seg_length_at(left_pos);
                    for _ in 0..seg_len {
                        left_syll.segments.push_back(phrase.get_seg_at(left_pos).unwrap());
                    }

                    right_res.push(Action { 
                        kind: ActionKind::ReplaceSyllable(left_syll), 
                        pos: SegPos { word_index: right_word_pos, syll_index: right_syll_pos, seg_index: 0 }
                    });

                    left_res.push(Action { 
                        kind: ActionKind::ReplaceSegment(
                            (Self::non_zero_len(seg_len as u8), Self::ONE),
                            Payload::Syllable(phrase[right_word_pos].syllables[right_syll_pos].clone()),
                            right_err_pos
                        ), 
                        pos: left_pos
                    });
                }
                // %s > &
                &(Meta::Some(&MatchElement::Syllable(left_word_pos, left_syll_pos, left_err_pos)), Meta::Some(&MatchElement::Segment(right_pos, _))) => {
                    let mut right_syll = Syllable::new();
                    right_syll.segments.push_back(phrase.get_seg_at(right_pos).unwrap());

                    right_res.push(Action {
                        kind: ActionKind::ReplaceSegment(
                            (Self::ONE, Self::ONE),
                            Payload::Syllable(phrase[left_word_pos].syllables[left_syll_pos].clone()),
                            left_err_pos
                        ), 
                        pos: right_pos
                    });

                    left_res.push(Action { 
                        kind: ActionKind::ReplaceSyllable(right_syll), 
                        pos: SegPos { word_index: left_word_pos, syll_index: left_syll_pos, seg_index: 0 }
                    });
                }
                //
                &(Meta::Some(&MatchElement::Syllable(left_word_pos, left_syll_pos, left_err_pos)), Meta::Some(&MatchElement::LongSegment(right_pos, _))) => {
                    let mut right_syll = Syllable::new();
                    let seg_len = phrase.seg_length_at(right_pos);
                    for _ in 0..seg_len {
                        right_syll.segments.push_back(phrase.get_seg_at(right_pos).unwrap());
                    }

                    right_res.push(Action {
                        kind: ActionKind::ReplaceSegment(
                            (Self::non_zero_len(seg_len as u8), Self::ONE),
                            Payload::Syllable(phrase[left_word_pos].syllables[left_syll_pos].clone()),
                            left_err_pos
                        ), 
                        pos: right_pos
                    });

                    left_res.push(Action { 
                        kind: ActionKind::ReplaceSyllable(right_syll), 
                        pos: SegPos { word_index: left_word_pos, syll_index: left_syll_pos, seg_index: 0 }
                    });
                }


                &(Meta::Some(&MatchElement::SyllBound(.., pos0)), Meta::Some(&MatchElement::WordBound(.., pos1))) | 
                &(Meta::Some(&MatchElement::WordBound(.., pos0)), Meta::Some(&MatchElement::SyllBound(.., pos1))) => {
                    return Err(RuleRuntimeError::MetathWordBoundary(pos0, pos1))
                }
                &(Meta::Some(&MatchElement::Syllable(.., pos0)), Meta::Some(&MatchElement::SyllBound(.., pos1))) |
                &(Meta::Some(&MatchElement::SyllBound(.., pos0)), Meta::Some(&MatchElement::Syllable(.., pos1))) => {
                    return Err(RuleRuntimeError::MetathSyllBoundary(pos0, pos1))
                }
                
                // Delete at Start
                &(Meta::Ref(first_el), Meta::Some(el)) => {
                    maybe_insert = Some(false);
                    self.metathesis_gen_del_actions(phrase, &mut left_res, &mut right_res, el, first_el)?;
                },
                // Insert at end
                &(Meta::Some(el), Meta::Ref(last_el)) => {
                    maybe_insert = Some(true);
                    self.metathesis_gen_ins_actions(phrase, &mut left_res, &mut right_res, el, last_el)?;
                },
                
                
                (Meta::Ref(_), Meta::Ref(_)) |
                (Meta::Some(MatchElement::Set(..)), _) |
                (_, Meta::Some(MatchElement::Set(..))) => unreachable!(),
            }
        }

        // right = [1R, 2R]
        // left  = [1L, 2L]
        // reslt = [1L, 2L, 2R, 1R] // self.apply_sub_actions reverses the actions list

        if self.rule_type == RuleType::MetaOrdered {
            match maybe_insert {
                Some(true) => left_res.reverse(),   // reslt = [2L, 1L, 2R, 1R]
                Some(false) => right_res.reverse(), // reslt = [1L, 2L, 1R, 2R]
                None => {},
            }
        }

        while let Some(x) = right_res.pop() {
            left_res.push(x);
        }

        Ok(left_res)
    }

    fn metathesis_gen_del_actions(&self, phrase: &Phrase, left_res: &mut Vec<Action>, right_res: &mut Vec<Action>, el: &MatchElement, first_el: &MatchElement) -> Result<(), RuleRuntimeError> {
        let ins_pos = match first_el {
            MatchElement::Set(..) => unreachable!(),
            &MatchElement::Segment(seg_pos, _) | &MatchElement::LongSegment(seg_pos , _) => {
                seg_pos
            }
            &MatchElement::Syllable(word_index, syll_index, _) => {
                SegPos { word_index, syll_index, seg_index: 0 }
            },
            &MatchElement::SyllBound(word_index, syll_index, _) => {
                let mut insert_pos = SegPos::new(word_index, syll_index, 0);
                if syll_index != 0 {
                    insert_pos.decrement(phrase);
                    insert_pos.seg_index += 1;
                }
                insert_pos
            },
            MatchElement::WordBound(word_index, _) => {
                let mut insert_pos = SegPos::new(word_index+1, 0, 0);
                insert_pos.word_decrement(phrase);
                insert_pos.seg_index += 1;
                insert_pos
            },
        };

        match (el, first_el) {
            (&MatchElement::Segment(seg_pos, err_pos), MatchElement::Segment(..)) |
            (&MatchElement::Segment(seg_pos, err_pos), MatchElement::LongSegment(..)) => {
                right_res.push(Action { 
                    kind: ActionKind::DeleteSegment(Self::ONE), 
                    pos: seg_pos 
                });
                
                left_res.push(Action { 
                    kind: ActionKind::InsertSegment(
                        Self::ONE, 
                        phrase.get_seg_at(seg_pos).unwrap(), 
                        None, 
                        err_pos
                    ), 
                    pos: ins_pos
                });
            }
            (&MatchElement::Segment(seg_pos, err_pos), MatchElement::Syllable(..)) |
            (&MatchElement::Segment(seg_pos, err_pos), MatchElement::SyllBound(..)) => {
                left_res.push(Action { 
                        kind: ActionKind::InsertSegment(
                            Self::ONE, 
                            phrase.get_seg_at(seg_pos).unwrap(), 
                            None, 
                            err_pos
                        ), 
                        pos: ins_pos
                    });
                
                right_res.push(Action { 
                    kind: ActionKind::DeleteSegment(Self::ONE), 
                    pos: seg_pos 
                });
            }
            (&MatchElement::Segment(seg_pos, err_pos), MatchElement::WordBound(..)) => {
                left_res.push(Action {
                    kind: ActionKind::InsertSegment(
                        Self::ONE, 
                        phrase.get_seg_at(seg_pos).unwrap(), 
                        None, 
                        err_pos
                    ),
                    pos: ins_pos,
                });

                right_res.push(Action { 
                    kind: ActionKind::DeleteSegment(Self::ONE), 
                    pos: seg_pos 
                });
            }
            
            (&MatchElement::LongSegment(seg_pos, err_pos), MatchElement::Segment(..)) |
            (&MatchElement::LongSegment(seg_pos, err_pos), MatchElement::LongSegment(..)) => {
                let len = Self::non_zero_len(phrase.seg_length_at(seg_pos) as u8);
                left_res.push(Action { 
                    kind: ActionKind::InsertSegment(
                        len, 
                        phrase.get_seg_at(seg_pos).unwrap(), 
                        None, 
                        err_pos
                    ), 
                    pos: ins_pos
                });

                right_res.push(Action { 
                    kind: ActionKind::DeleteSegment(len), 
                    pos: seg_pos 
                });
            }
            (&MatchElement::LongSegment(seg_pos, err_pos), MatchElement::Syllable(..)) |
            (&MatchElement::LongSegment(seg_pos, err_pos), MatchElement::SyllBound(..)) => {
                let len = Self::non_zero_len(phrase.seg_length_at(seg_pos) as u8);
                
                left_res.push(Action { 
                    kind: ActionKind::InsertSegment(
                        len, 
                        phrase.get_seg_at(seg_pos).unwrap(), 
                        None, 
                        err_pos
                    ), 
                    pos: ins_pos
                });

                right_res.push(Action { 
                    kind: ActionKind::DeleteSegment(len), 
                    pos: seg_pos 
                });
            }
            (&MatchElement::LongSegment(seg_pos, err_pos), MatchElement::WordBound(..)) => {
                let len = Self::non_zero_len(phrase.seg_length_at(seg_pos) as u8);
                left_res.push(Action {
                    kind: ActionKind::InsertSegment(
                        len, 
                        phrase.get_seg_at(seg_pos).unwrap(), 
                        None, 
                        err_pos
                    ),
                    pos: ins_pos,
                });

                right_res.push(Action { 
                    kind: ActionKind::DeleteSegment(len), 
                    pos: seg_pos 
                });
            }

            (&MatchElement::Syllable(wp, sp, _), MatchElement::Segment(..)) |
            (&MatchElement::Syllable(wp, sp, _), MatchElement::LongSegment(..)) |
            (&MatchElement::Syllable(wp, sp, _), MatchElement::Syllable(..)) |
            (&MatchElement::Syllable(wp, sp, _), MatchElement::SyllBound(..)) | 
            (&MatchElement::Syllable(wp, sp, _), MatchElement::WordBound(..)) => {
                left_res.push(Action { 
                    kind: ActionKind::InsertSyllable(phrase[wp].syllables[sp].clone()), 
                    pos: ins_pos
                });

                right_res.push(Action { 
                    kind: ActionKind::DeleteSyllable, 
                    pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 } 
                });
            }

            (&MatchElement::SyllBound(wp, bp, _), MatchElement::Segment(..) | MatchElement::LongSegment(..)) => {
                left_res.push(Action { 
                    kind: ActionKind::InsertBoundary, 
                    pos: ins_pos
                });

                right_res.push(Action { 
                    kind: ActionKind::DeleteBoundary, 
                    pos: SegPos { word_index: wp, syll_index: bp, seg_index: 0 } 
                });
            }
            (MatchElement::SyllBound(..), MatchElement::Syllable (..)) => { /* Do Nothing */ }
            (MatchElement::SyllBound(..), MatchElement::SyllBound(..)) => { /* Do Nothing */ }
            (MatchElement::SyllBound(..), MatchElement::WordBound(..)) => { /* Do Nothing */ }
            (MatchElement::WordBound(..), MatchElement::WordBound(..)) => { /* Do Nothing */ }

            (&MatchElement::WordBound(wp, _), _) => {
                left_res.push(Action { 
                    kind: ActionKind::InsertWordBound, 
                    pos: ins_pos
                });

                right_res.push(Action { 
                    kind: ActionKind::DeleteWordBound, 
                    pos: SegPos { word_index: wp, syll_index: 0, seg_index: 0 } 
                });
            },

            (MatchElement::Set(..), _) | (_, MatchElement::Set(..)) => unreachable!("sets"),
        }

        Ok(())
    }

    fn metathesis_gen_ins_actions(&self, phrase: &Phrase, left_res: &mut Vec<Action>, right_res: &mut Vec<Action>, el: &MatchElement, last_el: &MatchElement) -> Result<(), RuleRuntimeError> {
        let ins_pos = match last_el {
            MatchElement::Set(..) => unreachable!(),
            &MatchElement::Segment(SegPos { word_index, syll_index, seg_index }, _) => {
                SegPos { word_index, syll_index, seg_index: seg_index + 1 }
            },
            &MatchElement::LongSegment(seg_pos @ SegPos { word_index, syll_index, seg_index } , _) => {
                SegPos { word_index, syll_index, seg_index: seg_index + phrase.seg_length_at(seg_pos) }
            },
            &MatchElement::Syllable(word_index, syll_index, _) => {
                SegPos { word_index, syll_index: syll_index + 1, seg_index: 0 }
            },
            &MatchElement::SyllBound(word_index, syll_index, _) => {
                SegPos { word_index, syll_index, seg_index: 0 }
            },
            MatchElement::WordBound(wp, _) => {
                SegPos { word_index: wp + 1, syll_index: 0, seg_index: 0 }
            },
        };
        
        match (el, last_el) {
            (&MatchElement::Segment(seg_pos, err_pos), MatchElement::Segment(..)) |
            (&MatchElement::Segment(seg_pos, err_pos), MatchElement::LongSegment(..)) => {
                right_res.push(Action { 
                    kind: ActionKind::InsertSegment(
                        Self::ONE, 
                        phrase.get_seg_at(seg_pos).unwrap(), 
                        None, 
                        err_pos
                    ), 
                    pos: ins_pos
                });

                left_res.push(Action { 
                    kind: ActionKind::DeleteSegment(Self::ONE), 
                    pos: seg_pos 
                });
            }
            (&MatchElement::Segment(seg_pos, err_pos), MatchElement::Syllable(..)) |
            (&MatchElement::Segment(seg_pos, err_pos), MatchElement::SyllBound(..)) => { 
                let last_action_insert_syll = match right_res.last() {
                    Some(action) => matches!(action.kind, ActionKind::InsertSyllable(_)),
                    None => false,
                };

                if phrase[ins_pos.word_index].syllables.len() <= ins_pos.syll_index && !last_action_insert_syll {
                    let mut syll = Syllable::new();
                    syll.segments.push_back(phrase.get_seg_at(seg_pos).unwrap());
                    
                    right_res.push(Action { 
                        kind: ActionKind::InsertSyllable(syll), 
                        pos: ins_pos
                    });
                } else {
                    right_res.push(Action { 
                        kind: ActionKind::InsertSegment(
                            Self::ONE, 
                            phrase.get_seg_at(seg_pos).unwrap(), 
                            None, 
                            err_pos
                        ), 
                        pos: ins_pos
                    });
                }

                left_res.push(Action { 
                    kind: ActionKind::DeleteSegment(Self::ONE), 
                    pos: seg_pos 
                });
            }
            (&MatchElement::Segment(seg_pos, err_pos), MatchElement::WordBound(..)) => {
                right_res.push(Action {
                    kind: ActionKind::InsertSegment(
                        Self::ONE, 
                        phrase.get_seg_at(seg_pos).unwrap(), 
                        None, 
                        err_pos
                    ),
                    pos: ins_pos,
                });

                left_res.push(Action { 
                    kind: ActionKind::DeleteSegment(Self::ONE), 
                    pos: seg_pos 
                });
            }

            (&MatchElement::LongSegment(seg_pos, err_pos), MatchElement::Segment(..)) |
            (&MatchElement::LongSegment(seg_pos, err_pos), MatchElement::LongSegment(..)) => {
                let len = Self::non_zero_len(phrase.seg_length_at(seg_pos) as u8);
                right_res.push(Action { 
                    kind: ActionKind::InsertSegment(
                        len, 
                        phrase.get_seg_at(seg_pos).unwrap(), 
                        None, 
                        err_pos
                    ), 
                    pos: ins_pos
                });

                left_res.push(Action { 
                    kind: ActionKind::DeleteSegment(len), 
                    pos: seg_pos 
                });
            }
            (&MatchElement::LongSegment(seg_pos, err_pos), MatchElement::Syllable(..)) |
            (&MatchElement::LongSegment(seg_pos, err_pos), MatchElement::SyllBound(..)) => {
                let len = Self::non_zero_len(phrase.seg_length_at(seg_pos) as u8);
                let last_action_insert_syll = match right_res.last() {
                    Some(action) => matches!(action.kind, ActionKind::InsertSyllable(_)),
                    None => false,
                };
                
                if phrase[ins_pos.word_index].syllables.len() <= ins_pos.syll_index && !last_action_insert_syll {
                    let mut syll = Syllable::new();
                    let seg = phrase.get_seg_at(seg_pos).unwrap();

                    for _ in 0..len.get() {
                        syll.segments.push_back(seg);
                    }
                    
                    right_res.push(Action { 
                        kind: ActionKind::InsertSyllable(syll), 
                        pos: ins_pos
                    });
                } else {
                    right_res.push(Action { 
                        kind: ActionKind::InsertSegment(
                            len, 
                            phrase.get_seg_at(seg_pos).unwrap(), 
                            None, 
                            err_pos
                        ), 
                        pos: ins_pos
                    });
                }

                left_res.push(Action { 
                    kind: ActionKind::DeleteSegment(len), 
                    pos: seg_pos 
                });
            }
            (&MatchElement::LongSegment(seg_pos, err_pos), MatchElement::WordBound(..)) => {
                let len = Self::non_zero_len(phrase.seg_length_at(seg_pos) as u8);
                right_res.push(Action {
                    kind: ActionKind::InsertSegment(
                        len, 
                        phrase.get_seg_at(seg_pos).unwrap(), 
                        None, 
                        err_pos
                    ),
                    pos: ins_pos,
                });

                left_res.push(Action { 
                    kind: ActionKind::DeleteSegment(len), 
                    pos: seg_pos 
                });
            }
            
            (&MatchElement::Syllable(wp, sp, _), MatchElement::Segment(..)) |
            (&MatchElement::Syllable(wp, sp, _), MatchElement::LongSegment(..)) |
            (&MatchElement::Syllable(wp, sp, _), MatchElement::Syllable(..)) |
            (&MatchElement::Syllable(wp, sp, _), MatchElement::SyllBound(..)) | 
            (&MatchElement::Syllable(wp, sp, _), MatchElement::WordBound(..)) => {
                right_res.push(Action { 
                    kind: ActionKind::InsertSyllable(phrase[wp].syllables[sp].clone()), 
                    pos: ins_pos
                });

                left_res.push(Action { 
                    kind: ActionKind::DeleteSyllable, 
                    pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 } 
                });
            }

            (&MatchElement::SyllBound(wp, bp, _), MatchElement::Segment(..) | MatchElement::LongSegment(..)) => {
                right_res.push(Action { 
                    kind: ActionKind::InsertBoundary, 
                    pos: ins_pos
                });

                left_res.push(Action { 
                    kind: ActionKind::DeleteBoundary, 
                    pos: SegPos { word_index: wp, syll_index: bp, seg_index: 0 } 
                });
            }
            (MatchElement::SyllBound(..), MatchElement::Syllable (..)) => { /* Do Nothing */ }
            (MatchElement::SyllBound(..), MatchElement::SyllBound(..)) => { /* Do Nothing */ }
            (MatchElement::SyllBound(..), MatchElement::WordBound(..)) => { /* Do Nothing */ }
            (MatchElement::WordBound(..), MatchElement::WordBound(..)) => { /* Do Nothing */ }

            (&MatchElement::WordBound(wp, _), _) => {
                right_res.push(Action { 
                    kind: ActionKind::InsertWordBound, 
                    pos: ins_pos
                });

                left_res.push(Action { 
                    kind: ActionKind::DeleteWordBound, 
                    pos: SegPos { word_index: wp, syll_index: 0, seg_index: 0 } 
                });
            },

            (MatchElement::Set(..), _) | (_, MatchElement::Set(..)) => unreachable!("Sets are flattened"),
        }

        Ok(())
    }

    fn metathesis_ellipses_groups<'a>(&self, elements: Vec<Vec<&'a MatchElement>>) -> Result<Vec<MetaGroup<'a>>, RuleRuntimeError> {
        let mut match_els = Vec::new();

        for z in 0..(elements.len()/2) {
            let left_group = &elements[z];
            let right_group = &elements[elements.len()-1-z];

            debug_assert!(!left_group.is_empty());
            debug_assert!(!right_group.is_empty());

            match (left_group.len().cmp(&right_group.len()), self.rule_type) {
                (std::cmp::Ordering::Equal, RuleType::Metathesis) => {
                    for (a, b) in left_group.iter().zip(right_group.iter().rev()) {
                        match_els.push(MetaGroup(Meta::Some(a), Meta::Some(b)));
                    }
                },
                (std::cmp::Ordering::Equal, RuleType::MetaOrdered) => {
                    for (a, b) in left_group.iter().zip(right_group.iter()) {
                        match_els.push(MetaGroup(Meta::Some(a), Meta::Some(b)));
                    }
                },
                //  L > R &
                (std::cmp::Ordering::Greater, RuleType::Metathesis) => {
                    let last_el = right_group.last().unwrap();

                    for item in left_group.iter().take(left_group.len() - right_group.len()) {
                        match_els.push(MetaGroup(Meta::Some(item), Meta::Ref(last_el)));
                    }
                    
                    for (a , b) in (left_group.iter().rev().zip(right_group.iter())).rev() {
                        match_els.push(MetaGroup(Meta::Some(a), Meta::Some(b)));
                    }
                },
                // L > R @
                (std::cmp::Ordering::Greater, RuleType::MetaOrdered) => {
                    let last_el = right_group.last().unwrap();
                    
                    let right_len = right_group.len();
                    
                    for i in (0..left_group.len()).rev() {
                        if i > right_len - 1 {
                            match_els.push(MetaGroup(Meta::Some(left_group[i]), Meta::Ref(last_el)));
                        } else {
                            match_els.push(MetaGroup(Meta::Some(left_group[i]), Meta::Some(right_group[i])));
                        }
                    }
                }
                // L < R & 
                (std::cmp::Ordering::Less, RuleType::Metathesis) => {
                    let first_el = left_group.first().unwrap();
                    let left_len = left_group.len();

                    for i in (0..right_group.len()).rev() {
                        if i > left_len - 1 {
                            match_els.push(MetaGroup(Meta::Ref(first_el), Meta::Some(right_group[i])));
                        } else {
                            match_els.push(MetaGroup(Meta::Some(left_group[left_len-i-1]), Meta::Some(right_group[i])));
                        }
                    }
                },
                // L < R @
                (std::cmp::Ordering::Less, RuleType::MetaOrdered) => {
                    let first_el = left_group.first().unwrap();
    
                    for i in (0..right_group.len()).rev() {
                        if i > left_group.len() - 1 {
                            match_els.push(MetaGroup(Meta::Ref(first_el), Meta::Some(right_group[right_group.len() - i - 1])));
                        } else {
                            match_els.push(MetaGroup(Meta::Some(left_group[left_group.len()-i-1]), Meta::Some(right_group[right_group.len() - i - 1])));
                        }
                    }
                }

                _ => unreachable!()
            }
        }


        if !elements.len().is_power_of_two() && self.rule_type != RuleType::MetaOrdered {
            let middle_group = &elements[elements.len()/2];

            for z in 0..(middle_group.len()/2) {
                match_els.push(MetaGroup(Meta::Some(middle_group[z]), Meta::Some(middle_group[middle_group.len()-1-z])));
            }
        }

        Ok(match_els)
    }

    fn metathesis_ellipses(&self, phrase: &Phrase, input: &[MatchElement], parts: &[&[ParseItem]], next_pos: &mut Option<SegPos>) -> Result<Phrase, RuleRuntimeError> {
        let mut els = Vec::new();
        let mut ind = 0;
        for part in parts.iter() {
            let mut el = Vec::new();
            for _ in part.iter() {
                match &input[ind] {
                    MatchElement::Set(match_elements, _, _) => {
                        for me in match_elements {
                            el.push(me);
                        }
                    },
                    me => {
                        el.push(me);
                    }
                }
                ind += 1;
            }
            els.push(el);
        }
        debug_assert_eq!(parts.len(), els.len());


        let match_groups = self.metathesis_ellipses_groups(els)?;

        let actions = self.metathesis_gen_actions(phrase, match_groups)?;
        let (res_phrase, last_syll_len_change) = self.apply_actions(phrase, &actions)?;
        
        if let Some(next) = next_pos && let Some(last_action) = actions.last() {
            match self.calc_next_pos(last_action, &res_phrase, phrase, last_syll_len_change) {
                Some(np) => *next = np,
                None => *next_pos = None,
            }
        }
        Ok(res_phrase)
    }

    pub(super) fn metathesis(&self, phrase: &Phrase, matched_elements: &[MatchElement], next_pos: &mut Option<SegPos>) -> Result<Phrase, RuleRuntimeError> {
        
        let input_parts = self.input
            .split(|el| el.kind == ParseElement::Ellipsis || el.kind == ParseElement::OptEllipsis)
            .filter(|part| !part.is_empty()).collect::<Vec<_>>();
        
        debug_assert_eq!(input_parts.clone().into_iter().flatten().collect::<Vec<_>>().len(), matched_elements.len());

        match input_parts.len().cmp(&1) {
            std::cmp::Ordering::Greater => return self.metathesis_ellipses(phrase, matched_elements, &input_parts, next_pos),
            std::cmp::Ordering::Less    => return Ok(phrase.clone()),
            std::cmp::Ordering::Equal   => {},
        }

        let mut flattened_els = Vec::with_capacity(matched_elements.len());

        matched_elements.iter().for_each(|matched_element| {
            match matched_element {
                MatchElement::Set(els, _, _) => {
                    els.iter().for_each(|el| {
                        flattened_els.push(el);
                    });
                }
                el => flattened_els.push(el),
            }
        });

        // let flattened_els: Vec<&MatchElement> = matched_elements.iter().flat_map(|matched_el| match matched_el {
        //     MatchElement::Set(els, ..) => els,
        //     el => vec![el],
        // }).collect::<Vec<_>>();


        let mut matched_groups = Vec::new();
        for z in 0..(flattened_els.len()/2) {
            matched_groups.push(MetaGroup(Meta::Some(flattened_els[z]), Meta::Some(flattened_els[flattened_els.len()-1-z])));
        }

        let actions = self.metathesis_gen_actions(phrase, matched_groups)?;
        let (res_phrase, last_syll_len_change) = self.apply_actions(phrase, &actions)?;
        
        if let Some(next) = next_pos && let Some(last_action) = actions.last() {
            match self.calc_next_pos(last_action, &res_phrase, phrase, last_syll_len_change) {
                Some(np) => *next = np,
                None => *next_pos = None,
            }
        }
        // exit(1);
        Ok(res_phrase)
    }
}
