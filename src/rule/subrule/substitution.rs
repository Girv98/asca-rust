use std::num::NonZeroU8;

use super :: {
    Action, ActionKind, MatchElement, Payload, RefKind, SubRule
};

use crate :: {
    error :: RuleRuntimeError, 
    rule  :: { Modifiers, ParseElement, ParseItem, Position, Reference, SpecMod },
    word  :: { Phrase, SegPos, Segment, Syllable, Tone }
};


impl SubRule {
    pub(super) const ONE: NonZeroU8 = NonZeroU8::new(1).unwrap();
    
    pub(super) fn non_zero_len(seg_len: u8) -> NonZeroU8 { NonZeroU8::new(seg_len).unwrap_or(Self::ONE) }

    pub(super) fn substitution(&self, phrase: &Phrase, input: &[MatchElement], next_pos: &mut Option<SegPos>) -> Result<Phrase, RuleRuntimeError> {
        let input_filt = self.input.iter().filter(|x| x.kind != ParseElement::Ellipsis && x.kind != ParseElement::OptEllipsis).cloned().collect::<Vec<_>>();
        let output_filt = self.output.iter().filter(|x| x.kind != ParseElement::Ellipsis && x.kind != ParseElement::OptEllipsis).cloned().collect::<Vec<_>>();

        // Just in case input/output is somehow only ellipses TODO: probs should error
        if input_filt.is_empty() || output_filt.is_empty() { return Ok(phrase.clone()) }

        if self.input.len() != input_filt.len() || self.output.len() != output_filt.len() {
            return self.substitution_ellipses(phrase, input, next_pos)
        }
        
        let actions = self.substitution_gen_actions(phrase, &input_filt, &output_filt, input)?;
        let (res_phrase, last_syll_len_change) = self.apply_actions(phrase, &actions)?;
        
        if let Some(next) = next_pos && let Some(last_action) = actions.last() {
            match self.calc_next_pos(last_action, &res_phrase, phrase, last_syll_len_change) {
                Some(np) => *next = np,
                None => *next_pos = None,
            }
        }

        Ok(res_phrase)
    }

    fn substitution_ellipses(&self, phrase: &Phrase, input: &[MatchElement], next_pos: &mut Option<SegPos>) -> Result<Phrase, RuleRuntimeError> {
        // Ellipses change the semantics
        // E.G. p...k > pf...g  is different to p...k > p...fg

        let mut in_parts = Vec::new();
        for x in self.input.split(|x| x.kind == ParseElement::Ellipsis || x.kind == ParseElement::OptEllipsis) {
            in_parts.push(x);
        }

        let mut out_parts = Vec::new();
        for x in self.output.split(|x| x.kind == ParseElement::Ellipsis || x.kind == ParseElement::OptEllipsis) {
            out_parts.push(x);
        }

        if in_parts.len() != out_parts.len() {
            let all_pos = self.input.iter().chain(self.output.iter()).filter_map(|x| {
                if x.kind == ParseElement::Ellipsis || x.kind == ParseElement::OptEllipsis {
                    Some(x.position)
                } else {
                    None
                }
            }).collect::<Vec<_>>();
            return Err(RuleRuntimeError::UnevenEllipsis(all_pos))
        }

        let mut actions: Vec<Action> = Vec::with_capacity(input.len());
        let mut input = input.to_owned();

        for (i, o) in in_parts.iter().zip(out_parts.iter()) {
            actions.extend(self.substitution_gen_actions(phrase, i, o, &input)?);
            
            for _ in 0..i.len() {
                input.remove(0);
            }
        }
        
        let (res_phrase, last_syll_len_change) = self.apply_actions(phrase, &actions)?;

        if let Some(next) = next_pos && let Some(last_action) = actions.last() {
            match self.calc_next_pos(last_action, &res_phrase, phrase, last_syll_len_change) {
                Some(np) => *next = np,
                None => *next_pos = None,
            }
        }

        Ok(res_phrase)
    }
    
    fn sub_structure(&self, phrase: &Phrase, items: &[ParseItem], stress: &Option<SpecMod>, tone: &Option<Tone>, refr: &Option<usize>, state: &MatchElement, out_pos: Position) -> Result<Action, RuleRuntimeError> {
        // X -> <CVC>
        if items.is_empty() { return Err(RuleRuntimeError::SubstitutionSyll(out_pos)) }
        match state {
            &MatchElement::Syllable(wp, sp, _) => {
                Ok(Action {
                    kind: ActionKind::ReplaceSyllable(self.gen_syll_from_struct(items, stress, tone, refr, out_pos, false)?),
                    pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 },
                })
            },
            &MatchElement::Segment(pos, _) => {
                Ok(Action {
                    kind: ActionKind::ReplaceSegment(
                        (Self::ONE, Self::ONE), 
                        Payload::Syllable(self.gen_syll_from_struct(items, stress, tone, refr, out_pos, false)?),
                        out_pos
                    ),
                    pos,
                })
            },
            &MatchElement::LongSegment(pos, _) => {
                Ok(Action {
                    kind: ActionKind::ReplaceSegment(
                        (Self::non_zero_len(phrase.seg_length_at(pos) as u8), Self::ONE),
                        Payload::Syllable(self.gen_syll_from_struct(items, stress, tone, refr, out_pos, false)?),
                        out_pos
                    ),
                    pos,
                })
            },
            
            MatchElement::WordBound(.., in_pos) => Err(RuleRuntimeError::SubstitutionWordBound(*in_pos, out_pos)),
            MatchElement::SyllBound(..) | MatchElement::Set(..) => unreachable!("Should be handled in gen_actions()"),
        }
    }

    fn sub_matrix(&self, phrase: &Phrase, mods: &Modifiers, refr: &Option<usize>, state: &MatchElement, out_pos: Position) -> Result<Vec<Action>, RuleRuntimeError> {
        // X -> Matrix        
        match state {
            &MatchElement::LongSegment(pos, _) => {
                let old_len = phrase.seg_length_at(pos) as u8;
                let (seg, new_len) = self.gen_seg(unsafe { phrase.get_seg_at(pos).unwrap_unchecked() }, old_len,Some(mods), refr, out_pos)?;
                Ok(vec![Action {
                    // SAFETY: old_len > 0
                    kind: ActionKind::ReplaceSegment(
                        (Self::non_zero_len(old_len), Self::non_zero_len(new_len)),
                        Payload::Segment(seg, Some(mods.suprs)),
                        out_pos,
                    ),
                    pos,
                }])
            },
            &MatchElement::Segment(pos, _) => {
                let (seg, new_len) = self.gen_seg(unsafe { phrase.get_seg_at(pos).unwrap_unchecked() }, 1,Some(mods), refr, out_pos)?;
                Ok(vec![Action {
                    kind: ActionKind::ReplaceSegment(
                        (Self::ONE, Self::non_zero_len(new_len)), 
                        Payload::Segment(seg, Some(mods.suprs)),
                        out_pos,
                    ),
                    pos,
                }])
            },
            &MatchElement::Syllable(wp, sp, _)  => {
                let mut syll = phrase[wp].syllables[sp].clone();
                syll.apply_syll_mods(&self.alphas, &mods.suprs, out_pos)?;
                if let Some(r) = refr {
                    self.references.borrow_mut().insert(*r, RefKind::Syllable(syll.clone()));
                }
                Ok(vec![Action {
                    kind: ActionKind::ReplaceSyllable(syll),
                    pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 },
                }])
            },
            MatchElement::Set(els, _, _) => {
                let mut actions = vec![];
                for el in els {
                    actions.extend(self.sub_matrix(phrase, mods, refr, el, out_pos)?);
                }
                Ok(actions)
            },
            MatchElement::WordBound(.., in_pos) => Err(RuleRuntimeError::SubstitutionWordBound(*in_pos, out_pos)),
            // This shouldn't occur
            MatchElement::SyllBound(.., in_pos)  => Err(RuleRuntimeError::SubstitutionBoundMod(*in_pos, out_pos)),
        }
    }

    fn sub_ipa(&self, phrase: &Phrase, seg: &Segment, mods: &Option<Modifiers>, state: &MatchElement, out_pos: Position) -> Result<Vec<Action>, RuleRuntimeError> {
        // X -> IPA
        match state {
            &MatchElement::LongSegment(pos, _) => {
                let suprs = mods.as_ref().map(|m| m.suprs);
                let out_has_length = if let Some(s) = suprs { s.length.is_some() } else { false };

                let old_len = phrase.seg_length_at(pos) as u8;
                let (seg, new_len) = self.gen_seg(*seg, if out_has_length { old_len } else { 1 }, mods.as_ref(), &None, out_pos)?;

                Ok(vec![Action {
                    kind: ActionKind::ReplaceSegment(
                        (Self::non_zero_len(old_len), Self::non_zero_len(new_len)), 
                        Payload::Segment(seg, suprs),
                        out_pos
                    ),
                    pos,
                }])
            },
            &MatchElement::Segment(pos, _)=> {
                let (seg, new_len) = self.gen_seg(*seg, 1,  mods.as_ref(), &None, out_pos)?;
                let suprs = mods.as_ref().map(|m| m.suprs);

                Ok(vec![Action {
                    kind: ActionKind::ReplaceSegment(
                        (Self::ONE, Self::non_zero_len(new_len)), 
                        Payload::Segment(seg, suprs),
                        out_pos
                    ),
                    pos,
                }])
            },
            
            MatchElement::WordBound(.., in_pos) => Err(RuleRuntimeError::SubstitutionWordBound(*in_pos, out_pos)),
            MatchElement::Syllable(.., in_pos)  => Err(RuleRuntimeError::SubstitutionSylltoMatrix(*in_pos, out_pos)), // TODO: This should be ok
            MatchElement::SyllBound(..) | MatchElement::Set(..) => unreachable!("Should be handled in gen_actions()"),
        }
    }

    fn sub_ref(&self, phrase: &Phrase, num: &Reference, mods: &Option<Modifiers>, state: &MatchElement, out_pos: Position) -> Result<Action, RuleRuntimeError> {
        // X -> Reference
        let binding = self.references.borrow();
        let Some(refr) = binding.get(&num.value) else { return Err(RuleRuntimeError::UnknownReference(*num)) };
        match (state, refr) {
            (&MatchElement::LongSegment(pos, _), RefKind::Segment(seg)) => {
                let old_len = phrase.seg_length_at(pos) as u8;
                let (seg, new_len) = self.gen_seg(*seg, old_len, mods.as_ref(), &None, num.position)?;
                let suprs = mods.as_ref().map(|m| m.suprs);

                Ok(Action {
                    kind: ActionKind::ReplaceSegment(
                        (Self::non_zero_len(old_len), Self::non_zero_len(new_len)),
                        Payload::Segment(seg, suprs),
                        num.position
                    ),
                    pos,
                })
            },
            (&MatchElement::Segment(pos, _), RefKind::Segment(seg)) => {
                // TODO: Pass 1 here instead of the actual seg length may cause some issues, need to test once implemented
                let (seg, new_len) = self.gen_seg(*seg, 1, mods.as_ref(), &None, num.position)?;
                let suprs = mods.as_ref().map(|m| m.suprs);

                Ok(Action {
                    kind: ActionKind::ReplaceSegment(
                        (Self::ONE, Self::non_zero_len(new_len)), 
                        Payload::Segment(seg, suprs), 
                        num.position
                    ),
                    pos,
                })
            },
            (&MatchElement::LongSegment(pos, _), RefKind::Syllable(insert_syll)) => {
                let mut syll = insert_syll.clone();
                let seg_len = phrase.seg_length_at(pos) as u8;
                if let Some(m) = mods { syll.apply_syll_mods(&self.alphas, &m.suprs, num.position)?; }

                Ok(Action {
                    kind: ActionKind::ReplaceSegment(
                        (Self::non_zero_len(seg_len), Self::ONE),
                        Payload::Syllable(syll),
                        num.position
                    ),
                    pos,
                })
            },
            (&MatchElement::Segment(pos, _), RefKind::Syllable(insert_syll)) => {
                let mut syll = insert_syll.clone();

                if let Some(m) = mods { syll.apply_syll_mods(&self.alphas, &m.suprs, num.position)?; }

                Ok(Action {
                    kind: ActionKind::ReplaceSegment(
                        (Self::ONE, Self::ONE), 
                        Payload::Syllable(syll),
                        num.position
                    ),
                    pos,
                })
            },
            (&MatchElement::Syllable(wp, sp, _), RefKind::Syllable(syll)) => {
                let mut syll = syll.clone();
                if let Some(m) = mods {
                    syll.apply_syll_mods(&self.alphas, &m.suprs, num.position)?;
                }

                Ok(Action {
                    kind: ActionKind::ReplaceSyllable(syll),
                    pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 },
                })
            },

            (MatchElement::Syllable(.., in_pos),  RefKind::Segment(..)) => Err(RuleRuntimeError::SubstitutionSylltoSeg(*in_pos, out_pos)),
            (MatchElement::WordBound(_, in_pos), _) => Err(RuleRuntimeError::SubstitutionWordBound(*in_pos, out_pos)),

            (MatchElement::SyllBound(..) | MatchElement::Set(..), ..) => unreachable!("Should be handled in gen_actions()"),
        }
    }

    fn sub_bound(&self, phrase: &Phrase, state: &MatchElement) -> Result<Vec<Action>, RuleRuntimeError> {
        // X -> $
        match state {
            MatchElement::SyllBound(wp, sp, _) => {
                Ok(vec![Action { 
                    kind: ActionKind::PassBoundary, 
                    pos: SegPos { word_index: *wp, syll_index: *sp, seg_index: 0 } 
                }])
            },
            &MatchElement::Segment(mut pos, _) => {
                let mut v = Vec::with_capacity(2);
                v.push(Action {
                    kind: ActionKind::DeleteSegment(Self::ONE),
                    pos,
                });
                pos.seg_index += 1;
                v.push(Action {
                    kind: ActionKind::InsertBoundary,
                    pos,
                });

                Ok(v)
            },
            &MatchElement::LongSegment(mut pos, _) => {
                let mut v = Vec::with_capacity(2);
                v.push(Action {
                    kind: ActionKind::DeleteSegment(Self::non_zero_len(phrase.seg_length_at(pos) as u8)),
                    pos,
                });
                pos.seg_index += phrase.seg_length_at(pos);
                v.push(Action {
                    kind: ActionKind::InsertBoundary,
                    pos,
                });

                Ok(v)
            },
            MatchElement::Syllable(wp, sp, _) => {
                Ok(vec![Action { 
                    kind: ActionKind::DeleteSyllable,
                    pos: SegPos { word_index: *wp, syll_index: *sp, seg_index: 0 },
                }])
            },
            MatchElement::Set(els, _, _) => {
                if els.len() == 1 {
                    return self.sub_bound(phrase, &els[0])
                }

                let mut actions = vec![];

                for el in els {
                    match el {
                        MatchElement::WordBound(_, _) | MatchElement::Set(..) => unimplemented!(),
                        MatchElement::SyllBound(_, _, _) => {},

                        &MatchElement::Segment(pos, _) => actions.push(Action {
                            kind: ActionKind::DeleteSegment(Self::ONE),
                            pos,
                        }),
                        &MatchElement::LongSegment(pos, _) => actions.push(Action {
                            kind: ActionKind::DeleteSegment(Self::non_zero_len(phrase.seg_length_at(pos) as u8)),
                            pos,
                        }),
                        MatchElement::Syllable(wp, sp, _) => actions.push(Action {
                            kind: ActionKind::DeleteSyllable,
                            pos: SegPos { word_index: *wp, syll_index: *sp, seg_index: 0 },
                        }),
                    }
                }

                // if last item is syllable, no need to insert boundary
                // else, insert boundary
                if let Some(el) = els.last() {
                    match el {
                        MatchElement::Set(..) | MatchElement::WordBound(_, _) | MatchElement::Syllable(..) => {},
                        MatchElement::Segment(pos, _) => actions.push(Action { 
                            kind: ActionKind::InsertBoundary,
                            pos: SegPos { word_index:pos.word_index, syll_index: pos.syll_index, seg_index: pos.seg_index + 1 }
                        }),
                        MatchElement::LongSegment(pos, _) => actions.push(Action { 
                            kind: ActionKind::InsertBoundary,
                            pos: SegPos { word_index:pos.word_index, syll_index: pos.syll_index, seg_index: pos.seg_index + phrase.seg_length_at(*pos) }
                        }),
                        MatchElement::SyllBound(wp, sp, _) => actions.push(Action { 
                            kind: ActionKind::PassBoundary, 
                            pos: SegPos { word_index: *wp, syll_index: *sp, seg_index: 0 } 
                        }),
                    }
                }

                Ok(actions)
            },
            MatchElement::WordBound(..) => unreachable!("Should be dealt with in gen_actions()"),
        }
    }

    fn gen_seg(&self, seg: Segment, old_len: u8, mods: Option<&Modifiers>, refr: &Option<usize>, err_pos: Position) -> Result<(Segment, u8), RuleRuntimeError> {
        let mut seg = seg;
        let mut new_len = old_len;
        if let Some(m) = mods {
            seg.apply_seg_mods(&self.alphas, m.nodes, m.feats, err_pos, false)?;
            new_len = Syllable::calc_new_length(&self.alphas, &m.suprs, old_len, err_pos)?;
        }

        if let Some(r) = refr {
            self.references.borrow_mut().insert(*r, RefKind::Segment(seg));
        }

        Ok((seg, new_len))
    }

    fn sub_set_insertion_position(&self, insert_pos: &mut Option<SegPos>, actions: &[Action]) {
        if insert_pos.is_none() {
            let last_action = actions.last().expect("not first item");
            let SegPos { word_index, syll_index, seg_index } = last_action.pos;
            
            *insert_pos = Some(match &last_action.kind {
                ActionKind::DeleteSyllable     |
                ActionKind::ModifySyllable(..) |
                ActionKind::ReplaceSyllable(..) => SegPos { word_index, syll_index: syll_index+1, seg_index: 0 },
                
                ActionKind::DeleteSegment(old_len) |
                ActionKind::ReplaceSegment((old_len, _), ..) => SegPos { word_index, syll_index, seg_index: seg_index + old_len.get() as usize },
                
                ActionKind::PassBoundary => last_action.pos,
                ActionKind::DeleteBoundary => last_action.pos,

                ActionKind::InsertBoundary => last_action.pos,
                ActionKind::DeleteWordBound |
                ActionKind::InsertWordBound |
                ActionKind::InsertSyllable(..) |
                ActionKind::InsertWord(..) | // NOTE: This is will never happen because of line 120 in self.apply()
                ActionKind::InsertSegment(..) => last_action.pos,
            });
        }
    }

    fn substitution_gen_actions(&self, phrase: &Phrase, input_filt: &[ParseItem], output_filt: &[ParseItem], input: &[MatchElement]) -> Result<Vec<Action>, RuleRuntimeError> {
        // FIXME: This shouldn't be happening
        if input_filt.len() > input.len() || input_filt.is_empty() || input.is_empty() {
            // Should error if input_filt is empty as this means we are inserting between empty ellipses e.g. p.. ..p > p..p..p
            return Ok(vec![])
        }

        let mut actions = Vec::with_capacity(input_filt.len());
        let mut insert_pos: Option<SegPos> = None;

        let mut in_index = 0;
        let mut out_index = 0;

        loop {
            match (input_filt.get(in_index), output_filt.get(out_index)) {
                (None, None) => break,
                (None, Some(out_item)) => {
                    self.sub_set_insertion_position(&mut insert_pos, &actions);
                
                    match &out_item.kind {
                        ParseElement::Ellipsis   | ParseElement::OptEllipsis | ParseElement::MetaOrdered  |
                        ParseElement::EmptySet   | ParseElement::WordBound   | ParseElement::Optional(..) | 
                        ParseElement::Metathesis | ParseElement::ExtlBound   | ParseElement::Negation(..) => unreachable!(),

                        ParseElement::Syllable(..) => return Err(RuleRuntimeError::SubstitutionSyll(out_item.position)),
                        ParseElement::Set(_) => return Err(RuleRuntimeError::LonelySet(out_item.position)),
                        ParseElement::Matrix(..) => return Err(RuleRuntimeError::InsertionMatrix(out_item.position)),

                        ParseElement::Ipa(segment, mods) => {
                            let pos = insert_pos.expect("insert_pos is set");
                            let (seg, len) = self.gen_seg(*segment, 1, mods.as_ref(), &None, out_item.position)?;
                            let suprs = mods.as_ref().map(|m| m.suprs);

                            actions.push(Action { 
                                kind: ActionKind::InsertSegment(Self::non_zero_len(len), seg, suprs, out_item.position), 
                                pos
                            });
                            out_index += 1;
                        }
                        ParseElement::Structure(items, stress, tone, refr) => {
                            if items.is_empty() { return Err(RuleRuntimeError::SubstitutionSyll(out_item.position)) }
                            let pos = insert_pos.expect("insert_pos is set");
                            let syll = self.gen_syll_from_struct(items, stress, tone, refr, out_item.position, true)?;

                            actions.push(Action { 
                                kind: ActionKind::InsertSyllable(syll), 
                                pos
                            });
                            out_index += 1;
                        }
                        ParseElement::Reference(num, mods) => {
                            let binding = self.references.borrow();
                            let Some(refr) = binding.get(&num.value) else { return Err(RuleRuntimeError::UnknownReference(*num)) };
                            match refr {
                                RefKind::Segment(segment) => {
                                    let pos = insert_pos.expect("insert_pos is set");
                                    let (seg, len) = self.gen_seg(*segment, 1, mods.as_ref(), &None, num.position)?;
                                    let suprs = mods.as_ref().map(|m| m.suprs);

                                    actions.push(Action {
                                        kind: ActionKind::InsertSegment(Self::non_zero_len(len), seg, suprs, num.position), 
                                        pos,
                                    });
                                },
                                RefKind::Syllable(insert_syll) => {
                                    let pos = insert_pos.expect("insert_pos is set");
                                    let mut syll = insert_syll.clone();
                                    if let Some(m) = mods { syll.apply_syll_mods(&self.alphas, &m.suprs, num.position)?; }

                                    actions.push(Action {
                                        kind: ActionKind::InsertSyllable(syll), 
                                        pos,
                                    });

                                },
                            }
                            out_index += 1;
                        }
                        ParseElement::SyllBound => {
                            actions.push(Action {
                                kind: ActionKind::InsertBoundary,
                                pos: insert_pos.unwrap_or(actions.last().unwrap().pos),
                            });
                            out_index += 1;
                        }
                    }
                },
                (Some(_), None) => match &input[in_index] {
                    &MatchElement::Segment(pos, _) | &MatchElement::LongSegment(pos, _) => {
                        let seg_len = phrase.seg_length_at(pos) as u8;
                        actions.push(Action { kind: ActionKind::DeleteSegment(Self::non_zero_len(seg_len)), pos });
                        in_index += 1;
                    }
                    &MatchElement::Syllable(wp, sp, _)  => {
                        actions.push(Action { kind: ActionKind::DeleteSyllable, pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 } });
                        in_index += 1;
                    }
                    &MatchElement::SyllBound(wp, sp, _) => {
                        actions.push(Action { kind: ActionKind::DeleteBoundary, pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 } });
                        in_index += 1;
                    }
                    MatchElement::Set(els, _, _) => {
                        for el in els {
                            match el {
                                &MatchElement::Segment(pos, _) | &MatchElement::LongSegment(pos, _) => {
                                    let seg_len = phrase.seg_length_at(pos) as u8;
                                    actions.push(Action { kind: ActionKind::DeleteSegment(Self::non_zero_len(seg_len)), pos });
                                },
                                &MatchElement::Syllable(wp, sp, _)  => {
                                    actions.push(Action { kind: ActionKind::DeleteSyllable, pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 } });
                                }
                                &MatchElement::SyllBound(wp, sp, _) => {
                                    actions.push(Action { kind: ActionKind::DeleteBoundary, pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 } });
                                }
                                MatchElement::WordBound(_, _) => return Err(RuleRuntimeError::SubstitutionWordBound(input_filt[in_index].position, output_filt.last().expect("Output isn't empty").position)),
                                MatchElement::Set(..) => unimplemented!("Sets cannot be nested"),
                            }
                        }
                        in_index += 1;
                    }
                    MatchElement::WordBound(_, _) => return Err(RuleRuntimeError::SubstitutionWordBound(input_filt[in_index].position, output_filt.last().expect("Output isn't empty").position)),
                },
                (Some(_), Some(out_item)) => {
                    let in_item = &input_filt[in_index];
                    let match_el = &input[in_index];
                    match (match_el, &out_item.kind) {
                        (_, ParseElement::Ellipsis )  | (_, ParseElement::OptEllipsis) | (_, ParseElement::EmptySet)     | 
                        (_, ParseElement::ExtlBound)  | (_, ParseElement::MetaOrdered) | (_, ParseElement::Optional(..)) | 
                        (_, ParseElement::Metathesis) | (_, ParseElement::WordBound)   | (_, ParseElement::Negation(..)) => unreachable!(),

                        (_, ParseElement::Syllable(..)) => return Err(RuleRuntimeError::SubstitutionSyll(out_item.position)),
                        (MatchElement::WordBound(..), _) => return Err(RuleRuntimeError::SubstitutionWordBound(in_item.position, out_item.position)),

                        (MatchElement::Set(items, set_index, _), ParseElement::Set(set_output)) => {
                            let ParseElement::Set(ref set_input) = in_item.kind else { unreachable!() };
                            // let ParseElement::Set(ref set_output) = out_item.kind else { unreachable!() };

                            if set_input.choices.len() != set_output.choices.len() { return Err(RuleRuntimeError::UnevenSet(in_item.position, out_item.position)) }

                            let input_items = &set_input.choices[*set_index].items;
                            let output_items = &set_output.choices[*set_index].items;

                            actions.extend(self.substitution_gen_actions(phrase, input_items, output_items, items)?);
                            in_index += 1; out_index += 1;
                        }

                        (MatchElement::Set(items, set_index, _), _) => {
                            let ParseElement::Set(ref input_set) = in_item.kind else { unreachable!() };
                            let input_items = &input_set.choices[*set_index].items;

                            actions.extend(self.substitution_gen_actions(phrase, input_items, std::slice::from_ref(out_item), items)?);
                            in_index += 1; out_index += 1;
                        }
                        (_, ParseElement::Set(_)) => unreachable!(),

                        (_, ParseElement::SyllBound) => {
                            actions.extend(self.sub_bound(phrase, match_el)?);
                            in_index += 1; out_index += 1;
                        }
                        // Note: Must be placed after sub_bound and before everything else
                        (&MatchElement::SyllBound(wp, sp, _), _) => {
                            actions.push(Action { 
                                kind: ActionKind::DeleteBoundary, 
                                pos: SegPos { word_index: wp, syll_index: sp, seg_index: 0 },
                            });
                            in_index += 1;
                        }
                        (_, ParseElement::Structure(items, stress, tone, refr)) => {
                            actions.push(self.sub_structure(phrase, items, stress, tone, refr, match_el, out_item.position)?);
                            in_index += 1; out_index += 1;
                        }
                        (_, ParseElement::Matrix(mods, refr)) => { 
                            actions.extend(self.sub_matrix(phrase, mods, refr, match_el, out_item.position)?);
                            in_index += 1; out_index += 1;
                        }
                        (_, ParseElement::Ipa(seg, mods) )=> {
                            actions.extend(self.sub_ipa(phrase, seg, mods, match_el, out_item.position)?);
                            in_index += 1; out_index += 1;
                        }
                        (_, ParseElement::Reference(num, mods)) => {
                            actions.push(self.sub_ref(phrase, num, mods, match_el, out_item.position)?);
                            in_index += 1; out_index += 1;
                        }
                    }
                }
            }
        }
        
        Ok(actions)
    }
}
