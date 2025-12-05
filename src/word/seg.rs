use std::{cell::RefCell, collections::HashMap, fmt};
use serde::{Deserialize, Serialize};

use crate  :: {
    error  :: RuleRuntimeError, 
    rule   :: { Alpha, AlphaMod, BinMod, ModKind, Modifiers, PlaceMod, Position, SupraSegs }, 
    word   :: { Diacritic, Place }, 
    CARDINALS_MAP, DIACRITS 
};
use super  :: { DiaMods, FeatKind, NodeKind };

/// Representation of a phonemic segment
/// # Nodes and their features
/// * `root` - Only lower three bits used: `[consonantal, sonorant, syllabic]`
/// * `manner` - All bits used: `[continuent, approx., lateral, nasal, del.rel., strident, rhotic, click]`
/// * `laryngeal` - Only lower three bits used: `[voice, s.g., c.g.]`
/// * `place` - See [Place]
#[derive(Default, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)]
pub struct Segment {
    pub root      : u8,
    pub manner    : u8,
    pub laryngeal : u8,
    pub place: Place,
}
 
impl fmt::Display for Segment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get_as_grapheme().unwrap_or("�".to_owned()))
    }
}

impl fmt::Debug for Segment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn u8_to_str(f: &mut fmt::Formatter<'_>, num: u8, len: u8) -> fmt::Result {
            for i in (0..len).rev() {
                match num & (1 << i) != 0 {
                    true => write!(f, "+", )?,
                    false => write!(f, "-", )?,
                }
            }
            Ok(())
        }
        u8_to_str(f, self.root, 3)?;
        write!(f, "|")?;
        u8_to_str(f, self.manner, 8)?;
        write!(f, "|")?;
        u8_to_str(f, self.laryngeal, 3)?;
        write!(f, "|")?;
        write!(f, "{:?}", self.place)
    }
}

impl Segment {
    fn sort_and_filter_candidates(&self, candidates: Vec<(Segment, &String, usize)>) -> String {   
        debug_assert!(candidates.len() > 1);     
        let mut candidates = candidates;
        
        candidates.sort_by(|(.., a), (.., b) | a.cmp(b));

        candidates[0].1.to_string()

        // TODO

        // let best_diff = candidates[0].2;

        // candidates.retain(| (.., diff) | *diff == best_diff);

        // if candidates.len() == 1 { return candidates[0].1.to_string() }

        // && candidates[0].2 == candidates[1].2 

    }

    fn create_candidates(&self) -> Vec<(Segment, &String, usize)> {
        let mut candidates = Vec::with_capacity(64);
        for (c_grapheme, seg) in CARDINALS_MAP.iter() {
            let diff_count = self.diff_count(seg);
            if diff_count < 8 {
                candidates.push((*seg, c_grapheme, diff_count))
            }
        }
        candidates
    }

    pub(crate) fn get_nearest_grapheme(&self) -> String {
        // test against all cardinal values for a match
        for (c_grapheme, seg) in CARDINALS_MAP.iter() {
            if *seg == *self { return c_grapheme.to_string() }
        }

        let candidates = self.create_candidates();
                
        match candidates.len().cmp(&1) {
            std::cmp::Ordering::Greater => self.sort_and_filter_candidates(candidates),
            std::cmp::Ordering::Less => self.get_as_grapheme().unwrap_or("�".to_owned()),
            std::cmp::Ordering::Equal => candidates[0].1.to_string(),
        }
    }

    /// Returns the segment as a string in IPA form
    /// 
    /// Returns `Err`, if the segment value cannot be converted, 
    /// containing a string representation of the segment's values 
    pub fn try_as_grapheme(&self) -> Result<String, String> {
        match self.get_as_grapheme() {
            Some(grapheme) => Ok(grapheme),
            None => Err(format!("{self:?}")),
        }
    }

    /// Returns the segment as a string in IPA form
    /// 
    /// Returns `None`, if the segment value cannot be converted
    pub fn get_as_grapheme(&self) -> Option<String> {
        // TODO(girv): This wouldn't get me any leetcode cred

        // test against all cardinal values for a match
        for (c_grapheme, seg) in CARDINALS_MAP.iter() {
            if *seg == *self { return Some(c_grapheme.to_string()) }
        }

        // if no match is found, 
        // loop through again, but this time test cardinal + diacritics

        // Sort by difference (filter out diff >= 8 to cut on size)
        // iterate and match starting from smallest difference

        // if let Some(m) = self.try_edge_cases() { return Some(m) }

        let mut candidates = self.create_candidates();
        candidates.sort_by(|(.., a), (.., b) | a.cmp(b));

        // let x = candidates[0].2;
        // for (a,_, b) in &candidates {
        //     if *b == x {
        //         eprint!("{} {} ", a.get_as_grapheme().unwrap(), b);
        //     }
        // }
        // println!();
        
        for (candidate_segment , cand_graph, _) in candidates {
            let mut current_segment = candidate_segment;
            let mut buffer = cand_graph.clone();
            for d in DIACRITS.iter() {
                if self.match_modifiers(&d.prereqs).is_ok() && self.match_modifiers(&d.payload).is_ok() {
                    let before = current_segment;
                    current_segment.apply_diacritic_payload(&d.payload);
                    if current_segment == before || current_segment == candidate_segment {
                        continue;
                    } 
                    buffer.push(d.diacrit);
                }
                if current_segment == *self { 
                    return Some(buffer);
                }
            }
        }

        None
    }

    pub(crate) fn match_modifiers(&self, mods: &DiaMods) -> Result<(), (usize, bool)> {
        for (i, m) in mods.feats.iter().enumerate() {
            if !self.match_feat_mod(m, i) { return Err((i, false)) }
        }
        for (i, m) in mods.nodes.iter().enumerate() {
            if !self.match_node_mod(m, i) { return Err((i, true)) }
        }
        Ok(())
    }

    pub(crate) fn match_node_mod(&self, md: &Option<ModKind>, node_index: usize) -> bool {
        let Some(kind) = md else { return true };
        self.match_node_mod_kind(kind, NodeKind::from_usize(node_index))
    }

    pub(crate) fn match_feat_mod(&self, md: &Option<ModKind>, feat_index: usize) -> bool {
        let Some(kind) = md else { return true };
        let (node, mask) = FeatKind::from_usize(feat_index).as_node_mask();
        self.match_feat_mod_kind(kind, node, mask)
    }

    pub(crate) fn match_node_mod_kind(&self, kind: &ModKind, node: NodeKind) -> bool {
        // NOTE: Alphas don't make sense here
        // this is a consequence of using ModKind
        let ModKind::Binary(bt) = kind else { unreachable!() };
        match bt {
            BinMod::Negative => self.is_node_none(node),
            BinMod::Positive => self.is_node_some(node),
        }
    }

    pub(crate) fn match_feat_mod_kind(&self, kind: &ModKind, node: NodeKind, mask: u8) -> bool {
        // NOTE: Alphas don't make sense here
        // this is a consequence of using ModKind
        let ModKind::Binary(bt) = kind else { unreachable!() };
        match bt {
            BinMod::Negative => self.feat_match(node, mask, false),
            BinMod::Positive => self.feat_match(node, mask, true),
        }
    }

    pub(crate) fn as_modifiers(&self) -> Modifiers {
        let as_bin_mod = |b: bool | if b { BinMod::Positive } else { BinMod::Negative };
        
        let nodes = [
            Some(ModKind::Binary(BinMod::Positive)),
            Some(ModKind::Binary(BinMod::Positive)),
            Some(ModKind::Binary(BinMod::Positive)),
            Some(ModKind::Binary(as_bin_mod(self.place.is_some()))),
            Some(ModKind::Binary(as_bin_mod(self.place.labial_is_some()))),
            Some(ModKind::Binary(as_bin_mod(self.place.coronal_is_some()))),
            Some(ModKind::Binary(as_bin_mod(self.place.dorsal_is_some()))),
            Some(ModKind::Binary(as_bin_mod(self.place.pharyngeal_is_some()))),
        ];

        let mut feats = [();FeatKind::count()].map(|_| None);     
        for (i, feat) in feats.iter_mut().enumerate() {
            let (nk, f) = FeatKind::from_usize(i).as_node_mask();   
            let Some(x) = self.get_feat(nk, f) else { continue };
            
            *feat = Some(ModKind::Binary(as_bin_mod(x != 0)))
        }
        
        Modifiers { nodes, feats, suprs: SupraSegs::new() }
    }

    fn diff_count(&self, other: &Segment) -> usize {
        let diff_option = |f: Place, s: Place| {
            let Some(a) = *f else { return *s };
            let Some(b) = *s else { return *f };
            Some(a ^ b)
        };

        let rut = self.root ^ other.root;
        let man = self.manner ^ other.manner;
        let lar = self.laryngeal ^ other.laryngeal;
        let plc = diff_option(self.place, other.place).unwrap_or(0);

        (rut.count_ones() + man.count_ones() + lar.count_ones() + plc.count_ones()) as usize
    }

    /// Returns the value of a given node or sub-node
    /// # Arguments
    /// * `node` - The node to return
    /// # Panics
    /// Panics if `node` is [NodeKind::Place]
    #[inline]
    pub fn get_node(&self, node: NodeKind) -> Option<u8> {
        match node {
            NodeKind::Root       => Some(self.root),
            NodeKind::Manner     => Some(self.manner),
            NodeKind::Laryngeal  => Some(self.laryngeal),
            NodeKind::Labial     => self.place.get_labial(),
            NodeKind::Coronal    => self.place.get_coronal(),
            NodeKind::Dorsal     => self.place.get_dorsal(),
            NodeKind::Pharyngeal => self.place.get_pharyngeal(),
            NodeKind::Place      => panic!("`Place` cannot be accessed this way. Use `get_place_nodes()` instead."),
        }
    }

    /// Returns the place node
    #[inline]
    pub fn get_place_node(&self) -> Place { self.place }

    /// Returns the individual values of the four place nodes
    #[inline]
    pub fn get_place_sub_nodes(&self) -> (Option<u8>, Option<u8>, Option<u8>, Option<u8>) {
        (self.place.get_labial(), self.place.get_coronal(), self.place.get_dorsal(), self.place.get_pharyngeal())
    }
    
    /// Sets a given node or sub-node to given value
    /// # Arguments
    /// * `node`  - The node to set
    /// * `value` - The value to set
    /// # Panics
    /// Panics if `node` is [NodeKind::Place]
    #[inline]
    pub fn set_node(&mut self, node: NodeKind, value: Option<u8>) {
        match node {
            NodeKind::Root       => self.root = value.expect("RootNode cannot be null"),
            NodeKind::Manner     => self.manner = value.expect("MannerNode cannot be null"),
            NodeKind::Laryngeal  => self.laryngeal = value.expect("LaryngealNode cannot be null"),
            NodeKind::Labial     => self.place.set_labial(value),
            NodeKind::Coronal    => self.place.set_coronal(value),
            NodeKind::Dorsal     => self.place.set_dorsal(value),
            NodeKind::Pharyngeal => self.place.set_pharyngeal(value),
            NodeKind::Place      => panic!("`Place` cannot be set this way. Set each subnode individually instead."),
        }
    }
    
    /// Returns the values of a given set of features within a node or sub-node
    /// # Arguments
    /// * `node` - The node to get
    /// * `feat` - A bitmask of the feature values to return
    /// # Panics
    /// Panics if `node` is [NodeKind::Place]
    #[inline]
    pub fn get_feat(&self, node: NodeKind, feat: u8) -> Option<u8> {
        Some(self.get_node(node)? & feat)
    }
    
    /// Sets a given set of features within a `node` or sub-node
    /// # Arguments
    /// * `node` - The node to set
    /// * `feat` - A bitmask of the feature values to set
    /// * `to_positive` - Set the feature values to `0` or `1`
    /// # Panics
    /// Panics if `node` is [NodeKind::Place]
    #[inline]
    pub fn set_feat(&mut self, node: NodeKind, feat: u8, to_positive: bool) {
        if to_positive {
            let n = self.get_node(node).unwrap_or(0u8);
            self.set_node(node, Some(n | feat)) 
        } else if let Some(n) = self.get_node(node) {
            self.set_node(node, Some(n & !(feat)))
        }
    }
    
    /// Returns whether a given node or sub-node is a `Some` value
    /// # Arguments
    /// * `node` - The node to query
    /// # Panics
    /// Panics if `node` is [NodeKind::Place]
    #[inline]
    pub fn is_node_some(&self, node: NodeKind) -> bool {
        self.get_node(node).is_some()
    }
    
    /// Returns whether a given node or sub-node is a `None` value
    /// # Arguments
    /// * `node` - The node to query
    /// # Panics
    /// Panics if `node` is [NodeKind::Place]
    #[inline]
    pub fn is_node_none(&self, node: NodeKind) -> bool {
        self.get_node(node).is_none()
    }
    
    /// Returns whether place is a `Some` value
    #[inline]
    pub fn is_place_some(&self) -> bool {
        self.place.is_some()
    }

    /// Returns whether place is a `None` value
    #[inline]
    pub fn is_place_none(&self) -> bool {
        self.place.is_none()
    }

    /// Returns whether a given set of features within a node or sub-node are positive
    /// # Arguments
    /// * `node` - The node to match
    /// * `mask` - A bitmask of the feature values to match
    /// * `positive` - Match if feature values are positive
    /// # Panics
    /// Panics if `node` is [NodeKind::Place]
    #[inline]
    pub fn feat_match(&self, node: NodeKind, mask: u8, positive: bool) -> bool {
        let Some(n) = self.get_node(node) else {
            return false
        };
        if positive {
            n & mask == mask
        } else {
            n & mask == 0
        }
    }

    /// Returns whether a given node or sub-node matches a given value
    /// # Arguments
    /// * `node` - The node to match
    /// * `match_value` - The feature values to match
    /// # Panics
    /// Panics if `node` is [NodeKind::Place]
    pub fn node_match(&self, node: NodeKind, match_value: Option<u8>) -> bool {
        debug_assert_ne!(node, NodeKind::Place);
        let Some(n) = self.get_node(node) else {
            return match_value.is_none()
        };
        let Some(m) = match_value else {return false};

        n == m
    }

    fn apply_diacritic_payload(&mut self, dm:&DiaMods) {
        for (i, m) in dm.nodes.iter().enumerate() {
            if let Some(kind) = m {
                let node = NodeKind::from_usize(i);
                debug_assert_ne!(node, NodeKind::Place);
                match kind {
                    ModKind::Binary(b) => match b {
                        BinMod::Negative => self.set_node(node, None),
                        BinMod::Positive if self.is_node_none(node) => self.set_node(node, Some(0)),
                        BinMod::Positive => { /* Don't override it */ },
                    },
                    ModKind::Alpha(_) => unreachable!(),
                }
            }
        }
        for (i, m) in dm.feats.iter().enumerate() {
            if let Some(kind) = m {
                let (n,f) = FeatKind::from_usize(i).as_node_mask();
                match kind {
                    ModKind::Binary(b) => match b {
                        BinMod::Negative => self.set_feat(n, f, false),
                        BinMod::Positive => self.set_feat(n, f, true),
                    },
                    ModKind::Alpha(_) => unreachable!(),
                }
            }
        }
    }

    pub(crate) fn check_and_apply_diacritic(&mut self, d: &Diacritic) -> Result<(), (usize, bool)> {
        self.match_modifiers(&d.prereqs)?;
        self.apply_diacritic_payload(&d.payload);
        Ok(())
    }

    pub(crate) fn apply_seg_mods(&mut self, alphas: &RefCell<HashMap<char, Alpha>> , nodes: [Option<ModKind>; NodeKind::count()], feats: [Option<ModKind>; FeatKind::count()], err_pos: Position, is_matching_ipa: bool) -> Result<(), RuleRuntimeError>{
        for (i, m) in nodes.iter().enumerate() { 
            let node = NodeKind::from_usize(i);
            if let Some(kind) = m {
                match kind {
                    ModKind::Binary(bm) => match bm {
                        BinMod::Negative => match node {
                            NodeKind::Root      => return Err(RuleRuntimeError::NodeCannotBeNone("Root".to_owned(), err_pos)),
                            NodeKind::Manner    => return Err(RuleRuntimeError::NodeCannotBeNone("Manner".to_owned(), err_pos)),
                            NodeKind::Laryngeal => return Err(RuleRuntimeError::NodeCannotBeNone("Largyneal".to_owned(), err_pos)),
                            NodeKind::Place     => *self.place = None, // e.g. Debuccalization
                            _ => self.set_node(node, None),
                            
                        },
                        BinMod::Positive => match node {
                            NodeKind::Root      => return Err(RuleRuntimeError::NodeCannotBeSome("Root".to_owned(), err_pos)),
                            NodeKind::Manner    => return Err(RuleRuntimeError::NodeCannotBeSome("Manner".to_owned(), err_pos)),
                            NodeKind::Laryngeal => return Err(RuleRuntimeError::NodeCannotBeSome("Largyneal".to_owned(), err_pos)),
                            NodeKind::Place     => return Err(RuleRuntimeError::NodeCannotBeSome("Place".to_owned(), err_pos)),
                            // preserve node if already positive
                            _ if self.is_node_none(node) => self.set_node(node, Some(0)),
                            _ => { /* Don't override it */ }
                        },
                    },
                    ModKind::Alpha(am) => match am {
                        AlphaMod::Alpha(ch) => {
                        let mut alpha_assigned = false; // needed because of borrow checker weirdness. See: https://github.com/rust-lang/rust/issues/113792
                            if let Some(alpha) = alphas.borrow().get(ch) {
                                if let Some((n, m)) = alpha.as_node() {
                                    if n == node {
                                        self.set_node(n, m);
                                    } else {
                                        return Err(RuleRuntimeError::AlphaIsNotSameNode(err_pos))
                                    }
                                } else if let Some(place) = alpha.as_place() {
                                    match node {
                                        NodeKind::Root      => return Err(RuleRuntimeError::NodeCannotBeSet("Root".to_owned(), err_pos)),
                                        NodeKind::Manner    => return Err(RuleRuntimeError::NodeCannotBeSet("Manner".to_owned(), err_pos)),
                                        NodeKind::Laryngeal => return Err(RuleRuntimeError::NodeCannotBeSet("Laryngeal".to_owned(), err_pos)),
                                        NodeKind::Place => {
                                            self.set_node(NodeKind::Labial    , place.lab);
                                            self.set_node(NodeKind::Coronal   , place.cor);
                                            self.set_node(NodeKind::Dorsal    , place.dor);
                                            self.set_node(NodeKind::Pharyngeal, place.phr);
                                        },
                                        // Partial Place application
                                        NodeKind::Labial     => self.set_node(NodeKind::Labial    , place.lab),
                                        NodeKind::Coronal    => self.set_node(NodeKind::Coronal   , place.cor),
                                        NodeKind::Dorsal     => self.set_node(NodeKind::Dorsal    , place.dor),
                                        NodeKind::Pharyngeal => self.set_node(NodeKind::Pharyngeal, place.phr),
                                    }
                                    
                                } else {
                                    return Err(RuleRuntimeError::AlphaIsNotNode(err_pos))
                                }
                                alpha_assigned = true;
                            }
                            if !alpha_assigned {
                                if is_matching_ipa {
                                    if node == NodeKind::Place {
                                        let pm = PlaceMod { 
                                            lab: self.get_node(NodeKind::Labial), 
                                            cor: self.get_node(NodeKind::Coronal), 
                                            dor: self.get_node(NodeKind::Dorsal), 
                                            phr: self.get_node(NodeKind::Pharyngeal) 
                                        };
                                        alphas.borrow_mut().insert(*ch, Alpha::Place(pm));
                                    } else {
                                        alphas.borrow_mut().insert(*ch, Alpha::Node(node, self.get_node(node)));
                                    }
                                } else {
                                    return Err(RuleRuntimeError::AlphaUnknown(err_pos))
                                }
                            }
                        },
                        AlphaMod::InvAlpha(_) => return Err(RuleRuntimeError::AlphaNodeAssignInv(err_pos))
                    },
                }
            }
        }
        for (i, m) in feats.iter().enumerate() {
            if let Some(kind) = m { 
                let (n, f) = FeatKind::from_usize(i).as_node_mask();
                match kind {
                    ModKind::Binary(b) => match b {
                        BinMod::Negative => self.set_feat(n, f, false),
                        BinMod::Positive => self.set_feat(n, f, true),
                    },
                    ModKind::Alpha(am) => match am {
                        AlphaMod::Alpha(ch) => {
                            let mut alpha_assigned = false;
                            if let Some(alpha) = alphas.borrow().get(ch) {
                                let tp = alpha.as_binary();
                                self.set_feat(n, f, tp);
                                alpha_assigned = true;
                            } 
                            if !alpha_assigned {
                                if is_matching_ipa {
                                    let x = if let Some(feat) = self.get_feat(n, f) {
                                        feat != 0
                                    } else {false};
                                    alphas.borrow_mut().insert(*ch, Alpha::Feature(x));
                                } else {
                                    return Err(RuleRuntimeError::AlphaUnknown(err_pos))
                                }
                            }
                        },
                        AlphaMod::InvAlpha(ch) => {
                            let mut alpha_assigned = false;
                            if let Some(alpha) = alphas.borrow().get(ch) {
                                let tp = alpha.as_binary();
                                self.set_feat(n, f, !tp);
                                alpha_assigned = true;
                            }
                            if !alpha_assigned {
                                if is_matching_ipa {
                                    let x = if let Some(feat) = self.get_feat(n, f) {
                                        feat != 0
                                    } else {false};
                                    alphas.borrow_mut().insert(*ch, Alpha::Feature(!x));
                                } else {
                                    return Err(RuleRuntimeError::AlphaUnknown(err_pos))
                                }
                            }
                        },
                    },
                }
            }
        }
        Ok(())
    } 
}

#[cfg(test)]
mod seg_tests {
    use super::super::*;

    #[test]
    fn test_debug_print() {
        let seg = Word::new("n".to_owned(), &[]).unwrap().syllables[0].segments[0];
        let n = format!("{:?}", seg);
        println!("{n}");
        assert_eq!(n, "++-|---+----|+--|00|+-|000000|00");
    }

    #[test]
    fn test_edge_cases() {
        let word = Word::new("i̯.y̯.ɯ̯.u̯.æ̯ʷ.ɐ̯ʷ.i̯ᵊ.y̯ᵊ.ɯ̯ᵊ.u̯ᵊ.æ̯ʷᵊ.ɐ̯ʷᵊ.nˡ".to_owned(), &[]).unwrap();

        assert_eq!(&word.render(&[]), "i̯.y̯.ɯ̯.u̯.æ̯ʷ.ɐ̯ʷ.i̯ᵊ.y̯ᵊ.ɯ̯ᵊ.u̯ᵊ.æ̯ʷᵊ.ɐ̯ʷᵊ.nˡ")
    }
    
    #[test]
    fn test_diacritics() {
        let word = Word::new("iʵ.t̪.".to_owned(), &[]).unwrap();
            
        assert_eq!(&word.render(&[]), "iʵ.t̪")
    }
}