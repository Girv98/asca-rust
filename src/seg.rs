use std::{cell::RefCell, collections::HashMap, fmt};
use serde::Deserialize;

use crate :: {
    error :: RuleRuntimeError, 
    lexer :: { FType, NodeType, Position }, 
    parser:: { AlphaMod, BinMod, ModKind, Modifiers, SupraSegs }, 
    rule  :: Alpha, 
    CARDINALS_MAP, CARDINALS_VEC, DIACRITS 
};

#[derive(Debug, Clone)]
pub(crate) struct Diacritic {
    #[allow(unused)]
    pub(crate) name: String,
    pub(crate) diacrit: char,
    pub(crate) prereqs: DiaMods,
    pub(crate) payload: DiaMods,
}


#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) struct DiaMods {
    pub(crate) nodes: [Option<ModKind>; NodeType::Pharyngeal as usize + 1],
    pub(crate) feats: [Option<ModKind>; FType::RetractedTongueRoot as usize + 1],
}

impl DiaMods {
    pub(crate) fn new() -> Self {
        Self { 
            nodes: [();NodeType::Pharyngeal as usize + 1].map(|_| None), 
            feats: [();FType::RetractedTongueRoot as usize + 1].map(|_| None), 
        }
    }
}

impl fmt::Debug for DiaMods {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {

        let mut nodes = String::new();
        for val in self.nodes.iter() {
            match val {
                Some(v) => match v {
                    ModKind::Binary(b) => match b {
                        BinMod::Negative => nodes.push('-'),
                        BinMod::Positive => nodes.push('+'),
                    },
                    ModKind::Alpha(am) => match am {
                        crate::parser::AlphaMod::Alpha(a) => nodes.push(*a),
                        crate::parser::AlphaMod::InvAlpha(ia) => nodes.push_str(&ia.to_uppercase().to_string()),
                    },
                },
                None => nodes.push('0'),
            }
        }

        let mut feats = String::new();
        for val in self.feats.iter() {
            match val {
                Some(v) => match v {
                    ModKind::Binary(b) => match b {
                        BinMod::Negative => feats.push('-'),
                        BinMod::Positive => feats.push('+'),
                    },
                    ModKind::Alpha(am) => match am {
                        crate::parser::AlphaMod::Alpha(a) => feats.push(*a),
                        crate::parser::AlphaMod::InvAlpha(ia) => feats.push_str(&ia.to_uppercase().to_string()),
                    },
                },
                None => feats.push('0'),
            }
        }

        f.debug_struct("DiaMods").field("nodes", &nodes).field("feats", &feats).finish()
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) enum NodeKind {
    Root,
    Manner,
    Laryngeal,
    Place,
    Labial,
    Coronal,
    Dorsal,
    Pharyngeal
}

impl NodeKind {
    pub(crate) const fn count() -> usize { 8 }

    pub(crate) fn from_usize(value: usize) -> Self {
        debug_assert!(NodeKind::count() == 8);
        use NodeKind::*;
        match value {
            0 => {debug_assert_eq!(value, Root as usize); Root}
            1 => {debug_assert_eq!(value, Manner as usize); Manner}
            2 => {debug_assert_eq!(value, Laryngeal as usize); Laryngeal}
            3 => {debug_assert_eq!(value, Place as usize); Place}
            4 => {debug_assert_eq!(value, Labial as usize); Labial}
            5 => {debug_assert_eq!(value, Coronal as usize); Coronal}
            6 => {debug_assert_eq!(value, Dorsal as usize); Dorsal}
            7 => {debug_assert_eq!(value, Pharyngeal as usize); Pharyngeal},
            _ => unreachable!("\nOut of Range converting `{value}` to `NodeType`(max: 7) \nThis is a bug!\n")
        }
    }
}

// pub(crate) fn test_node_variants() {
//     fn asdf(cache: HashMap<u8, u64>) -> Vec<String> {
//         let mut v: Vec<_> = cache.into_iter().collect();
//         v.sort_by(|x, y| x.0.cmp(&y.0));
//         let width = 8 - v.last().unwrap().0.leading_zeros() as usize;
//         v.iter().map(|(x, v)| format!("{:0width$b} : {}", x, v)).collect()
//     }
//     fn qwer(cache: HashMap<Option<u8>, u64>) -> Vec<String> {
//         let mut v: Vec<_> = cache.into_iter().collect();
//         v.sort_by(|x, y| x.0.cmp(&y.0));
//         let width = 8 - v.last().unwrap().0.unwrap().leading_zeros() as usize;
//         v.iter().map(|(x, v)| {
//             if let Some(q) = x {
//                 format!("{:0width$b} : {}", q, v)
//             } else {
//                 format!("- : {v}")
//             }
//         }).collect()
//     }
    
//     let mut rut_cache = std::collections::HashMap::<u8, u64>::new();
//     let mut man_cache = std::collections::HashMap::<u8, u64>::new();
//     let mut lar_cache = std::collections::HashMap::<u8, u64>::new();
//     let mut lab_cache = std::collections::HashMap::<Option<u8>, u64>::new();
//     let mut cor_cache = std::collections::HashMap::<Option<u8>, u64>::new();
//     let mut dor_cache = std::collections::HashMap::<Option<u8>, u64>::new();
//     let mut phr_cache = std::collections::HashMap::<Option<u8>, u64>::new();

//     let mut count = 0;
    
//     CARDINALS_VEC.iter().for_each(|s| {
//         let seg = CARDINALS_MAP.get(s).unwrap();
        
//         *rut_cache.entry(seg.root).or_default() += 1;
//         *man_cache.entry(seg.manner).or_default() += 1;
//         *lar_cache.entry(seg.laryngeal).or_default() += 1;
//         *lab_cache.entry(seg.labial).or_default() += 1;
//         *cor_cache.entry(seg.coronal).or_default() += 1;
//         *dor_cache.entry(seg.dorsal).or_default() += 1;
//         *phr_cache.entry(seg.pharyngeal).or_default() += 1;
//         count += 1;
//     });
//     println!("Of {count} cardinals:");
//     println!("rut: - {:#?}", asdf(rut_cache));
//     println!("man: - {:#?}", asdf(man_cache));
//     println!("lar: - {:#?}", asdf(lar_cache));
//     println!("lab: - {:#?}", qwer(lab_cache));
//     println!("cor: - {:#?}", qwer(cor_cache));
//     println!("dor: - {:#?}", qwer(dor_cache));
//     println!("phr: - {:#?}", qwer(phr_cache));
// }

// pub(crate) fn test_vowel_variants() {
//     let mut errs = 0;
//     let mut count = 0;
//     let root = 0b011;
//     let manner = 0b11000000;
//     let laryngeal = 0b100;
//     let labial = [0b11, 0b10, 0b1, 0b0];
//     let coronal = None;
//     let pharyngeal = None;

//     for l in labial {
//         for d in 0..=0b111111 {
//             let seg = Segment {root, manner, laryngeal, labial : Some(l), coronal, dorsal: Some(d), pharyngeal };
//             count += 1;
//             if let Some(g) = seg.get_as_grapheme() {
//                 if g == "�".to_string() {
//                     errs += 1;
//                 }
//                 println!("{g}")
//             } else {
//                 errs += 1;
//             }
//         }
//     }
//     println!("Total {errs} out of {count}");
// }

// More than 246172 legal variants
// pub(crate) fn test_variants() {
//     let mut errs = 0;
//     let mut count = 0;
//
//     for root in 1..=0b111 {
//         for manner in 0..=0b11111111 {
//             for laryngeal in 0..=0b111 {
//                 for l in 0..=0b11 {
//                     for c in 0..=0b11 {
//                         for d in 0..=0b111111 {
//                             for p in 0..=0b11 {
//                                 let seg = Segment { root, manner, laryngeal, labial: Some(l), coronal: Some(c), dorsal: Some(d), pharyngeal: Some(p) };
//                                 count +=1;
//                                 if let Some(g) = seg.get_as_grapheme() {
//                                     if g == "�".to_string() {
//                                         errs += 1;
//                                     }
//                                 } else {
//                                     errs += 1;
//                                 }
//                             }
//                         }
//                     }
//                 }
//             }
//         }
//         println!("{root}/7: {errs} out of {count} ");
//     } 
//     println!("Total {errs} out of {count}");
// }

#[derive(Default, Clone, Copy, PartialEq, Eq, Deserialize)]
pub(crate) struct Segment {
    pub(crate) root      : u8,
    pub(crate) manner    : u8,
    pub(crate) laryngeal : u8,
    pub(crate) labial    : Option<u8>,
    pub(crate) coronal   : Option<u8>,
    pub(crate) dorsal    : Option<u8>,
    pub(crate) pharyngeal: Option<u8>,
}
 
impl fmt::Debug for Segment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {

        let grapheme = self.get_as_grapheme();
        write!(f, "{}", grapheme)
    }
}

impl Segment {

    // Get 
    pub(crate) fn get_nearest_grapheme(&self) -> String {
        // test against all cardinal values for a match
        for c_grapheme in CARDINALS_VEC.iter() {
            let x = CARDINALS_MAP.get(c_grapheme).unwrap();
            if *x == *self { return c_grapheme.to_string() }
        }

        let mut candidates = Vec::new();
        for c_grapheme in CARDINALS_VEC.iter() {
            let seg: Segment = *CARDINALS_MAP.get(c_grapheme).unwrap();
            let diff_count = self.diff_count(&seg);
            if diff_count < 8 {
                candidates.push((seg, c_grapheme, diff_count))
            }
        }
        
        // If differences are two numerous, fall back to diacritics
        // Won't happen when using diff_count < 8, but guarantees
        // that indexing into candidates doesn't error
        if candidates.is_empty() {
            return self.get_as_grapheme()
        }
        // Get best candidate
        // TODO(girv): suffers from same bug as get_as_grapheme where
        // equal distance candidates are randomly ordered
        candidates.sort_by(|(.., a), (.., b) | a.cmp(b));
        candidates[0].1.to_string()
    }


    // TODO(girv): This wouldn't get me any leetcode cred
    pub(crate) fn get_as_grapheme(&self) -> String {
        // test against all cardinal values for a match
        for c_grapheme in CARDINALS_VEC.iter() {
            let x = CARDINALS_MAP.get(c_grapheme).unwrap();
            if *x == *self { return c_grapheme.to_string() }
        }

        // if no match is found, 
        // loop through again, but this time test cardinal + diacritics

        // Sort by difference (filter out diff >= 8 to cut on size)
        // iterate and match starting from smallest difference

        // Because CARDINALS_MAP's order is random, this can lead to random edge cases
        // TODO(girv): test, test, test

        let mut candidates = Vec::new();

        for c_grapheme in CARDINALS_VEC.iter() {
            let seg: Segment = *CARDINALS_MAP.get(c_grapheme).unwrap();
            let diff_count = self.diff_count(&seg);
            if diff_count < 8 {
                candidates.push((seg, c_grapheme, diff_count))
            }
        }

        // let mut candidates = CARDINALS_VEC.iter().filter_map(|grapheme| {
        //     let seg = *CARDINALS_MAP.get(grapheme).unwrap();
        //     let diff_count = self.diff_count(&seg);
        //     if diff_count < 8 {
        //         Some((seg, grapheme, diff_count))
        //     } else {
        //         None
        //     }
        // }).collect::<Vec<_>>();

        candidates.sort_by(|(.., a), (.., b) | a.cmp(b));

        // for (a,_, b) in &candidates {
        //     eprintln!("{} {}", a.get_as_grapheme().unwrap(), b);
        // }
        // eprintln!("{}", candidates.len());
        
        for (cand_seg , cand_graph, _) in candidates {
            let mut buf_seg = cand_seg;
            let mut buf_str = cand_graph.clone();
            for d in DIACRITS.iter() {
                if self.match_modifiers(&d.prereqs).is_ok() && self.match_modifiers(&d.payload).is_ok() {
                    let before = buf_seg;
                    buf_seg.apply_diacritic_payload(&d.payload);
                    if buf_seg == before {
                        buf_seg = before;
                        continue;
                    }
                    if buf_seg == cand_seg {
                        continue;
                    } else {
                        buf_str.push(d.diacrit);
                    }
                }
                if buf_seg == *self { 
                    return buf_str;
                }
            }
        }

        "�".to_string()
    }

    pub(crate) fn match_modifiers(&self, mods: &DiaMods) -> Result<(), (usize, bool)> {
        for (i, m) in mods.feats.iter().enumerate() {
            if !self.match_feat_mod(m, i) {
                return Err((i, false))
            }
        }
        for (i, m) in mods.nodes.iter().enumerate() {
            if !self.match_node_mod(m, i) {
                return Err((i, true))
            }
        }
        Ok(())
    }

    pub(crate) fn match_node_mod(&self, md: &Option<ModKind>, node_index: usize) -> bool {
        let Some(kind) = md else { return true };
        self.match_node_mod_kind(kind, NodeKind::from_usize(node_index))
    }

    pub(crate) fn match_feat_mod(&self, md: &Option<ModKind>, feat_index: usize) -> bool {
        let Some(kind) = md else { return true };
        let (node, mask) = FType::from_usize(feat_index).to_node_mask();
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
        
        let to_bin_mod = |b: bool | { if b {BinMod::Positive} else {BinMod::Negative}};
        
        let place = self.labial.is_some() || self.coronal.is_some() || self.dorsal.is_some() || self.pharyngeal.is_some();

        let nodes = [
            Some(ModKind::Binary(BinMod::Positive)),
            Some(ModKind::Binary(BinMod::Positive)),
            Some(ModKind::Binary(BinMod::Positive)),
            Some(ModKind::Binary(to_bin_mod(place))),
            Some(ModKind::Binary(to_bin_mod(self.labial.is_some()))),
            Some(ModKind::Binary(to_bin_mod(self.coronal.is_some()))),
            Some(ModKind::Binary(to_bin_mod(self.dorsal.is_some()))),
            Some(ModKind::Binary(to_bin_mod(self.pharyngeal.is_some()))),
            ];

        let mut feats = [();FType::count()].map(|_| None);     
        #[allow(clippy::needless_range_loop)] 
        for i in 0..FType::count() {
            let (n, f) = feature_to_node_mask(FType::from_usize(i));   
            let Some(x) = self.get_feat(n, f) else { continue };
            
            feats[i] = Some(ModKind::Binary(to_bin_mod(x != 0)))
        }
        
        let suprs = SupraSegs::new();

        Modifiers { nodes, feats, suprs }
    }

    fn diff_count(&self, other: &Segment) -> usize {
        fn dif_option(f: Option<u8>, s: Option<u8>) -> Option<u8> {
            let Some(a) = f else { return s };
            let Some(b) = s else { return f };
            Some(a ^ b)
        }

        let rut = self.root ^ other.root;
        let man = self.manner ^ other.manner;
        let lar = self.laryngeal ^ other.laryngeal;
        let lab = dif_option(self.labial, other.labial).unwrap_or(0);
        let cor = dif_option(self.coronal, other.coronal).unwrap_or(0);
        let dor = dif_option(self.dorsal, other.dorsal).unwrap_or(0);
        let phr = dif_option(self.pharyngeal, other.pharyngeal).unwrap_or(0);

        ( rut.count_ones()
        + man.count_ones()
        + lar.count_ones()
        + lab.count_ones()
        + cor.count_ones()
        + dor.count_ones()
        + phr.count_ones() ) as usize
    }

    #[inline]
    pub(crate) fn get_node(&self, node: NodeKind) -> Option<u8> {
        match node {
            NodeKind::Root       => Some(self.root),
            NodeKind::Manner     => Some(self.manner),
            NodeKind::Laryngeal  => Some(self.laryngeal),
            NodeKind::Labial     => self.labial,
            NodeKind::Coronal    => self.coronal,
            NodeKind::Dorsal     => self.dorsal,
            NodeKind::Pharyngeal => self.pharyngeal,
            NodeKind::Place      => unreachable!(),
        }
    }

    #[inline]
    pub(crate) fn set_node(&mut self, node: NodeKind, val: Option<u8>) {
        match node {
            NodeKind::Root       => self.root = val.expect("RootNode cannot be null"),
            NodeKind::Manner     => self.manner = val.expect("MannerNode cannot be null"),
            NodeKind::Laryngeal  => self.laryngeal = val.expect("LaryngealNode cannot be null"),
            NodeKind::Labial     => self.labial = val,
            NodeKind::Coronal    => self.coronal = val,
            NodeKind::Dorsal     => self.dorsal = val,
            NodeKind::Pharyngeal => self.pharyngeal = val,
            NodeKind::Place      => unreachable!(),
        }
    }

    #[inline]
    pub(crate) fn get_feat(&self, node: NodeKind, feat: u8) -> Option<u8> {
        Some(self.get_node(node)? & feat)
    }

    pub(crate) fn set_feat(&mut self, node: NodeKind, feat: u8, to_positive: bool) {
        debug_assert_ne!(node, NodeKind::Place);
        if to_positive {
            let n = self.get_node(node).unwrap_or(0u8);
            self.set_node(node, Some(n | feat)) 
        } else if let Some(n) = self.get_node(node) {
            self.set_node(node, Some(n & !(feat)))
        }
    }

    pub(crate) fn feat_match(&self, node: NodeKind, mask: u8, positive: bool) -> bool {
        let Some(n) = self.get_node(node) else {
            return false
        };
        if positive {
            n & mask == mask
        } else {
            n & mask == 0
        }
    }

    #[inline]
    pub(crate) fn is_node_some(&self, node: NodeKind) -> bool {
        self.get_node(node).is_some()
    }

    #[inline]
    pub(crate) fn is_node_none(&self, node: NodeKind) -> bool {
        self.get_node(node).is_none()
    }

    pub(crate) fn node_match(&self, node: NodeKind, match_value: Option<u8>) -> bool {
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
                        BinMod::Positive => self.set_node(node, Some(0))
                    },
                    ModKind::Alpha(_) => unreachable!(),
                }
            }
        }
        for (i, m) in dm.feats.iter().enumerate() {
            if let Some(kind) = m {
                let (n,f) = feature_to_node_mask(FType::from_usize(i));
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

    pub(crate) fn apply_seg_mods(&mut self, alphas: &RefCell<HashMap<char, Alpha>> , nodes: [Option<ModKind>; NodeType::count()], feats: [Option<ModKind>; FType::count()], err_pos: Position, is_matching_ipa: bool) -> Result<(), RuleRuntimeError>{
        for (i, m) in nodes.iter().enumerate() { 
            let node = NodeKind::from_usize(i);
            if let Some(kind) = m {
                match kind {
                    ModKind::Binary(bm) => match bm {
                        BinMod::Negative => match node {
                            NodeKind::Root      => return Err(RuleRuntimeError::NodeCannotBeNone("Root".to_owned(), err_pos)),
                            NodeKind::Manner    => return Err(RuleRuntimeError::NodeCannotBeNone("Manner".to_owned(), err_pos)),
                            NodeKind::Laryngeal => return Err(RuleRuntimeError::NodeCannotBeNone("Largyneal".to_owned(), err_pos)),
                            NodeKind::Place => {
                                // e.g. Debuccalization
                                self.set_node(NodeKind::Labial    , None);
                                self.set_node(NodeKind::Coronal   , None);
                                self.set_node(NodeKind::Dorsal    , None);
                                self.set_node(NodeKind::Pharyngeal, None);
                            },
                            _ => self.set_node(node, None),
                            
                        },
                        BinMod::Positive => match node {
                            NodeKind::Root      => return Err(RuleRuntimeError::NodeCannotBeSome("Root".to_owned(), err_pos)),
                            NodeKind::Manner    => return Err(RuleRuntimeError::NodeCannotBeSome("Manner".to_owned(), err_pos)),
                            NodeKind::Laryngeal => return Err(RuleRuntimeError::NodeCannotBeSome("Largyneal".to_owned(), err_pos)),
                            NodeKind::Place     => return Err(RuleRuntimeError::NodeCannotBeSome("Place".to_owned(), err_pos)),
                            _ => {
                                // preserve node if already positive
                                if self.get_node(node).is_none() {
                                    self.set_node(node, Some(0))
                                }
                            },
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
                                        let pm = crate::PlaceMod { 
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
                let (n, f) = feature_to_node_mask(FType::from_usize(i));
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