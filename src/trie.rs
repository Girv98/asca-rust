use std::{
    collections::VecDeque, fmt::Display
};

#[derive(Debug, Clone)]
struct Node {
    key: char,
    is_terminal: bool,
    children: Vec<Node>
}

impl Node {
    fn new() -> Self {
        Self { key: '\0', is_terminal: false, children: Vec::new() }
    }

    fn with_key(ch: char) -> Self {
        Node { 
            key: ch, 
            is_terminal: false, 
            children: Vec::new()
        }
    }
}

#[derive(Clone)]
pub(crate) struct Trie {
    root: Node,
    elements: usize
}

impl Trie {
    pub(crate) fn new() -> Self {
        Self { root: Node::new(), elements: 0 }
    }

    #[allow(unused)]
    /// Returns the number of words in the tree
    pub(crate) fn length(&self) -> usize { self.elements + 1 }

    #[allow(unused)]
    pub(crate) fn max_width(&self) -> usize {
        let mut queue = VecDeque::new();
        queue.push_back(&self.root);
        let mut max_width = 0;

        while !queue.is_empty() {
            max_width = max_width.max(queue.len());
            
            for _ in 0..queue.len() {
                if let Some(current) = queue.pop_front() {
                    for child in &current.children {
                        queue.push_back(child);
                    }
                }
            }
        }
        
        max_width
    }

    #[allow(unused)]
    pub(crate) fn common_prefix_len(a: &str, b: &str) -> usize {
        let mut len = 0;
        for (a_ch, b_ch) in a.chars().zip(b.chars()) {
            if a_ch == b_ch { len += 1 } else { break }
        }

        len
    }
    
    /// Inserts a word into the tree without duplication
    pub(crate) fn insert(&mut self, s: &str) {
        let mut current = &mut self.root;

        for ch in s.chars() {
            match current.children.binary_search_by(|f| f.key.cmp(&ch)) {
                Ok (i) => current = &mut current.children[i],
                Err(i) => {
                    current.children.insert(i, Node::with_key(ch));
                    current = &mut current.children[i];
                }
            }
        }

        if !current.is_terminal {
            current.is_terminal = true;
            self.elements += 1;
        }
    }

    /// returns true if input matches a leaf in the tree
    #[allow(dead_code)]
    pub(crate) fn contains(&self, s: &str) -> bool { 
        let mut current = &self.root;

        for ch in s.chars() {
            match current.children.binary_search_by(|f| f.key.cmp(&ch)) {
                Ok (i) => current = &current.children[i],
                Err(_) => return false,
            }
        }

        current.is_terminal
    }

    /// Returns true if input is a 'prefix' of a leaf in the tree.
    /// A prefix can also end in a leaf.
    pub(crate) fn contains_prefix(&self, s: &str) -> bool { 
        let mut current = &self.root;

        for c in s.chars() {
            match current.children.binary_search_by(|f| f.key.cmp(&c)) {
                Ok (i) => current = &current.children[i],
                Err(_) => return false,
            }
        }

        true
    }

    // /// Traverses the tree using a given string input and then returns vector of all leaf nodes past the end of input
    // #[allow(dead_code)]
    // pub(crate) fn find_all(&self, s: &str) -> Vec<&str> { 
    //     let mut cur = &self.root;

    //     for c in s.chars() {
    //         match cur.children.binary_search_by(|f| f.key.cmp(&Some(c))) {
    //             Ok (i) => cur = &cur.children[i],
    //             Err(_) => return Vec::new(),
    //         }
    //     }

    //     let mut results = Vec::new();
    //     let mut q = Vec::new();
    //     q.push(cur);
    //     while let Some(c) = q.pop() {
    //         for child in c.children.iter() {
    //             q.push(child);
    //         }

    //         if c.is_terminal {
    //             let value = c.val.as_ref().unwrap();
    //             results.push(*value);
    //         }
    //     }

    //     results
    // }

}

// impl Display for Trie {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let mut queue: VecDeque<&Node> = VecDeque::new();
//         queue.push_back(&self.root);

//         while !queue.is_empty() {
//             for _ in 0..queue.len() {
//                 if let Some(node) = queue.pop_front() {
//                     for child in &node.children {
//                         write!(f, "{} ", child.key)?;
//                         queue.push_back(child);
//                     }
//                 }
//             }

//             writeln!(f)?;
//             writeln!(f)?;
//         }

//         Ok(())
//     }
// }


impl Display for Trie {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.root)
    }
}

impl Node {
    fn fmt_tree(&self, f: &mut std::fmt::Formatter<'_>, prefix: &str, is_end: bool, is_root: bool) -> std::fmt::Result {
        if is_root {
            writeln!(f, "{}", self.key)?;
        } else {
            let connector = if is_end { "└── " } else { "├── " };
            writeln!(f, "{}{}{}", prefix, connector, self.key)?;
        }

        let child_prefix = if is_root {
            String::new()
        } else if is_end {
            format!("{}    ", prefix)
        } else {
            format!("{}│   ", prefix)
        };

        let len = self.children.len();
        for (i, child) in self.children.iter().enumerate() {
            let last_child = i == len - 1;
            child.fmt_tree(f, &child_prefix, last_child, false)?;
        }

        Ok(())
    }
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_tree(f, "", true, true)
    }
}



#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_trie() {
        let mut trie = Trie::new();
        trie.insert("a");
        trie.insert("to");
        trie.insert("tea");
        trie.insert("apples");
        trie.insert("an");
        trie.insert("test");
        trie.insert("tea");

        assert!(trie.contains("test"));
        assert!(trie.contains("to"));
        assert!(trie.contains("tea"));
        assert!(!trie.contains("air"));

        assert!(trie.contains_prefix("te"));
        assert!(trie.contains_prefix("a"));
        assert!(trie.contains_prefix("ap"));
        assert!(trie.contains_prefix("app"));
        assert!(!trie.contains_prefix("b"));

        // assert_eq!(trie.find_all("te"), vec!["test", "tea"]);
        // assert_eq!(trie.find_all("a"), vec!["a", "apples", "an"]);
        trie.insert("test");
        trie.insert("test");
        assert_eq!(trie.length(), 7);
    }
}
