use std::{
    fmt::Display, 
    collections::VecDeque
};

#[derive(Clone)]
struct Node {
    key: Option<char>,
    val: Option<String>,
    is_terminal: bool,
    children: Vec<Node>

}

impl Node {
    fn new() -> Self {
        Self {key: None, val: None, is_terminal: false, children: Vec::new() }
    }

    fn with_key(c: char) -> Self {
        Node { 
            key: Some(c), 
            val: None,
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

    #[allow(dead_code)]
    /// Returns the number of words in the tree
    pub(crate) fn length(&self) -> usize { self.elements + 1 }
    
    /// Inserts a word into the tree without duplication
    pub(crate) fn insert(&mut self, s: &str) {
        let mut cur = &mut self.root;
        for c in s.chars() {
            match cur.children.binary_search_by(|f| f.key.cmp(&Some(c))) {
                Ok (i) => cur = &mut cur.children[i],
                Err(i) => {
                    cur.children.insert(i, Node::with_key(c));
                    cur = &mut cur.children[i];
                }
            }
        }
        if !cur.is_terminal {
            cur.is_terminal = true;
            self.elements += 1;
        }
        cur.val.replace(s.to_string());
    }

    /// returns true if input matches a leaf in the tree
    #[allow(dead_code)]
    pub(crate) fn contains(&self, s: &str) -> bool { 
        let mut curr_node = &self.root;

        for c in s.chars() {
            match curr_node.children.binary_search_by(|f| f.key.cmp(&Some(c))) {
                Ok (i) => curr_node = &curr_node.children[i],
                Err(_) => return false,
            }
        }

        curr_node.is_terminal
    }

    /// Returns true if input is a 'prefix' of a leaf in the tree.
    /// A prefix can also end in a leaf.
    pub(crate) fn contains_prefix(&self, s: &str) -> bool { 
        let mut curr_node = &self.root;

        for c in s.chars() {
            match curr_node.children.binary_search_by(|f| f.key.cmp(&Some(c))) {
                Ok (i) => curr_node = &curr_node.children[i],
                Err(_) => return false,
            }
        }

        true
    }

    /// Traverses the tree using a given string input and then returns vector of all leaf nodes past the end of input
    #[allow(dead_code)]
    pub(crate) fn find_all(&self, s: &str) -> Vec<String> { 
        let mut cur = &self.root;

        for c in s.chars() {
            match cur.children.binary_search_by(|f| f.key.cmp(&Some(c))) {
                Ok (i) => cur = &cur.children[i],
                Err(_) => return Vec::new(),
            }
        }

        let mut results = Vec::new();
        let mut q = Vec::new();
        q.push(cur);
        while let Some(c) = q.pop() {
            for child in c.children.iter() {
                q.push(child);
            }

            if c.is_terminal {
                let value = c.val.as_ref().unwrap();
                results.push(value.clone());
            }
        }

        results
     }

}

impl Display for Trie {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut q: VecDeque<&Node> = VecDeque::new();
        let root  = &self.root;
        q.push_back(root);

        while !q.is_empty() {
            for _ in 0..q.len() {
                if let Some(node) = q.pop_front() {
                    for c in node.children.iter() {
                        write!(f, "{} ", c.key.unwrap())?;
                        if !c.children.is_empty() {
                            q.push_back(c);
                        }
                    }
                }
            }

            if !q.is_empty() {
                writeln!(f)?;
            }
        }

        Ok(())
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

        assert_eq!(trie.find_all("te"), vec!["test", "tea"]);
        assert_eq!(trie.find_all("a"), vec!["a", "apples", "an"]);
        trie.insert("test");
        trie.insert("test");
        assert_eq!(trie.length(), 7);
    }
}



