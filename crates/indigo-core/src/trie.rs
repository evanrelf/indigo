use std::sync::Arc;

#[derive(Clone)]
pub struct Trie<K, V> {
    root: Arc<Node<K, V>>,
}

impl<K, V> Trie<K, V> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            root: Arc::new(Node::default()),
        }
    }

    pub fn insert(&mut self, key: &[K], value: V) -> Option<V>
    where
        K: Clone + Ord,
        V: Clone,
    {
        let root = Arc::make_mut(&mut self.root);
        root.insert(key, value)
    }

    pub fn remove(&mut self, key: &[K]) -> Option<V>
    where
        K: Clone + Ord,
        V: Clone,
    {
        let root = Arc::make_mut(&mut self.root);
        root.remove(key)
    }

    #[must_use]
    pub fn get(&self, key: &[K]) -> TrieResult<&V>
    where
        K: Ord,
    {
        self.root.get(key)
    }

    #[must_use]
    pub fn get_mut(&mut self, key: &[K]) -> TrieResult<&mut V>
    where
        K: Clone + Ord,
        V: Clone,
    {
        let root = Arc::make_mut(&mut self.root);
        root.get_mut(key)
    }
}

impl<K, V> Default for Trie<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone)]
enum Node<K, V> {
    Branch {
        keys: Vec<K>,
        children: Vec<Arc<Self>>,
    },
    Leaf {
        value: V,
    },
}

impl<K, V> Node<K, V> {
    fn insert(&mut self, key: &[K], value: V) -> Option<V>
    where
        K: Clone + Ord,
        V: Clone,
    {
        match self {
            Self::Branch { keys, children } => {
                let Some((head, tail)) = key.split_first() else {
                    panic!("Overlapping keys are forbidden")
                };
                match keys.binary_search(head) {
                    Ok(index) => {
                        let child = Arc::make_mut(&mut children[index]);
                        child.insert(tail, value)
                    }
                    Err(index) => {
                        keys.insert(index, head.clone());
                        let child = if tail.is_empty() {
                            Arc::new(Self::Leaf { value })
                        } else {
                            let mut branch = Self::default();
                            branch.insert(tail, value);
                            Arc::new(branch)
                        };
                        children.insert(index, child);
                        None
                    }
                }
            }
            Self::Leaf { value: leaf_value } => {
                assert!(key.is_empty(), "Overlapping keys are forbidden");
                Some(std::mem::replace(leaf_value, value))
            }
        }
    }

    fn is_empty(&self) -> bool {
        match self {
            Self::Branch { keys, .. } => keys.is_empty(),
            Self::Leaf { .. } => false,
        }
    }

    fn remove(&mut self, key: &[K]) -> Option<V>
    where
        K: Clone + Ord,
        V: Clone,
    {
        match self {
            Self::Branch { keys, children } => {
                let (head, tail) = key.split_first()?;
                let index = keys.binary_search(head).ok()?;
                let child = Arc::make_mut(&mut children[index]);
                let result = child.remove(tail);
                if child.is_empty() {
                    keys.remove(index);
                    children.remove(index);
                }
                result
            }
            Self::Leaf { .. } => {
                if !key.is_empty() {
                    return None;
                }
                match std::mem::take(self) {
                    Self::Leaf { value } => Some(value),
                    Self::Branch { .. } => unreachable!(),
                }
            }
        }
    }

    #[must_use]
    fn get(&self, key: &[K]) -> TrieResult<&V>
    where
        K: Ord,
    {
        match self {
            Self::Branch { keys, children } => {
                if let Some((head, tail)) = key.split_first() {
                    if let Ok(index) = keys.binary_search(head) {
                        children[index].get(tail)
                    } else {
                        TrieResult::Missing
                    }
                } else {
                    TrieResult::Partial
                }
            }
            Self::Leaf { value } => {
                if key.is_empty() {
                    TrieResult::Found(value)
                } else {
                    TrieResult::Missing
                }
            }
        }
    }

    #[must_use]
    fn get_mut(&mut self, key: &[K]) -> TrieResult<&mut V>
    where
        K: Clone + Ord,
        V: Clone,
    {
        match self {
            Self::Branch { keys, children } => {
                if let Some((head, tail)) = key.split_first() {
                    if let Ok(index) = keys.binary_search(head) {
                        let child = Arc::make_mut(&mut children[index]);
                        child.get_mut(tail)
                    } else {
                        TrieResult::Missing
                    }
                } else {
                    TrieResult::Partial
                }
            }
            Self::Leaf { value } => {
                if key.is_empty() {
                    TrieResult::Found(value)
                } else {
                    TrieResult::Missing
                }
            }
        }
    }
}

impl<K, V> Default for Node<K, V> {
    fn default() -> Self {
        Self::Branch {
            keys: Vec::new(),
            children: Vec::new(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TrieResult<T> {
    Missing,
    Partial,
    Found(T),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn works() {
        let mut trie: Trie<&str, &str> = Trie::new();

        trie.insert(&["1", "+", "2"], "3");
        trie.insert(&["1", "+", "9"], "10");

        assert_eq!(trie.get(&["1"]), TrieResult::Partial);
        assert_eq!(trie.get(&["1", "+"]), TrieResult::Partial);
        assert_eq!(trie.get(&["1", "+", "2"]), TrieResult::Found(&"3"));
        assert_eq!(trie.get(&["1", "+", "9"]), TrieResult::Found(&"10"));
        assert_eq!(trie.get(&["1", "+", "5"]), TrieResult::Missing);
        assert_eq!(trie.get(&["1", "+", "2", "+", "3"]), TrieResult::Missing);

        // remove returns the value, missing keys return None
        assert_eq!(trie.remove(&["1", "+", "5"]), None);
        assert_eq!(trie.remove(&["1"]), None);
        assert_eq!(trie.remove(&["1", "+", "2"]), Some("3"));

        // after removal: exact key is gone, sibling and its parent are intact
        assert_eq!(trie.get(&["1", "+", "2"]), TrieResult::Missing);
        assert_eq!(trie.get(&["1", "+", "9"]), TrieResult::Found(&"10"));
        assert_eq!(trie.get(&["1"]), TrieResult::Partial);

        // removing the last entry prunes empty branches
        assert_eq!(trie.remove(&["1", "+", "9"]), Some("10"));
        assert_eq!(trie.get(&["1"]), TrieResult::Missing);
    }

    #[test]
    #[should_panic]
    fn forbids_overlap() {
        let mut trie: Trie<&str, ()> = Trie::new();
        trie.insert(&["1"], ());
        trie.insert(&["1", "2"], ());
    }
}
