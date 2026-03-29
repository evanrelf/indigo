use std::sync::Arc;

#[derive(Clone)]
pub struct Trie<K, V> {
    root: Arc<Node<K, V>>,
}

impl<K, V> Trie<K, V> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            root: Arc::new(Node::Branch {
                keys: Vec::new(),
                children: Vec::new(),
            }),
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

    /*
    pub fn remove(&mut self, key: &[K]) -> Option<V>
    where
        K: Clone,
        V: Clone,
    {
        let root = Arc::make_mut(&mut self.root);
        root.remove(key)
    }
    */

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
                if let Some((head, tail)) = key.split_first() {
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
                                let mut branch = Self::Branch {
                                    keys: Vec::new(),
                                    children: Vec::new(),
                                };
                                branch.insert(tail, value);
                                Arc::new(branch)
                            };
                            children.insert(index, child);
                            None
                        }
                    }
                } else {
                    panic!("Overlapping keys are forbidden")
                }
            }
            Self::Leaf { value: leaf_value } => {
                if key.is_empty() {
                    Some(std::mem::replace(leaf_value, value))
                } else {
                    panic!("Overlapping keys are forbidden")
                }
            }
        }
    }

    /*
    fn remove(&mut self, key: &[K]) -> Option<V> {
        match self {
            Self::Branch { keys, children } => todo!(),
            Self::Leaf { value } => todo!(),
        }
    }
    */

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
    }

    #[test]
    #[should_panic]
    fn forbids_overlap() {
        let mut trie: Trie<&str, ()> = Trie::new();
        trie.insert(&["1"], ());
        trie.insert(&["1", "2"], ());
    }
}
