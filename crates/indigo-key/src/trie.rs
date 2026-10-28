use arrayvec::ArrayVec;
use std::iter;

pub enum Trie<K, V> {
    Branch {
        keys: Vec<K>,
        children: Vec<Box<Self>>,
    },
    Leaf {
        value: V,
    },
}

impl<K, V> Trie<K, V> {
    pub fn insert(&mut self, key: &[K], value: V) -> Option<V>
    where
        K: Clone + Ord,
    {
        match self {
            Self::Branch { keys, children } => {
                let Some((head, tail)) = key.split_first() else {
                    panic!("Overlapping keys are forbidden")
                };
                match keys.binary_search(head) {
                    Ok(index) => children[index].insert(tail, value),
                    Err(index) => {
                        keys.insert(index, head.clone());
                        let child = if tail.is_empty() {
                            Box::new(Self::Leaf { value })
                        } else {
                            let mut branch = Self::default();
                            branch.insert(tail, value);
                            Box::new(branch)
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

    pub fn remove(&mut self, key: &[K]) -> Option<V>
    where
        K: Ord,
    {
        match self {
            Self::Branch { keys, children } => {
                let (head, tail) = key.split_first()?;
                let index = keys.binary_search(head).ok()?;
                let result = children[index].remove(tail);
                if children[index].is_empty() {
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
    fn child(&self, key: &K) -> Option<&Self>
    where
        K: Ord,
    {
        match self {
            Self::Branch { keys, children } => {
                let index = keys.binary_search(key).ok()?;
                Some(&children[index])
            }
            Self::Leaf { .. } => None,
        }
    }

    #[must_use]
    fn child_mut(&mut self, key: &K) -> Option<&mut Self>
    where
        K: Ord,
    {
        match self {
            Self::Branch { keys, children } => {
                let index = keys.binary_search(key).ok()?;
                Some(&mut children[index])
            }
            Self::Leaf { .. } => None,
        }
    }

    #[must_use]
    pub fn get<const N: usize>(&self, key: &[K], wildcard: &K) -> TrieResult<Match<'_, K, V, N>>
    where
        K: Clone + Ord,
    {
        let mut trie = self;
        let mut captures = ArrayVec::new();
        for k in key {
            trie = if let Some(child) = trie.child(k) {
                child
            } else if let Some(child) = trie.child(wildcard) {
                captures.push(k.clone());
                child
            } else {
                return TrieResult::Missing;
            };
        }
        match trie {
            Self::Branch { .. } => TrieResult::Partial,
            Self::Leaf { value } => TrieResult::Found(Match { value, captures }),
        }
    }

    #[must_use]
    pub fn get_mut(&mut self, key: &[K]) -> TrieResult<&mut V>
    where
        K: Ord,
    {
        match key.split_first() {
            Some((head, tail)) => match self.child_mut(head) {
                Some(child) => child.get_mut(tail),
                None => TrieResult::Missing,
            },
            None => match self {
                Self::Branch { .. } => TrieResult::Partial,
                Self::Leaf { value } => TrieResult::Found(value),
            },
        }
    }

    pub fn iter<const N: usize>(&self) -> impl Iterator<Item = (ArrayVec<K, N>, &V)>
    where
        K: Clone,
    {
        let mut pairs = Vec::new();
        self.collect_pairs::<N>(&mut ArrayVec::new(), &mut pairs);
        pairs.into_iter()
    }

    fn collect_pairs<'a, const N: usize>(
        &'a self,
        prefix: &mut ArrayVec<K, N>,
        pairs: &mut Vec<(ArrayVec<K, N>, &'a V)>,
    ) where
        K: Clone,
    {
        match self {
            Self::Branch { keys, children } => {
                for (key, child) in iter::zip(keys, children) {
                    prefix.push(key.clone());
                    child.collect_pairs(prefix, pairs);
                    prefix.pop();
                }
            }
            Self::Leaf { value } => {
                pairs.push((prefix.clone(), value));
            }
        }
    }
}

impl<K, V> Default for Trie<K, V> {
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

#[derive(Debug, PartialEq)]
pub struct Match<'a, K, V, const N: usize> {
    pub value: &'a V,
    pub captures: ArrayVec<K, N>,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn found<'a, K: Clone, V, const N: usize>(
        value: &'a V,
        captures: &[K],
    ) -> TrieResult<Match<'a, K, V, N>> {
        let captures = captures.iter().cloned().collect();
        TrieResult::Found(Match { value, captures })
    }

    #[test]
    fn trie_works() {
        let mut trie: Trie<&str, &str> = Trie::default();

        trie.insert(&["1", "+", "2"], "3");
        trie.insert(&["1", "+", "9"], "10");

        assert_eq!(trie.get::<3>(&["1"], &"*"), TrieResult::Partial);
        assert_eq!(trie.get::<3>(&["1", "+"], &"*"), TrieResult::Partial);
        assert_eq!(trie.get::<3>(&["1", "+", "2"], &"*"), found(&"3", &[]));
        assert_eq!(trie.get::<3>(&["1", "+", "9"], &"*"), found(&"10", &[]));
        assert_eq!(trie.get::<3>(&["1", "+", "5"], &"*"), TrieResult::Missing);
        assert_eq!(
            trie.get::<3>(&["1", "+", "2", "+", "3"], &"*"),
            TrieResult::Missing
        );

        // remove returns the value, missing keys return None
        assert_eq!(trie.remove(&["1", "+", "5"]), None);
        assert_eq!(trie.remove(&["1"]), None);
        assert_eq!(trie.remove(&["1", "+", "2"]), Some("3"));

        // after removal: exact key is gone, sibling and its parent are intact
        assert_eq!(trie.get::<3>(&["1", "+", "2"], &"*"), TrieResult::Missing);
        assert_eq!(trie.get::<3>(&["1", "+", "9"], &"*"), found(&"10", &[]));
        assert_eq!(trie.get::<3>(&["1"], &"*"), TrieResult::Partial);

        // removing the last entry prunes empty branches
        assert_eq!(trie.remove(&["1", "+", "9"]), Some("10"));
        assert_eq!(trie.get::<3>(&["1"], &"*"), TrieResult::Missing);
    }

    #[test]
    fn trie_wildcard() {
        let mut trie: Trie<&str, &str> = Trie::default();

        trie.insert(&["g", "k"], "gk");
        trie.insert(&["g", "*"], "g*");
        trie.insert(&["*", "x"], "*x");

        assert_eq!(trie.get::<3>(&["g"], &"*"), TrieResult::Partial);
        // exact key wins over wildcard
        assert_eq!(trie.get::<3>(&["g", "k"], &"*"), found(&"gk", &[]));
        assert_eq!(trie.get::<3>(&["g", "z"], &"*"), found(&"g*", &["z"]));
        // exact `g` was taken, so `* x` isn't considered
        assert_eq!(trie.get::<3>(&["g", "x"], &"*"), found(&"g*", &["x"]));
        // the wildcard key itself matches exactly, so isn't captured
        assert_eq!(trie.get::<3>(&["g", "*"], &"*"), found(&"g*", &[]));
        assert_eq!(trie.get::<3>(&["q"], &"*"), TrieResult::Partial);
        assert_eq!(trie.get::<3>(&["q", "x"], &"*"), found(&"*x", &["q"]));
        assert_eq!(trie.get::<3>(&["q", "y"], &"*"), TrieResult::Missing);
    }

    #[test]
    #[should_panic]
    fn trie_forbids_overlap() {
        let mut trie: Trie<&str, ()> = Trie::default();
        trie.insert(&["1"], ());
        trie.insert(&["1", "2"], ());
    }
}
