use crate::key::{Key, Keys};
use arrayvec::ArrayVec;
use std::iter;

struct Trie<K, V> {
    root: Node<K, V>,
}

#[expect(unused)]
impl<K, V> Trie<K, V> {
    #[must_use]
    fn new() -> Self {
        Self {
            root: Node::default(),
        }
    }

    fn insert(&mut self, key: &[K], value: V) -> Option<V>
    where
        K: Clone + Ord,
    {
        self.root.insert(key, value)
    }

    fn remove(&mut self, key: &[K]) -> Option<V>
    where
        K: Ord,
    {
        self.root.remove(key)
    }

    #[must_use]
    fn get(&self, key: &[K]) -> TrieResult<&V>
    where
        K: Ord,
    {
        self.root.get(key)
    }

    #[must_use]
    fn get_mut(&mut self, key: &[K]) -> TrieResult<&mut V>
    where
        K: Ord,
    {
        self.root.get_mut(key)
    }

    fn iter<const N: usize>(&self) -> impl Iterator<Item = (ArrayVec<K, N>, &V)>
    where
        K: Clone,
    {
        let mut pairs = Vec::new();
        self.root
            .collect_pairs::<N>(&mut ArrayVec::new(), &mut pairs);
        pairs.into_iter()
    }
}

impl<K, V> Default for Trie<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

enum Node<K, V> {
    Branch {
        keys: Vec<K>,
        children: Vec<Box<Self>>,
    },
    Leaf {
        value: V,
    },
}

impl<K, V> Node<K, V> {
    fn insert(&mut self, key: &[K], value: V) -> Option<V>
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

    fn remove(&mut self, key: &[K]) -> Option<V>
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
        K: Ord,
    {
        match self {
            Self::Branch { keys, children } => {
                if let Some((head, tail)) = key.split_first() {
                    if let Ok(index) = keys.binary_search(head) {
                        children[index].get_mut(tail)
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

impl<K, V> Default for Node<K, V> {
    fn default() -> Self {
        Self::Branch {
            keys: Vec::new(),
            children: Vec::new(),
        }
    }
}

#[derive(Debug, PartialEq)]
enum TrieResult<T> {
    Missing,
    Partial,
    Found(T),
}

pub struct Keymap<V> {
    mappings: Trie<Key, V>,
    fallback: for<'a> fn(&'a Self, &[Key]) -> KeymapResult<'a, V>,
}

impl<V> Keymap<V> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            mappings: Trie::new(),
            fallback: |_, _| KeymapResult::Unmapped,
        }
    }

    pub fn insert(&mut self, keys: &str, value: V) -> Option<V> {
        let mut keys: Keys = keys.parse().unwrap();
        for key in &mut keys.0 {
            key.normalize();
        }
        self.mappings.insert(&keys.0, value)
    }

    pub fn remove(&mut self, keys: &str) -> Option<V> {
        let mut keys: Keys = keys.parse().unwrap();
        for key in &mut keys.0 {
            key.normalize();
        }
        self.mappings.remove(&keys.0)
    }

    pub fn set_fallback(&mut self, fallback: for<'a> fn(&'a Self, &[Key]) -> KeymapResult<'a, V>) {
        self.fallback = fallback;
    }

    #[must_use]
    pub fn get(&self, keys: &str) -> KeymapResult<'_, V> {
        let mut keys: Keys = keys.parse().unwrap();
        for key in &mut keys.0 {
            key.normalize();
        }
        match self.mappings.get(&keys.0) {
            TrieResult::Missing => (self.fallback)(self, &keys.0),
            TrieResult::Partial => KeymapResult::Pending,
            TrieResult::Found(value) => KeymapResult::Mapped(value),
        }
    }

    #[must_use]
    pub fn get_keys(&self, keys: &[Key]) -> KeymapResult<'_, V> {
        match self.mappings.get(keys) {
            TrieResult::Missing => (self.fallback)(self, keys),
            TrieResult::Partial => KeymapResult::Pending,
            TrieResult::Found(value) => KeymapResult::Mapped(value),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (ArrayVec<Key, 3>, &V)> + '_ {
        self.mappings.iter::<3>()
    }
}

impl<'a, V> IntoIterator for &'a Keymap<V> {
    type Item = (ArrayVec<Key, 3>, &'a V);
    type IntoIter = std::vec::IntoIter<(ArrayVec<Key, 3>, &'a V)>;
    fn into_iter(self) -> Self::IntoIter {
        let mut pairs = Vec::new();
        self.mappings
            .root
            .collect_pairs::<3>(&mut ArrayVec::new(), &mut pairs);
        pairs.into_iter()
    }
}

impl<V> Default for Keymap<V> {
    fn default() -> Self {
        Self::new()
    }
}

macro_rules! keymap {
    // [x] type [x] fallback (base case)
    ($value_ty:ty; $($keys:literal => $value:expr,)* $fallback_arg:ident => $fallback_body:block $(,)?) => {{
        let mut keymap: $crate::keymap::Keymap<$value_ty> = $crate::keymap::Keymap::new();
        $(keymap.insert($keys, $value);)*
        keymap.set_fallback(|_self, $fallback_arg| $fallback_body);
        keymap
    }};
    // [x] type [ ] fallback
    ($value_ty:ty; $($keys:literal => $value:expr),* $(,)?) => {
        $crate::keymap::keymap! { $value_ty;
            $($keys => $value,)*
            __keys => { $crate::keymap::KeymapResult::Unmapped },
        }
    };
    // [ ] type [x] fallback
    ($($keys:literal => $value:expr,)* $fallback_arg:ident => $fallback_body:block $(,)?) => {
        $crate::keymap::keymap! { _;
            $($keys => $value,)*
            $fallback_arg => $fallback_body,
        }
    };
    // [ ] type [ ] fallback
    ($($keys:literal => $value:expr),* $(,)?) => {
        $crate::keymap::keymap! { _;
            $($keys => $value,)*
            __keys => { $crate::keymap::KeymapResult::Unmapped },
        }
    };
}

pub(crate) use keymap;

#[derive(Debug, PartialEq)]
pub enum KeymapResult<'a, V> {
    Unmapped,
    Pending,
    Mapped(&'a V),
    Fallback(V),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone, Debug, PartialEq)]
    struct Action(&'static str);

    #[test]
    fn trie_works() {
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
    fn trie_forbids_overlap() {
        let mut trie: Trie<&str, ()> = Trie::new();
        trie.insert(&["1"], ());
        trie.insert(&["1", "2"], ());
    }

    #[test]
    fn keymap_get() {
        let keymap = keymap! {
            "gj" => vec![Action("move to bottom")],
        };
        assert_eq!(keymap.get("x"), KeymapResult::Unmapped);
        assert_eq!(keymap.get("g"), KeymapResult::Pending);
        assert_eq!(
            keymap.get("gj"),
            KeymapResult::Mapped(&vec![Action("move to bottom")])
        );
    }

    #[test]
    fn keymap_remove() {
        let mut keymap = keymap! {
            "gj" => vec![Action("move to bottom")],
            "gk" => vec![Action("move to top")],
        };
        assert_eq!(
            keymap.get("gj"),
            KeymapResult::Mapped(&vec![Action("move to bottom")])
        );
        assert_eq!(keymap.remove("gj"), Some(vec![Action("move to bottom")]));
        assert_eq!(keymap.get("gj"), KeymapResult::Unmapped);
        // sibling still present
        assert_eq!(
            keymap.get("gk"),
            KeymapResult::Mapped(&vec![Action("move to top")])
        );
        // missing key returns None
        assert_eq!(keymap.remove("x"), None);
    }

    #[test]
    fn keymap_fallback() {
        let keymap = keymap! {
            "gj" => vec![Action("move to bottom")],
            keys => {
                if keys.len() == 4 {
                    KeymapResult::Fallback(vec![Action("yay 4")])
                } else {
                    KeymapResult::Unmapped
                }
            }
        };
        assert_eq!(
            keymap.get("gj"),
            KeymapResult::Mapped(&vec![Action("move to bottom")])
        );
        assert_eq!(keymap.get("123"), KeymapResult::Unmapped);
        assert_eq!(
            keymap.get("1234"),
            KeymapResult::Fallback(vec![Action("yay 4")])
        );
    }

    #[test]
    #[should_panic]
    fn keymap_forbids_overlap() {
        let _keymap = keymap! {
            "g" => vec![Action("enter goto mode")],
            "gj" => vec![Action("move to bottom")],
        };
    }

    #[test]
    fn keymap_iter() {
        let keymap = keymap! {
            "gj" => vec![Action("move to bottom")],
            "gk" => vec![Action("move to top")],
        };
        let mut pairs: Vec<_> = keymap.iter().collect();
        pairs.sort_by_key(|(keys, _)| keys.clone());
        assert_eq!(pairs.len(), 2);
        assert_eq!(pairs[0].1, &vec![Action("move to bottom")]);
        assert_eq!(pairs[1].1, &vec![Action("move to top")]);
    }
}
