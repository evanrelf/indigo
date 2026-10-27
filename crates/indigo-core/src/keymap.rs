use crate::key::{Key, KeyCode, Keys};
use arrayvec::ArrayVec;
use std::{iter, mem};

enum Trie<K, V> {
    Branch {
        keys: Vec<K>,
        children: Vec<Box<Self>>,
    },
    Leaf {
        value: V,
    },
}

impl<K, V> Trie<K, V> {
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
    fn get<const N: usize>(&self, key: &[K], wildcard: &K) -> TrieResult<Match<'_, K, V, N>>
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
    fn get_mut(&mut self, key: &[K]) -> TrieResult<&mut V>
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

    fn iter<const N: usize>(&self) -> impl Iterator<Item = (ArrayVec<K, N>, &V)>
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
enum TrieResult<T> {
    Missing,
    Partial,
    Found(T),
}

#[derive(Debug, PartialEq)]
struct Match<'a, K, V, const N: usize> {
    value: &'a V,
    captures: ArrayVec<K, N>,
}

pub enum Mapping<V> {
    Static(V),
    Char(fn(u8) -> Option<V>),
}

impl<V> From<V> for Mapping<V> {
    fn from(value: V) -> Self {
        Self::Static(value)
    }
}

struct Entry<V> {
    value: Mapping<V>,
    order: u16,
    description: Option<&'static str>,
}

pub struct Keymap<V> {
    mappings: Trie<Key, Entry<V>>,
    next_order: u16,
    fallback: for<'a> fn(&'a Self, &[Key]) -> KeymapResult<'a, V>,
    fallback_description: Option<&'static str>,
}

impl<V> Keymap<V> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            mappings: Trie::default(),
            next_order: 0,
            fallback: |_, _| KeymapResult::Unmapped,
            fallback_description: None,
        }
    }

    pub fn insert(
        &mut self,
        keys: &str,
        value: impl Into<Mapping<V>>,
        description: Option<&'static str>,
    ) -> Option<Mapping<V>> {
        let value = value.into();
        let mut keys: Keys = keys.parse().unwrap();
        for key in &mut keys.0 {
            key.normalize();
        }
        if let TrieResult::Found(entry) = self.mappings.get_mut(&keys.0) {
            // Keep original place in order, like `IndexMap::insert`
            entry.description = description;
            return Some(mem::replace(&mut entry.value, value));
        }
        let order = self.next_order;
        self.next_order = order.checked_add(1).expect("wow that's a lotta keys");
        self.mappings.insert(
            &keys.0,
            Entry {
                value,
                order,
                description,
            },
        );
        None
    }

    pub fn remove(&mut self, keys: &str) -> Option<Mapping<V>> {
        let mut keys: Keys = keys.parse().unwrap();
        for key in &mut keys.0 {
            key.normalize();
        }
        self.mappings.remove(&keys.0).map(|entry| entry.value)
    }

    pub fn set_fallback(
        &mut self,
        fallback: for<'a> fn(&'a Self, &[Key]) -> KeymapResult<'a, V>,
        description: Option<&'static str>,
    ) {
        self.fallback = fallback;
        self.fallback_description = description;
    }

    #[must_use]
    pub fn get(&self, keys: &str) -> KeymapResult<'_, V> {
        let mut keys: Keys = keys.parse().unwrap();
        for key in &mut keys.0 {
            key.normalize();
        }
        self.get_keys(&keys.0)
    }

    #[must_use]
    pub fn get_keys(&self, keys: &[Key]) -> KeymapResult<'_, V> {
        match self.mappings.get::<3>(keys, &Key::ANY) {
            TrieResult::Missing => {
                if let Some((last, init)) = keys.split_last()
                    && let TrieResult::Found(Match { value: entry, .. }) =
                        self.mappings.get::<3>(init, &Key::ANY)
                    && let Mapping::Char(f) = entry.value
                    && let Some(byte) = key_byte(*last)
                    && let Some(value) = f(byte)
                {
                    KeymapResult::Fallback(value)
                } else {
                    (self.fallback)(self, keys)
                }
            }
            TrieResult::Partial => KeymapResult::Pending,
            TrieResult::Found(Match { value: entry, .. }) => match &entry.value {
                Mapping::Static(value) => KeymapResult::Mapped(value),
                Mapping::Char(_) => KeymapResult::Pending,
            },
        }
    }

    fn entries(&self) -> impl Iterator<Item = (ArrayVec<Key, 3>, &Entry<V>)> {
        let mut pairs: Vec<_> = self.mappings.iter::<3>().collect();
        pairs.sort_by_key(|(_, entry)| entry.order);
        pairs.into_iter()
    }

    pub fn iter(&self) -> impl Iterator<Item = (ArrayVec<Key, 3>, &Mapping<V>)> + '_ {
        self.entries().map(|(keys, entry)| (keys, &entry.value))
    }

    pub fn docs(
        &self,
    ) -> impl Iterator<Item = (Keys<ArrayVec<Key, 3>>, Option<&'static str>)> + '_ {
        self.entries()
            .map(|(keys, entry)| (Keys(keys), entry.description))
    }

    #[must_use]
    pub fn fallback_description(&self) -> Option<&'static str> {
        self.fallback_description
    }
}

impl<'a, V> IntoIterator for &'a Keymap<V> {
    type Item = (ArrayVec<Key, 3>, &'a Mapping<V>);
    type IntoIter = std::vec::IntoIter<(ArrayVec<Key, 3>, &'a Mapping<V>)>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter().collect::<Vec<_>>().into_iter()
    }
}

impl<V> Default for Keymap<V> {
    fn default() -> Self {
        Self::new()
    }
}

fn key_byte(key: Key) -> Option<u8> {
    match key.code {
        KeyCode::Return if key.modifiers.is_empty() => Some(b'\n'),
        KeyCode::Tab if key.modifiers.is_empty() => Some(b'\t'),
        _ => key.typed_char(),
    }
}

macro_rules! keymap {
    // [x] type [x] fallback (base case)
    ($value_ty:ty; $($keys:literal => $value:expr, $(#[doc = $desc:literal])?)* $fallback_arg:ident => $fallback_body:block $(,)? $(#[doc = $fallback_desc:literal])?) => {{
        let mut keymap: $crate::keymap::Keymap<$value_ty> = $crate::keymap::Keymap::new();
        $(keymap.insert($keys, $value, None $(.or(Some($desc.trim())))?);)*
        keymap.set_fallback(|_self, $fallback_arg| $fallback_body, None $(.or(Some($fallback_desc.trim())))?);
        keymap
    }};
    // [x] type [ ] fallback
    ($value_ty:ty; $($keys:literal => $value:expr, $(#[doc = $desc:literal])?)*) => {
        $crate::keymap::keymap! { $value_ty;
            $($keys => $value, $(#[doc = $desc])?)*
            __keys => { $crate::keymap::KeymapResult::Unmapped },
        }
    };
    // [ ] type [x] fallback
    ($($keys:literal => $value:expr, $(#[doc = $desc:literal])?)* $fallback_arg:ident => $fallback_body:block $(,)? $(#[doc = $fallback_desc:literal])?) => {
        $crate::keymap::keymap! { _;
            $($keys => $value, $(#[doc = $desc])?)*
            $fallback_arg => $fallback_body, $(#[doc = $fallback_desc])?
        }
    };
    // [ ] type [ ] fallback
    ($($keys:literal => $value:expr, $(#[doc = $desc:literal])?)*) => {
        $crate::keymap::keymap! { _;
            $($keys => $value, $(#[doc = $desc])?)*
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
    use crate::key::KeyModifiers;

    #[derive(Clone, Debug, PartialEq)]
    struct Action(&'static str);

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
        assert!(matches!(
            keymap.remove("gj"),
            Some(Mapping::Static(value)) if value == vec![Action("move to bottom")]
        ));
        assert_eq!(keymap.get("gj"), KeymapResult::Unmapped);
        // sibling still present
        assert_eq!(
            keymap.get("gk"),
            KeymapResult::Mapped(&vec![Action("move to top")])
        );
        // missing key returns None
        assert!(keymap.remove("x").is_none());
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
            } /// Yay 4
        };
        assert_eq!(keymap.fallback_description(), Some("Yay 4"));
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
    fn keymap_char() {
        let keymap = keymap! { Vec<u8>;
            "f" => Mapping::Char(|byte| Some(vec![byte])),
            "<a-f>" => Mapping::Char(|byte| byte.is_ascii_digit().then(|| vec![byte, byte])),
        };
        assert_eq!(keymap.get("f"), KeymapResult::Pending);
        assert_eq!(keymap.get("<a-f>"), KeymapResult::Pending);
        let f: Key = "f".parse().unwrap();
        let alt_f: Key = "<a-f>".parse().unwrap();
        for byte in 0..=u8::MAX {
            let key = Key {
                modifiers: KeyModifiers::empty(),
                code: KeyCode::Char(byte),
            };
            let shifted = Key {
                modifiers: KeyModifiers::SHIFT,
                ..key
            };
            let alted = Key {
                modifiers: KeyModifiers::ALT,
                ..key
            };
            assert_eq!(
                keymap.get_keys(&[f, key]),
                KeymapResult::Fallback(vec![byte])
            );
            assert_eq!(
                keymap.get_keys(&[f, shifted]),
                KeymapResult::Fallback(vec![byte])
            );
            assert_eq!(keymap.get_keys(&[f, alted]), KeymapResult::Unmapped);
            if byte.is_ascii_digit() {
                assert_eq!(
                    keymap.get_keys(&[alt_f, key]),
                    KeymapResult::Fallback(vec![byte, byte])
                );
            } else {
                assert_eq!(keymap.get_keys(&[alt_f, key]), KeymapResult::Unmapped);
            }
        }
        assert_eq!(keymap.get("f<ret>"), KeymapResult::Fallback(vec![b'\n']));
        assert_eq!(keymap.get("f<tab>"), KeymapResult::Fallback(vec![b'\t']));
        assert_eq!(keymap.get("f<esc>"), KeymapResult::Unmapped);
        assert_eq!(keymap.get("f<s-ret>"), KeymapResult::Unmapped);
        assert_eq!(keymap.get("fxy"), KeymapResult::Unmapped);
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
        let mut keymap = keymap! {
            "gk" => vec![Action("move to top")],
            "x" => vec![Action("select line")],
            "gj" => vec![Action("move to bottom")],
        };
        let values = |keymap: &Keymap<Vec<Action>>| {
            keymap
                .iter()
                .map(|(keys, value)| match value {
                    Mapping::Static(value) => (Keys(keys).to_string(), value[0].0),
                    Mapping::Char(_) => unreachable!(),
                })
                .collect::<Vec<_>>()
        };
        // insertion order, not trie order
        assert_eq!(
            values(&keymap),
            [
                (String::from("gk"), "move to top"),
                (String::from("x"), "select line"),
                (String::from("gj"), "move to bottom"),
            ]
        );
        // reinserting keeps the original position
        keymap.insert("gk", vec![Action("goto top")], None);
    }

    #[test]
    fn keymap_descriptions() {
        let keymap = keymap! {
            "gk" => (), /// Move to top
            "x" => (), // this isn't parsed as a description, because it isn't a doc comment
        };
        let descriptions: Vec<_> = keymap.docs().map(|(_, description)| description).collect();
        assert_eq!(descriptions, [Some("Move to top"), None]);
        assert_eq!(keymap.fallback_description(), None);
    }
}
