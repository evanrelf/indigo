use crate::{
    key::{Key, Keys},
    trie::{Trie, TrieResult},
};

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

    pub fn insert(&mut self, keys: &str, value: V) {
        let mut keys: Keys = keys.parse().unwrap();
        for key in &mut keys.0 {
            key.normalize();
        }
        self.mappings.insert(&keys.0, value);
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
    use imbl::Vector;

    #[derive(Clone, Debug, PartialEq)]
    pub struct Action(pub &'static str);

    #[test]
    fn basic() {
        let keymap = keymap! {
            "gj" => Vector::from([Action("move to bottom")]),
        };
        assert_eq!(keymap.get("x"), KeymapResult::Unmapped);
        assert_eq!(keymap.get("g"), KeymapResult::Pending);
        assert_eq!(
            keymap.get("gj"),
            KeymapResult::Mapped(&Vector::from([Action("move to bottom")]))
        );
    }

    #[test]
    fn fallback() {
        let keymap = keymap! {
            "gj" => Vector::from([Action("move to bottom")]),
            keys => {
                if keys.len() == 4 {
                    KeymapResult::Fallback(Vector::from([Action("yay 4")]))
                } else {
                    KeymapResult::Unmapped
                }
            }
        };
        assert_eq!(
            keymap.get("gj"),
            KeymapResult::Mapped(&Vector::from([Action("move to bottom")]))
        );
        assert_eq!(keymap.get("123"), KeymapResult::Unmapped);
        assert_eq!(
            keymap.get("1234"),
            KeymapResult::Fallback(Vector::from([Action("yay 4")]))
        );
    }

    #[test]
    #[should_panic]
    fn forbids_overlap() {
        let _keymap = keymap! {
            "g" => Vector::from([Action("enter goto mode")]),
            "gj" => Vector::from([Action("move to bottom")]),
        };
    }
}
