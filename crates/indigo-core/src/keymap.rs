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
    ($value_ty:ty; $($keys:literal => $value:expr,)* $fallback_arg:ident => $fallback_body:block $(,)?) => {{
        let mut keymap $(: crate::keymap::Keymap<$value_ty>)? = crate::keymap::Keymap::new();
        $(keymap.insert($keys, $value);)*
        keymap.set_fallback(|_self, $fallback_arg| $fallback_body);
        keymap
    }};
    ($($keys:literal => $value:expr,)* $fallback_arg:ident => $fallback_body:block $(,)?) => {{
        let mut keymap = crate::keymap::Keymap::new();
        $(keymap.insert($keys, $value);)*
        keymap.set_fallback(|_self, $fallback_arg| $fallback_body);
        keymap
    }};
    ($($value_ty:ty;)? $($keys:literal => $value:expr),* $(,)?) => {{
        let mut keymap $(: crate::keymap::Keymap<$value_ty>)? = crate::keymap::Keymap::new();
        $(keymap.insert($keys, $value);)*
        keymap
    }};
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

    macro_rules! keymap_actions {
        ($($keys:literal => [$($actions:expr),*],)* $fallback_arg:ident => $fallback_body:block $(,)?) => {{
            let mut keymap: crate::keymap::Keymap<::imbl::Vector<Action>> = crate::keymap::Keymap::new();
            $(keymap.insert($keys, ::imbl::Vector::from([$($actions),+]));)+
            keymap.set_fallback(|_self, $fallback_arg| $fallback_body);
            keymap
        }};
        ($($keys:literal => [$($actions:expr),*]),* $(,)?) => {{
            let mut keymap: crate::keymap::Keymap<::imbl::Vector<Action>> = crate::keymap::Keymap::new();
            $(keymap.insert($keys, ::imbl::Vector::from([$($actions),+]));)+
            keymap
        }};
    }

    #[test]
    fn basic() {
        let keymap = keymap_actions! {
            "gj" => [Action("move to bottom")],
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
        let keymap = keymap_actions! {
            "gj" => [Action("move to bottom")],
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
        let _keymap = keymap_actions! {
            "g" => [Action("enter goto mode")],
            "gj" => [Action("move to bottom")],
        };
    }
}
