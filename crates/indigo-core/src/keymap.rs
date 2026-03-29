use crate::{
    key::{Key, Keys},
    trie::{Trie, TrieResult},
};
use imbl::Vector;

#[derive(Clone, Debug, PartialEq)]
pub struct Action(pub &'static str);

pub struct Keymap {
    mappings: Trie<Key, Vector<Action>>,
    fallback: fn(&[Key]) -> KeymapResult,
}

impl Keymap {
    #[must_use]
    pub fn new() -> Self {
        Self {
            mappings: Trie::new(),
            fallback: |_| KeymapResult::Unmapped,
        }
    }

    pub fn insert(&mut self, keys: &str, actions: Vector<Action>) {
        let mut keys: Keys = keys.parse().unwrap();
        for key in &mut keys.0 {
            key.normalize();
        }
        self.mappings.insert(&keys.0, actions);
    }

    pub fn set_fallback(&mut self, fallback: fn(&[Key]) -> KeymapResult) {
        self.fallback = fallback;
    }

    #[must_use]
    pub fn get(&self, keys: &str) -> KeymapResult {
        let mut keys: Keys = keys.parse().unwrap();
        for key in &mut keys.0 {
            key.normalize();
        }
        match self.mappings.get(&keys.0) {
            TrieResult::Missing => (self.fallback)(&keys.0),
            TrieResult::Partial => KeymapResult::Pending,
            TrieResult::Found(actions) => KeymapResult::Mapped(actions.clone()),
        }
    }
}

impl Default for Keymap {
    fn default() -> Self {
        Self::new()
    }
}

#[macro_export]
macro_rules! keymap {
    ($($keys:literal => [$($actions:expr),+],)+ $fallback_arg:ident => $fallback_body:block $(,)?) => {{
        let mut keymap = Keymap::new();
        $(keymap.insert($keys, Vector::from([$($actions),+]));)+
        keymap.set_fallback(|$fallback_arg| $fallback_body);
        keymap
    }};
    ($($keys:literal => [$($actions:expr),+]),+ $(,)?) => {{
        let mut keymap = Keymap::new();
        $(keymap.insert($keys, Vector::from([$($actions),+]));)+
        keymap
    }};
}

#[derive(Debug, PartialEq)]
pub enum KeymapResult {
    Unmapped,
    Pending,
    Mapped(Vector<Action>),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        let keymap = keymap! {
            "gj" => [Action("move to bottom")],
        };
        assert_eq!(keymap.get("x"), KeymapResult::Unmapped);
        assert_eq!(keymap.get("g"), KeymapResult::Pending);
        assert_eq!(
            keymap.get("gj"),
            KeymapResult::Mapped(Vector::from([Action("move to bottom")]))
        );
    }

    #[test]
    fn fallback() {
        let keymap = keymap! {
            "gj" => [Action("move to bottom")],
            keys => {
                if keys.len() == 4 {
                    KeymapResult::Mapped(Vector::from([Action("yay 4")]))
                } else {
                    KeymapResult::Unmapped
                }
            }
        };
        assert_eq!(
            keymap.get("gj"),
            KeymapResult::Mapped(Vector::from([Action("move to bottom")]))
        );
        assert_eq!(keymap.get("123"), KeymapResult::Unmapped);
        assert_eq!(
            keymap.get("1234"),
            KeymapResult::Mapped(Vector::from([Action("yay 4")]))
        );
    }

    #[test]
    #[should_panic]
    fn forbids_overlap() {
        let _keymap = keymap! {
            "g" => [Action("enter goto mode")],
            "gj" => [Action("move to bottom")],
        };
    }
}
