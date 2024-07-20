use crate::{Editor, Key, Mode};
use std::collections::HashMap;

#[derive(Default)]
pub struct Keymap {
    mappings: HashMap<(Mode, Key), fn(&mut Editor)>,
}

impl Keymap {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn map(&mut self, mode: &str, key: &str, action: fn(&mut Editor)) {
        let mode = mode.parse().unwrap();
        let key = key.parse().unwrap();
        self.mappings.insert((mode, key), action);
    }

    #[must_use]
    pub fn get(&self, mode: Mode, key: impl Into<Key>) -> Option<fn(&mut Editor)> {
        self.mappings.get(&(mode, key.into())).copied()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        use crate::{actions, KeyCode::Backspace, KeyModifier::Control};

        let mut keymap = Keymap::new();
        keymap.map("normal", "<ctrl-c>", |_| panic!());
        keymap.map("normal", "h", actions::move_left);
        keymap.map("insert", "<backspace>", actions::backspace);

        assert!(keymap.get(Mode::Normal, (Control, 'c')).is_some());
        assert!(keymap.get(Mode::Normal, 'h').is_some());
        assert!(keymap.get(Mode::Insert, Backspace).is_some());
    }
}
