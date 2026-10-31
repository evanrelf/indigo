pub mod command;
pub mod insert;
pub mod normal;
pub mod prompt;
pub mod view;

use indigo_key::Keymap;
use std::sync::Arc;

#[derive(Clone, Default)]
pub enum Mode {
    #[default]
    Normal,
    Insert,
    Prompt(prompt::State),
    View(view::State),
}

impl Mode {
    #[must_use]
    pub fn kind(&self) -> ModeKind {
        match self {
            Self::Normal => ModeKind::Normal,
            Self::Insert => ModeKind::Insert,
            Self::Prompt(_) => ModeKind::Prompt,
            Self::View(_) => ModeKind::View,
        }
    }
}

#[derive(Clone, Copy)]
pub enum ModeKind {
    Normal,
    Insert,
    Prompt,
    View,
    // TODO: `Command`?
}

#[derive(Clone)]
pub struct Keymaps {
    pub normal: Arc<Keymap<Vec<normal::Action>>>,
    pub insert: Arc<Keymap<Vec<insert::Action>>>,
    pub prompt: Arc<Keymap<Vec<prompt::Action>>>,
    pub view: Arc<Keymap<Vec<view::Action>>>,
}

impl Default for Keymaps {
    fn default() -> Self {
        Self {
            normal: Arc::new(normal::keymap()),
            insert: Arc::new(insert::keymap()),
            prompt: Arc::new(prompt::keymap()),
            view: Arc::new(view::keymap()),
        }
    }
}
