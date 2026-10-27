#![allow(clippy::enum_glob_use)]

use crate::{
    client::ClientMut,
    keymap::{Keymap, KeymapResult, keymap},
    mode::{
        Mode,
        normal::{self, SeekDirection, SeekInclude, SeekSelect},
    },
};
use std::sync::LazyLock;

#[cfg(feature = "arbitrary")]
use arbitrary::Arbitrary;

#[derive(Clone)]
pub struct State {
    /// Move and create a new selection, or extend the current one?
    pub select: SeekSelect,
    /// Include seek target in selection, or stop just short?
    pub include: SeekInclude,
    /// Seek forward or backward?
    pub direction: SeekDirection,
}

#[cfg_attr(feature = "arbitrary", derive(Arbitrary))]
#[derive(Debug)]
pub enum Action {
    Seek(u8),
}

pub static KEYMAP: LazyLock<Keymap<Vec<Action>>> = LazyLock::new(|| {
    use Action::*;
    keymap! { Vec<Action>;
        "<esc>" => vec![], /// Cancel
        "<ret>" => vec![Seek(b'\n')], /// Seek to newline
        "<tab>" => vec![Seek(b'\t')], /// Seek to tab
        keys => {
            if let [key] = keys
                && let Some(c) = key.typed_char()
            {
                KeymapResult::Fallback(vec![Seek(c)])
            } else {
                KeymapResult::Unmapped
            }
        } /// Seek to character
    }
});

pub fn handle_actions(client: &mut ClientMut, actions: &[Action]) {
    for action in actions {
        handle_action(client, action);
    }
}

pub fn handle_action(client: &mut ClientMut, action: &Action) {
    match action {
        Action::Seek(byte) => {
            let Mode::Seek(seek_mode) = &client.mode else {
                panic!("Not in seek mode")
            };
            let (select, include, direction) =
                (seek_mode.select, seek_mode.include, seek_mode.direction);
            normal::seek(client, select, include, direction, *byte);
        }
    }
}

pub fn handle_keys(client: &mut ClientMut) -> bool {
    match KEYMAP.get_keys(&client.pending_keys) {
        KeymapResult::Mapped(actions) => {
            client.pending_keys.clear();
            handle_actions(client, actions);
            normal::enter(client);
        }
        KeymapResult::Fallback(actions) => {
            client.pending_keys.clear();
            handle_actions(client, &actions);
            normal::enter(client);
        }
        KeymapResult::Unmapped => {
            client.pending_keys.clear();
            normal::enter(client);
        }
        KeymapResult::Pending => {}
    }

    true
}

pub fn enter(
    client: &mut ClientMut,
    select: SeekSelect,
    include: SeekInclude,
    direction: SeekDirection,
) {
    client.mode = Mode::Seek(State {
        select,
        include,
        direction,
    });
}
