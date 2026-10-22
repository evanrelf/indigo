#![allow(clippy::enum_glob_use)]

use crate::{
    key::{KeyCode, KeyModifiers},
    keymap::{Keymap, KeymapResult, keymap},
    mode::{Mode, normal},
    session::Session,
};
use std::sync::LazyLock;

#[cfg(feature = "arbitrary")]
use arbitrary::Arbitrary;

#[cfg_attr(feature = "arbitrary", derive(Arbitrary))]
#[derive(Debug)]
pub enum Action {
    Replace(u8),
}

pub static KEYMAP: LazyLock<Keymap<Vec<Action>>> = LazyLock::new(|| {
    use Action::*;
    keymap! { Vec<Action>;
        "<esc>" => vec![], /// Cancel
        "<ret>" => vec![Replace(b'\n')], /// Replace with newline
        "<tab>" => vec![Replace(b'\t')], /// Replace with tab
        keys => {
            if let [key] = keys
                && let KeyCode::Char(c) = key.code
                && key.modifiers.difference(KeyModifiers::SHIFT).is_empty()
            {
                KeymapResult::Fallback(vec![Replace(c)])
            } else {
                KeymapResult::Unmapped
            }
        } /// Replace with character
    }
});

pub fn handle_actions(session: &mut Session, actions: &[Action]) {
    for action in actions {
        handle_action(session, action);
    }
}

pub fn handle_action(session: &mut Session, action: &Action) {
    match action {
        Action::Replace(byte) => replace(session, *byte),
    }
}

pub fn handle_keys(session: &mut Session) -> bool {
    match KEYMAP.get_keys(&session.client.pending_keys) {
        KeymapResult::Mapped(actions) => {
            session.client.pending_keys.clear();
            handle_actions(session, actions);
            normal::enter(session);
        }
        KeymapResult::Fallback(actions) => {
            session.client.pending_keys.clear();
            handle_actions(session, &actions);
            normal::enter(session);
        }
        KeymapResult::Unmapped => {
            session.client.pending_keys.clear();
            normal::enter(session);
        }
        KeymapResult::Pending => {}
    }

    true
}

pub fn enter(session: &mut Session) {
    session.client.mode = Mode::Replace;
}

fn replace(session: &mut Session, byte: u8) {
    if session.focused_buffer().text.readonly {
        session.client.message = Some(Err(String::from("Buffer is readonly")));
        session.client.count = None;
        return;
    }
    let mut window = session.focused_window_mut();
    window.selection_edit().replace_each(byte);
    window.scroll_to_selection();
    session.client.count = None;
}
