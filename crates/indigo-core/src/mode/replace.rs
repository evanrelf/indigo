#![allow(clippy::enum_glob_use)]

use crate::{
    client::ClientMut,
    key::{KeyCode, KeyModifiers},
    keymap::{Keymap, KeymapResult, keymap},
    mode::{Mode, normal},
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

pub fn handle_actions(client: &mut ClientMut, actions: &[Action]) {
    for action in actions {
        handle_action(client, action);
    }
}

pub fn handle_action(client: &mut ClientMut, action: &Action) {
    match action {
        Action::Replace(byte) => replace(client, *byte),
    }
}

pub fn handle_keys(client: &mut ClientMut) -> bool {
    match KEYMAP.get_keys(&client.state().pending_keys) {
        KeymapResult::Mapped(actions) => {
            client.state_mut().pending_keys.clear();
            handle_actions(client, actions);
            normal::enter(client);
        }
        KeymapResult::Fallback(actions) => {
            client.state_mut().pending_keys.clear();
            handle_actions(client, &actions);
            normal::enter(client);
        }
        KeymapResult::Unmapped => {
            client.state_mut().pending_keys.clear();
            normal::enter(client);
        }
        KeymapResult::Pending => {}
    }

    true
}

pub fn enter(client: &mut ClientMut) {
    client.state_mut().mode = Mode::Replace;
}

fn replace(client: &mut ClientMut, byte: u8) {
    if client.focused_buffer().text.readonly {
        client.state_mut().message = Some(Err(String::from("Buffer is readonly")));
        client.state_mut().count = None;
        return;
    }
    let mut window = client.focused_window_mut();
    window.selection_edit().replace_each(byte);
    window.scroll_to_selection();
    client.state_mut().count = None;
}
