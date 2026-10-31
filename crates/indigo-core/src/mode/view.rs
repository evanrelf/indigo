#![allow(clippy::enum_glob_use)]

use crate::{
    client::ClientMut,
    mode::{Mode, normal},
};
use indigo_key::{Keymap, KeymapResult, keymap};
use std::sync::Arc;

#[cfg(feature = "arbitrary")]
use arbitrary::Arbitrary;

#[cfg_attr(feature = "arbitrary", derive(Arbitrary))]
#[derive(Debug)]
pub enum Action {
    EnterNormalMode,
    ScrollLineUp,
    ScrollLineDown,
    ScrollHalfPageUp,
    ScrollHalfPageDown,
    ScrollFullPageUp,
    ScrollFullPageDown,
    ScrollCenter,
}

#[must_use]
pub fn keymap() -> Keymap<Vec<Action>> {
    use Action::*;
    keymap! { Vec<Action>;
        "<esc>" => vec![EnterNormalMode], /// Exit view mode
        "k" => vec![ScrollLineUp], /// Scroll line up
        "j" => vec![ScrollLineDown], /// Scroll line down
        "u" => vec![ScrollHalfPageUp], /// Scroll half page up
        "d" => vec![ScrollHalfPageDown], /// Scroll half page down
        "b" => vec![ScrollFullPageUp], /// Scroll page up
        "f" => vec![ScrollFullPageDown], /// Scroll page down
        "c" => vec![ScrollCenter], /// Scroll to center cursor
    }
}

pub fn handle_actions(client: &mut ClientMut, actions: &[Action]) {
    for action in actions {
        handle_action(client, action);
    }
}

pub fn handle_action(client: &mut ClientMut, action: &Action) {
    match action {
        Action::EnterNormalMode => normal::enter(client),
        Action::ScrollLineUp => client.focused_window_mut().scroll_line_up(),
        Action::ScrollLineDown => client.focused_window_mut().scroll_line_down(),
        Action::ScrollHalfPageUp => client.focused_window_mut().scroll_half_page_up(),
        Action::ScrollHalfPageDown => client.focused_window_mut().scroll_half_page_down(),
        Action::ScrollFullPageUp => client.focused_window_mut().scroll_full_page_up(),
        Action::ScrollFullPageDown => client.focused_window_mut().scroll_full_page_down(),
        Action::ScrollCenter => client.focused_window_mut().scroll_center_selection(),
    }
}

pub fn handle_keys(client: &mut ClientMut) -> bool {
    let keymap = Arc::clone(&client.keymaps.view);
    match keymap.get_keys(&client.pending_keys) {
        KeymapResult::Mapped(actions) => {
            client.pending_keys.clear();
            handle_actions(client, actions);
            true
        }
        KeymapResult::Computed(actions) => {
            client.pending_keys.clear();
            handle_actions(client, &actions);
            true
        }
        KeymapResult::Unmapped => {
            client.pending_keys.clear();
            false
        }
        KeymapResult::Pending => true,
    }
}

pub fn enter(client: &mut ClientMut) {
    client.mode = Mode::View;
}
