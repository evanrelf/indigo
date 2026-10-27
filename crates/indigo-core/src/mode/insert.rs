#![allow(clippy::enum_glob_use)]

use crate::{
    client::ClientMut,
    keymap::{Keymap, KeymapResult, keymap},
    mode::{Mode, normal},
};
use std::sync::LazyLock;

#[cfg(feature = "arbitrary")]
use arbitrary::Arbitrary;

#[cfg_attr(feature = "arbitrary", derive(Arbitrary))]
#[derive(Debug)]
pub enum Action {
    EnterNormalMode,
    DeleteBefore,
    DeleteAfter,
    InsertChar(char),
}

pub static KEYMAP: LazyLock<Keymap<Vec<Action>>> = LazyLock::new(|| {
    use Action::*;
    keymap! { Vec<Action>;
        "<esc>" => vec![EnterNormalMode], /// Exit insert mode
        "<tab>" => vec![InsertChar('\t')], /// Insert tab
        "<ret>" => vec![InsertChar('\n')], /// Insert newline
        "<bs>" => vec![DeleteBefore], /// Delete before cursor
        "<del>" => vec![DeleteAfter], /// Delete after cursor
        keys => {
            if let [key] = keys
                && let Some(c) = key.typed_char()
            {
                KeymapResult::Fallback(vec![InsertChar(char::from(c))])
            } else {
                KeymapResult::Unmapped
            }
        } /// Insert character
    }
});

pub fn handle_actions(client: &mut ClientMut, actions: &[Action]) {
    for action in actions {
        handle_action(client, action);
    }
}

pub fn handle_action(client: &mut ClientMut, action: &Action) {
    match action {
        Action::EnterNormalMode => normal::enter(client),
        Action::DeleteBefore => delete_before(client),
        Action::DeleteAfter => delete_after(client),
        Action::InsertChar(c) => insert_char(client, *c),
    }
}

pub fn handle_keys(client: &mut ClientMut) -> bool {
    match KEYMAP.get_keys(&client.pending_keys) {
        KeymapResult::Mapped(actions) => {
            client.pending_keys.clear();
            handle_actions(client, actions);
            true
        }
        KeymapResult::Fallback(actions) => {
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
    if client.focused_buffer().text.readonly {
        client.message = Some(Err(String::from("Buffer is readonly")));
        client.count = None;
        return;
    }
    let mut window = client.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.collapse());
    window.scroll_to_selection();
    client.mode = Mode::Insert;
    client.count = None;
}

fn delete_before(client: &mut ClientMut) {
    let mut window = client.focused_window_mut();
    window.selection_edit().delete_before();
    window.scroll_to_selection();
    client.count = None;
}

fn delete_after(client: &mut ClientMut) {
    let mut window = client.focused_window_mut();
    window.selection_edit().delete_after();
    window.scroll_to_selection();
    client.count = None;
}

fn insert_char(client: &mut ClientMut, char: char) {
    let mut window = client.focused_window_mut();
    window.selection_edit().insert_char(char);
    window.scroll_to_selection();
    client.count = None;
}

pub fn paste(client: &mut ClientMut, text: &str) {
    let mut window = client.focused_window_mut();
    window.selection_edit().insert(text);
    window.scroll_to_selection();
    client.count = None;
}
