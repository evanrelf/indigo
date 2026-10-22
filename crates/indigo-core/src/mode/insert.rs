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
                && let KeyCode::Char(c) = key.code
                && key.modifiers.difference(KeyModifiers::SHIFT).is_empty()
            {
                KeymapResult::Fallback(vec![InsertChar(char::from(c))])
            } else {
                KeymapResult::Unmapped
            }
        } /// Insert character
    }
});

pub fn handle_actions(session: &mut Session, actions: &[Action]) {
    for action in actions {
        handle_action(session, action);
    }
}

pub fn handle_action(session: &mut Session, action: &Action) {
    match action {
        Action::EnterNormalMode => normal::enter(session),
        Action::DeleteBefore => delete_before(session),
        Action::DeleteAfter => delete_after(session),
        Action::InsertChar(c) => insert_char(session, *c),
    }
}

pub fn handle_keys(session: &mut Session) -> bool {
    match KEYMAP.get_keys(&session.pending_keys) {
        KeymapResult::Mapped(actions) => {
            session.pending_keys.clear();
            handle_actions(session, actions);
            true
        }
        KeymapResult::Fallback(actions) => {
            session.pending_keys.clear();
            handle_actions(session, &actions);
            true
        }
        KeymapResult::Unmapped => {
            session.pending_keys.clear();
            false
        }
        KeymapResult::Pending => true,
    }
}

pub fn enter(session: &mut Session) {
    if session.focused_buffer().text.readonly {
        session.message = Some(Err(String::from("Buffer is readonly")));
        session.count = None;
        return;
    }
    let mut window = session.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.collapse());
    window.scroll_to_selection();
    session.mode = Mode::Insert;
    session.count = None;
}

fn delete_before(session: &mut Session) {
    let mut window = session.focused_window_mut();
    window.selection_edit().delete_before();
    window.scroll_to_selection();
    session.count = None;
}

fn delete_after(session: &mut Session) {
    let mut window = session.focused_window_mut();
    window.selection_edit().delete_after();
    window.scroll_to_selection();
    session.count = None;
}

fn insert_char(session: &mut Session, char: char) {
    let mut window = session.focused_window_mut();
    window.selection_edit().insert_char(char);
    window.scroll_to_selection();
    session.count = None;
}

pub fn paste(session: &mut Session, text: &str) {
    let mut window = session.focused_window_mut();
    window.selection_edit().insert(text);
    window.scroll_to_selection();
    session.count = None;
}
