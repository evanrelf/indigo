#![allow(clippy::enum_glob_use)]

use crate::{
    editor::Editor,
    key::KeyCode,
    keymap::{Keymap, KeymapResult, keymap},
};
use std::sync::LazyLock;

#[cfg(any(feature = "arbitrary", test))]
use arbitrary::Arbitrary;

#[cfg_attr(any(feature = "arbitrary", test), derive(Arbitrary))]
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
        "<esc>" => vec![EnterNormalMode],
        "<bs>" => vec![DeleteBefore],
        "<del>" => vec![DeleteAfter],
        "<s-\\ >" => vec![InsertChar(' ')],
        "<ret>" => vec![InsertChar('\n')],
        "<tab>" => vec![InsertChar('\t')],
        keys => {
            if let [key] = keys
                && key.modifiers.is_empty()
                && let KeyCode::Char(c) = key.code
            {
                KeymapResult::Fallback(vec![InsertChar(char::from(c))])
            } else {
                KeymapResult::Unmapped
            }
        }
    }
});

pub fn handle_actions(editor: &mut Editor, actions: &[Action]) {
    for action in actions {
        handle_action(editor, action);
    }
}

pub fn handle_action(editor: &mut Editor, action: &Action) {
    match action {
        Action::EnterNormalMode => _ = editor.pop_mode(),
        Action::DeleteBefore => delete_before(editor),
        Action::DeleteAfter => delete_after(editor),
        Action::InsertChar(c) => insert_char(editor, *c),
    }
}

pub fn handle_keys(editor: &mut Editor) -> bool {
    match KEYMAP.get_keys(&editor.pending_keys) {
        KeymapResult::Mapped(actions) => {
            editor.pending_keys.clear();
            handle_actions(editor, actions);
            true
        }
        KeymapResult::Fallback(actions) => {
            editor.pending_keys.clear();
            handle_actions(editor, &actions);
            true
        }
        KeymapResult::Unmapped => {
            editor.pending_keys.clear();
            false
        }
        KeymapResult::Pending => true,
    }
}

pub fn on_create(_editor: &mut Editor) {}

pub fn on_destroy(editor: &mut Editor) {
    editor.focused_buffer_mut().text.commit();
}

pub fn on_focus(_editor: &mut Editor) {}

pub fn on_blur(_editor: &mut Editor) {}

fn delete_before(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window.selection_mut().delete_before();
    window.scroll_to_selection();
    editor.count = None;
}

fn delete_after(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window.selection_mut().delete_after();
    window.scroll_to_selection();
    editor.count = None;
}

fn insert_char(editor: &mut Editor, char: char) {
    let mut window = editor.focused_window_mut();
    window.selection_mut().insert_char(char);
    window.scroll_to_selection();
    editor.count = None;
}

pub fn paste(editor: &mut Editor, text: &str) {
    let mut window = editor.focused_window_mut();
    window.selection_mut().insert(text);
    window.scroll_to_selection();
    editor.count = None;
}
