#![allow(clippy::enum_glob_use)]

use crate::{
    editor::Editor,
    key::KeyCode,
    keymap::{Keymap, KeymapResult, keymap},
    mode::{Mode, normal},
};
use std::sync::LazyLock;

#[cfg(any(feature = "arbitrary", test))]
use arbitrary::Arbitrary;

#[cfg_attr(any(feature = "arbitrary", test), derive(Arbitrary))]
#[derive(Debug)]
pub enum Action {
    Replace(u8),
}

pub static KEYMAP: LazyLock<Keymap<Vec<Action>>> = LazyLock::new(|| {
    use Action::*;
    keymap! { Vec<Action>;
        "<ret>" => vec![Replace(b'\n')],
        "<tab>" => vec![Replace(b'\t')],
        keys => {
            if let [key] = keys
                && key.modifiers.is_empty()
                && let KeyCode::Char(c) = key.code
            {
                KeymapResult::Fallback(vec![Replace(c)])
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
        Action::Replace(byte) => replace(editor, *byte),
    }
}

pub fn handle_keys(editor: &mut Editor) -> bool {
    match KEYMAP.get_keys(&editor.pending_keys) {
        KeymapResult::Mapped(actions) => {
            editor.pending_keys.clear();
            handle_actions(editor, actions);
            normal::enter(editor);
        }
        KeymapResult::Fallback(actions) => {
            editor.pending_keys.clear();
            handle_actions(editor, &actions);
            normal::enter(editor);
        }
        KeymapResult::Unmapped => {
            editor.pending_keys.clear();
            normal::enter(editor);
        }
        KeymapResult::Pending => {}
    }

    true
}

pub fn enter(editor: &mut Editor) {
    editor.mode = Mode::Replace;
}

fn replace(editor: &mut Editor, byte: u8) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        editor.count = None;
        return;
    }
    let mut window = editor.focused_window_mut();
    window.selection_mut().replace_each(byte);
    window.scroll_to_selection();
    editor.count = None;
}
