#![allow(clippy::enum_glob_use)]

use crate::{
    editor::Editor,
    keymap::{Keymap, KeymapResult, keymap},
    mode::normal::enter_normal_mode,
};
use std::sync::LazyLock;

pub enum Action {
    MoveToStart,
    ExtendToStart,
    MoveToBottom,
    ExtendToBottom,
    MoveToEnd,
    ExtendToEnd,
    MoveToLineStart,
    ExtendToLineStart,
    MoveToLineNonBlankStart,
    ExtendToLineNonBlankStart,
    MoveUntilLineEnd,
    ExtendUntilLineEnd,
}

pub static KEYMAP: LazyLock<Keymap<Vec<Action>>> = LazyLock::new(|| {
    use Action::*;
    keymap! { Vec<Action>;
        "k" => vec![MoveToStart],
        "K" => vec![ExtendToStart],
        "j" => vec![MoveToBottom],
        "J" => vec![ExtendToBottom],
        "e" => vec![MoveToEnd],
        "E" => vec![ExtendToEnd],
        "h" => vec![MoveToLineStart],
        "H" => vec![ExtendToLineStart],
        "i" => vec![MoveToLineNonBlankStart],
        "I" => vec![ExtendToLineNonBlankStart],
        "l" => vec![MoveUntilLineEnd],
        "L" => vec![ExtendUntilLineEnd],
    }
});

pub fn handle_event_goto(editor: &mut Editor) -> bool {
    match KEYMAP.get_keys(&editor.pending_keys) {
        KeymapResult::Mapped(actions) => {
            editor.pending_keys.clear();
            handle_actions_goto(editor, actions);
            enter_normal_mode(editor);
        }
        KeymapResult::Fallback(actions) => {
            editor.pending_keys.clear();
            handle_actions_goto(editor, &actions);
            enter_normal_mode(editor);
        }
        KeymapResult::Unmapped => {
            editor.pending_keys.clear();
            enter_normal_mode(editor);
        }
        KeymapResult::Pending => {}
    }

    true
}

pub fn handle_actions_goto(editor: &mut Editor, actions: &[Action]) {
    for action in actions {
        handle_action_goto(editor, action);
    }
}

pub fn handle_action_goto(editor: &mut Editor, action: &Action) {
    let mut window = editor.focused_window_mut();

    match action {
        Action::MoveToStart => {
            window
                .selection_mut()
                .for_each_mut(|mut range| range.move_to_start());
            window.scroll_to_selection();
        }
        Action::ExtendToStart => {
            window
                .selection_mut()
                .for_each_mut(|mut range| range.extend_to_start());
            window.scroll_to_selection();
        }
        Action::MoveToBottom => {
            window
                .selection_mut()
                .for_each_mut(|mut range| range.move_to_bottom());
            window.scroll_to_selection();
        }
        Action::ExtendToBottom => {
            window
                .selection_mut()
                .for_each_mut(|mut range| range.extend_to_bottom());
            window.scroll_to_selection();
        }
        Action::MoveToEnd => {
            window
                .selection_mut()
                .for_each_mut(|mut range| range.move_to_end());
            window.scroll_to_selection();
        }
        Action::ExtendToEnd => {
            window
                .selection_mut()
                .for_each_mut(|mut range| range.extend_to_end());
            window.scroll_to_selection();
        }
        Action::MoveToLineStart => {
            window
                .selection_mut()
                .for_each_mut(|mut range| range.move_to_line_start());
        }
        Action::ExtendToLineStart => {
            window
                .selection_mut()
                .for_each_mut(|mut range| range.extend_to_line_start());
        }
        Action::MoveToLineNonBlankStart => {
            window
                .selection_mut()
                .for_each_mut(|mut range| range.move_to_line_non_blank_start());
        }
        Action::ExtendToLineNonBlankStart => {
            window
                .selection_mut()
                .for_each_mut(|mut range| range.extend_to_line_non_blank_start());
        }
        Action::MoveUntilLineEnd => {
            window
                .selection_mut()
                .for_each_mut(|mut range| range.move_until_line_end());
        }
        Action::ExtendUntilLineEnd => {
            window
                .selection_mut()
                .for_each_mut(|mut range| range.extend_until_line_end());
        }
    }

    editor.mode.set_count(None);
}
