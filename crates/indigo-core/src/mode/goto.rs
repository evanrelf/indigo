#![allow(clippy::enum_glob_use)]

use crate::{
    editor::Editor,
    event::{Event, KeyEvent},
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

pub fn handle_event_goto(editor: &mut Editor, event: &Event) -> bool {
    match event {
        Event::Key(KeyEvent { key, .. }) => match KEYMAP.get_keys(&[*key]) {
            KeymapResult::Mapped(actions) => handle_actions_goto(editor, actions),
            KeymapResult::Fallback(actions) => handle_actions_goto(editor, &actions),
            KeymapResult::Unmapped | KeymapResult::Pending => {}
        },
    }

    enter_normal_mode(editor);

    true
}

pub fn handle_actions_goto(editor: &mut Editor, actions: &[Action]) {
    for action in actions {
        handle_action_goto(editor, action);
    }
}

pub fn handle_action_goto(editor: &mut Editor, action: &Action) {
    match action {
        Action::MoveToStart => {
            let mut window = editor.focused_window_mut();
            window
                .selection_mut()
                .for_each_mut(|mut range| range.move_to_start());
            window.scroll_to_selection();
            editor.mode.set_count(None);
        }
        Action::ExtendToStart => {
            let mut window = editor.focused_window_mut();
            window
                .selection_mut()
                .for_each_mut(|mut range| range.extend_to_start());
            window.scroll_to_selection();
            editor.mode.set_count(None);
        }
        Action::MoveToBottom => {
            let mut window = editor.focused_window_mut();
            window
                .selection_mut()
                .for_each_mut(|mut range| range.move_to_bottom());
            window.scroll_to_selection();
            editor.mode.set_count(None);
        }
        Action::ExtendToBottom => {
            let mut window = editor.focused_window_mut();
            window
                .selection_mut()
                .for_each_mut(|mut range| range.extend_to_bottom());
            window.scroll_to_selection();
            editor.mode.set_count(None);
        }
        Action::MoveToEnd => {
            let mut window = editor.focused_window_mut();
            window
                .selection_mut()
                .for_each_mut(|mut range| range.move_to_end());
            window.scroll_to_selection();
            editor.mode.set_count(None);
        }
        Action::ExtendToEnd => {
            let mut window = editor.focused_window_mut();
            window
                .selection_mut()
                .for_each_mut(|mut range| range.extend_to_end());
            window.scroll_to_selection();
            editor.mode.set_count(None);
        }
        Action::MoveToLineStart => {
            editor
                .focused_window_mut()
                .selection_mut()
                .for_each_mut(|mut range| range.move_to_line_start());
            editor.mode.set_count(None);
        }
        Action::ExtendToLineStart => {
            editor
                .focused_window_mut()
                .selection_mut()
                .for_each_mut(|mut range| range.extend_to_line_start());
            editor.mode.set_count(None);
        }
        Action::MoveToLineNonBlankStart => {
            editor
                .focused_window_mut()
                .selection_mut()
                .for_each_mut(|mut range| range.move_to_line_non_blank_start());
            editor.mode.set_count(None);
        }
        Action::ExtendToLineNonBlankStart => {
            editor
                .focused_window_mut()
                .selection_mut()
                .for_each_mut(|mut range| range.extend_to_line_non_blank_start());
            editor.mode.set_count(None);
        }
        Action::MoveUntilLineEnd => {
            editor
                .focused_window_mut()
                .selection_mut()
                .for_each_mut(|mut range| range.move_until_line_end());
            editor.mode.set_count(None);
        }
        Action::ExtendUntilLineEnd => {
            editor
                .focused_window_mut()
                .selection_mut()
                .for_each_mut(|mut range| range.extend_until_line_end());
            editor.mode.set_count(None);
        }
    }
}
