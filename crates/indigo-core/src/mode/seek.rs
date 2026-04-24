#![allow(clippy::enum_glob_use)]

use crate::{
    editor::Editor,
    event::Event,
    key::KeyCode,
    keymap::{Keymap, KeymapResult, keymap},
    mode::{Mode, normal::enter_normal_mode},
};
use std::{num::NonZeroUsize, sync::LazyLock};

pub enum Action {
    Seek(u8),
}

pub static KEYMAP: LazyLock<Keymap<Vec<Action>>> = LazyLock::new(|| {
    use Action::*;
    keymap! { Vec<Action>;
        "<ret>" => vec![Seek(b'\n')],
        "<tab>" => vec![Seek(b'\t')],
        keys => {
            if let [key] = keys
                && key.modifiers.is_empty()
                && let KeyCode::Char(c) = key.code
            {
                KeymapResult::Fallback(vec![Seek(c)])
            } else {
                KeymapResult::Unmapped
            }
        }
    }
});

#[derive(Clone, Copy)]
pub enum SeekSelect {
    Move,
    Extend,
}

#[derive(Clone, Copy)]
pub enum SeekInclude {
    Until,
    Onto,
}

#[derive(Clone, Copy)]
pub enum SeekDirection {
    Prev,
    Next,
}

#[derive(Clone)]
pub struct State {
    pub count: Option<NonZeroUsize>,
    /// Move and create a new selection, or extend the current one?
    pub select: SeekSelect,
    /// Include seek target in selection, or stop just short?
    pub include: SeekInclude,
    /// Seek forward or backward?
    pub direction: SeekDirection,
}

pub fn handle_event_seek(editor: &mut Editor, _event: &Event) -> bool {
    match KEYMAP.get_keys(&editor.pending_keys) {
        KeymapResult::Mapped(actions) => {
            editor.pending_keys.clear();
            handle_actions_seek(editor, actions);
            enter_normal_mode(editor);
        }
        KeymapResult::Fallback(actions) => {
            editor.pending_keys.clear();
            handle_actions_seek(editor, &actions);
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

pub fn enter_seek_mode(
    editor: &mut Editor,
    select: SeekSelect,
    include: SeekInclude,
    direction: SeekDirection,
) {
    editor.mode = Mode::Seek(State {
        count: editor.mode.count(),
        select,
        include,
        direction,
    });
}

fn seek(editor: &mut Editor, byte: u8) {
    use SeekDirection::{Next, Prev};
    use SeekInclude::{Onto, Until};
    use SeekSelect::{Extend, Move};

    let (select, include, direction, count) = {
        let Mode::Seek(seek_mode) = &editor.mode else {
            panic!("Not in seek mode")
        };
        let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
        (
            seek_mode.select,
            seek_mode.include,
            seek_mode.direction,
            count,
        )
    };

    let mut window = editor.focused_window_mut();

    window
        .selection_mut()
        .for_each_mut(|mut range| match (select, include, direction) {
            (Move, Until, Prev) => range.move_until_prev_byte(byte, count),
            (Extend, Until, Prev) => range.extend_until_prev_byte(byte, count),
            (Move, Until, Next) => range.move_until_next_byte(byte, count),
            (Extend, Until, Next) => range.extend_until_next_byte(byte, count),
            (Move, Onto, Prev) => range.move_onto_prev_byte(byte, count),
            (Extend, Onto, Prev) => range.extend_onto_prev_byte(byte, count),
            (Move, Onto, Next) => range.move_onto_next_byte(byte, count),
            (Extend, Onto, Next) => range.extend_onto_next_byte(byte, count),
        });

    window.scroll_to_selection();
    editor.mode.set_count(None);
}

pub fn handle_actions_seek(editor: &mut Editor, actions: &[Action]) {
    for action in actions {
        handle_action_seek(editor, action);
    }
}

pub fn handle_action_seek(editor: &mut Editor, action: &Action) {
    match action {
        Action::Seek(byte) => seek(editor, *byte),
    }
}
