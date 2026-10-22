#![allow(clippy::enum_glob_use)]

use crate::{
    key::{KeyCode, KeyModifiers},
    keymap::{Keymap, KeymapResult, keymap},
    mode::{Mode, normal},
    session::Session,
};
use std::{num::NonZeroUsize, sync::LazyLock};

#[cfg(feature = "arbitrary")]
use arbitrary::Arbitrary;

#[cfg_attr(feature = "arbitrary", derive(Arbitrary))]
#[derive(Clone, Copy, Debug)]
pub enum SeekSelect {
    Move,
    Extend,
}

#[cfg_attr(feature = "arbitrary", derive(Arbitrary))]
#[derive(Clone, Copy, Debug)]
pub enum SeekInclude {
    Until,
    Onto,
}

#[cfg_attr(feature = "arbitrary", derive(Arbitrary))]
#[derive(Clone, Copy, Debug)]
pub enum SeekDirection {
    Prev,
    Next,
}

#[derive(Clone)]
pub struct State {
    /// Move and create a new selection, or extend the current one?
    pub select: SeekSelect,
    /// Include seek target in selection, or stop just short?
    pub include: SeekInclude,
    /// Seek forward or backward?
    pub direction: SeekDirection,
}

#[cfg_attr(feature = "arbitrary", derive(Arbitrary))]
#[derive(Debug)]
pub enum Action {
    Seek(u8),
}

pub static KEYMAP: LazyLock<Keymap<Vec<Action>>> = LazyLock::new(|| {
    use Action::*;
    keymap! { Vec<Action>;
        "<esc>" => vec![], /// Cancel
        "<ret>" => vec![Seek(b'\n')], /// Seek to newline
        "<tab>" => vec![Seek(b'\t')], /// Seek to tab
        keys => {
            if let [key] = keys
                && let KeyCode::Char(c) = key.code
                && key.modifiers.difference(KeyModifiers::SHIFT).is_empty()
            {
                KeymapResult::Fallback(vec![Seek(c)])
            } else {
                KeymapResult::Unmapped
            }
        } /// Seek to character
    }
});

pub fn handle_actions(session: &mut Session, actions: &[Action]) {
    for action in actions {
        handle_action(session, action);
    }
}

pub fn handle_action(session: &mut Session, action: &Action) {
    match action {
        Action::Seek(byte) => seek(session, *byte),
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

pub fn enter(
    session: &mut Session,
    select: SeekSelect,
    include: SeekInclude,
    direction: SeekDirection,
) {
    session.client.mode = Mode::Seek(State {
        select,
        include,
        direction,
    });
}

fn seek(session: &mut Session, byte: u8) {
    use SeekDirection::{Next, Prev};
    use SeekInclude::{Onto, Until};
    use SeekSelect::{Extend, Move};

    let (select, include, direction, count) = {
        let Mode::Seek(seek_mode) = &session.client.mode else {
            panic!("Not in seek mode")
        };
        let count = session
            .client
            .count
            .take()
            .unwrap_or(NonZeroUsize::MIN)
            .get();
        (
            seek_mode.select,
            seek_mode.include,
            seek_mode.direction,
            count,
        )
    };

    let mut window = session.focused_window_mut();

    window
        .selection_move()
        .for_each_move(|mut range| match (select, include, direction) {
            (Move, Until, Prev) => range.move_until_prev_byte(byte, count),
            (Extend, Until, Prev) => range.extend_until_prev_byte(byte, count),
            (Move, Until, Next) => range.move_until_next_byte(byte, count),
            (Extend, Until, Next) => range.extend_until_next_byte(byte, count),
            (Move, Onto, Prev) => range.move_onto_prev_byte(byte, count),
            (Extend, Onto, Prev) => range.extend_onto_prev_byte(byte, count),
            (Move, Onto, Next) => range.move_onto_next_byte(byte, count),
            (Extend, Onto, Next) => range.extend_onto_next_byte(byte, count),
        });
    window.selection_move().merge_overlapping();

    window.scroll_to_selection();
}
