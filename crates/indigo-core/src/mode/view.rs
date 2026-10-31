#![allow(clippy::enum_glob_use)]

use crate::{
    client::ClientMut,
    mode::{Mode, normal},
};
use indigo_key::{Keymap, KeymapResult, keymap};
use std::{num::NonZeroUsize, sync::Arc};

#[cfg(feature = "arbitrary")]
use arbitrary::Arbitrary;

#[derive(Clone, Default)]
pub struct State {
    drag: Option<Drag>,
}

/// Where a drag started, so the grabbed line stays under the mouse.
#[derive(Clone, Copy)]
struct Drag {
    row: u16,
    vertical_scroll: usize,
}

#[cfg_attr(feature = "arbitrary", derive(Arbitrary))]
#[derive(Debug)]
pub enum Action {
    EnterNormalMode,
    AppendCountDigit(u8),
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

        "0" => vec![AppendCountDigit(0)],
        "1" => vec![AppendCountDigit(1)],
        "2" => vec![AppendCountDigit(2)],
        "3" => vec![AppendCountDigit(3)],
        "4" => vec![AppendCountDigit(4)],
        "5" => vec![AppendCountDigit(5)],
        "6" => vec![AppendCountDigit(6)],
        "7" => vec![AppendCountDigit(7)],
        "8" => vec![AppendCountDigit(8)],
        "9" => vec![AppendCountDigit(9)],

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
        Action::AppendCountDigit(digit) => normal::append_count_digit(client, *digit),
        Action::ScrollLineUp => scroll_line_up(client),
        Action::ScrollLineDown => scroll_line_down(client),
        Action::ScrollHalfPageUp => scroll_half_page_up(client),
        Action::ScrollHalfPageDown => scroll_half_page_down(client),
        Action::ScrollFullPageUp => scroll_full_page_up(client),
        Action::ScrollFullPageDown => scroll_full_page_down(client),
        Action::ScrollCenter => scroll_center(client),
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
            client.count = None;
            false
        }
        KeymapResult::Pending => true,
    }
}

pub fn enter(client: &mut ClientMut) {
    client.mode = Mode::View(State::default());
}

pub fn drag_start(client: &mut ClientMut, row: u16) {
    let vertical_scroll = client.focused_window().vertical_scroll();
    let Mode::View(view_mode) = &mut client.mode else {
        panic!("Not in view mode")
    };
    view_mode.drag = Some(Drag {
        row,
        vertical_scroll,
    });
}

pub fn drag_to(client: &mut ClientMut, row: u16) {
    let Mode::View(view_mode) = &client.mode else {
        panic!("Not in view mode")
    };
    let Some(drag) = view_mode.drag else {
        return;
    };
    // Dragging down pulls the text down, revealing earlier lines.
    let line = if row < drag.row {
        drag.vertical_scroll + usize::from(drag.row - row)
    } else {
        drag.vertical_scroll
            .saturating_sub(usize::from(row - drag.row))
    };
    client.focused_window_mut().scroll_to_line(line);
}

pub fn drag_end(client: &mut ClientMut) {
    let Mode::View(view_mode) = &mut client.mode else {
        panic!("Not in view mode")
    };
    view_mode.drag = None;
}

fn scroll_line_up(client: &mut ClientMut) {
    let count = client.count.take().unwrap_or(NonZeroUsize::MIN).get();
    client.focused_window_mut().scroll_lines_up(count);
}

fn scroll_line_down(client: &mut ClientMut) {
    let count = client.count.take().unwrap_or(NonZeroUsize::MIN).get();
    client.focused_window_mut().scroll_lines_down(count);
}

fn scroll_half_page_up(client: &mut ClientMut) {
    client.focused_window_mut().scroll_half_page_up();
    client.count = None;
}

fn scroll_half_page_down(client: &mut ClientMut) {
    client.focused_window_mut().scroll_half_page_down();
    client.count = None;
}

fn scroll_full_page_up(client: &mut ClientMut) {
    client.focused_window_mut().scroll_full_page_up();
    client.count = None;
}

fn scroll_full_page_down(client: &mut ClientMut) {
    client.focused_window_mut().scroll_full_page_down();
    client.count = None;
}

fn scroll_center(client: &mut ClientMut) {
    client.focused_window_mut().scroll_center_selection();
    client.count = None;
}
