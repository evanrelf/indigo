#![allow(clippy::wildcard_imports)]

use indigo_core::{
    action::*,
    editor::Editor,
    event::Event,
    key::{KeyCode, is},
    mode::Mode,
};
use std::num::NonZeroUsize;

pub fn handle_event(editor: &mut Editor, event: &Event) -> bool {
    match editor.mode {
        Mode::Normal(_) => handle_event_normal(editor, event),
        Mode::Insert(_) => handle_event_insert(editor, event),
        Mode::Command(_) => handle_event_command(editor, event),
    }
}

pub fn handle_event_normal(editor: &mut Editor, event: &Event) -> bool {
    let mut handled = true;

    let count = |c: char| {
        let digit = usize::from(u8::try_from(c).unwrap() - b'0');
        let current = editor.mode.count().map_or(0, |count| usize::from(count));
        NonZeroUsize::new(current.saturating_mul(10).saturating_add(digit))
    };

    match event {
        Event::KeyInput(key) => match (key.modifiers, key.code) {
            (m, KeyCode::Char(c @ '0'..='9')) if m.is_empty() => set_count(editor, count(c)),
            _ if is(key, "<esc>") => enter_normal_mode(editor),
            _ if is(key, ":") => enter_command_mode(editor),
            _ if is(key, "i") => enter_insert_mode(editor),
            // TODO: Add `a` for entering insert mode with the cursor moved to the right.
            _ if is(key, "h") => move_left(editor),
            _ if is(key, "l") => move_right(editor),
            _ if is(key, "H") => extend_left(editor),
            _ if is(key, "L") => extend_right(editor),
            // TODO: Capture key following these, use as the needle. Could add a "jump" mode with
            // settings to cover all of these cases.
            _ if is(key, "<a-t>") => move_until_prev_byte(editor, b'e'),
            _ if is(key, "<a-T>") => extend_until_prev_byte(editor, b'e'),
            _ if is(key, "t") => move_until_next_byte(editor, b'e'),
            _ if is(key, "T") => extend_until_next_byte(editor, b'e'),
            _ if is(key, "<a-f>") => move_to_prev_byte(editor, b'e'),
            _ if is(key, "<a-F>") => extend_to_prev_byte(editor, b'e'),
            _ if is(key, "f") => move_to_next_byte(editor, b'e'),
            _ if is(key, "F") => extend_to_next_byte(editor, b'e'),
            _ if is(key, ";") => reduce(editor),
            _ if is(key, "<a-;>") => flip(editor),
            _ if is(key, "<a-s-;>") => flip_forward(editor),
            _ if is(key, "d") => delete(editor),
            _ if is(key, "u") => undo(editor),
            _ if is(key, "U") => redo(editor),
            _ if is(key, "<c-u>") => scroll_half_page_up(editor),
            _ if is(key, "<c-d>") => scroll_half_page_down(editor),
            _ if is(key, "<c-b>") => scroll_full_page_up(editor),
            _ if is(key, "<c-f>") => scroll_full_page_down(editor),
            _ if is(key, "<c-c>") => exit(editor, 1),
            _ => handled = false,
        },
    }

    handled
}

pub fn handle_event_insert(editor: &mut Editor, event: &Event) -> bool {
    let Mode::Insert(_insert_mode) = &editor.mode else {
        unreachable!()
    };

    let mut handled = true;

    match event {
        Event::KeyInput(key) => match (key.modifiers, key.code) {
            _ if is(key, "<esc>") => enter_normal_mode(editor),
            _ if is(key, "<bs>") => delete_before(editor),
            _ if is(key, "<del>") => delete_after(editor),
            (m, KeyCode::Char(c)) if m.is_empty() => insert_char(editor, c),
            _ if is(key, "<ret>") => insert_char(editor, '\n'),
            _ if is(key, "<tab>") => insert_char(editor, '\t'),
            _ if is(key, "<c-u>") => scroll_half_page_up(editor),
            _ if is(key, "<c-d>") => scroll_half_page_down(editor),
            _ if is(key, "<c-b>") => scroll_full_page_up(editor),
            _ if is(key, "<c-f>") => scroll_full_page_down(editor),
            _ if is(key, "<c-c>") => exit(editor, 1),
            _ => handled = false,
        },
    }

    handled
}

pub fn handle_event_command(editor: &mut Editor, event: &Event) -> bool {
    let Mode::Command(_command_mode) = &editor.mode else {
        unreachable!()
    };

    let mut handled = true;

    match event {
        Event::KeyInput(key) => match (key.modifiers, key.code) {
            _ if is(key, "<esc>") => enter_normal_mode(editor),
            _ if is(key, "<bs>") => delete_before(editor),
            _ if is(key, "<ret>") => run_command(editor),
            (m, KeyCode::Char(c)) if m.is_empty() => insert_char(editor, c),
            _ if is(key, "<c-c>") => exit(editor, 1),
            _ => handled = false,
        },
    }

    handled
}
