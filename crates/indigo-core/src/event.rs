#![allow(clippy::wildcard_imports)]

use crate::{
    action::*,
    editor::Editor,
    key::{Key, KeyCode, is},
    mode::Mode,
};
use std::num::NonZeroUsize;

#[cfg(any(feature = "arbitrary", test))]
use arbitrary::Arbitrary;

#[cfg_attr(any(feature = "arbitrary", test), derive(Arbitrary))]
#[derive(Debug)]
pub enum Event {
    KeyInput(Key),
}

impl From<Key> for Event {
    fn from(key: Key) -> Self {
        Self::KeyInput(key)
    }
}

pub fn handle_event(editor: &mut Editor, mut event: Event) -> anyhow::Result<bool> {
    #[expect(irrefutable_let_patterns)]
    if let Event::KeyInput(key) = &mut event {
        key.normalize();
    }

    let handled = match editor.mode {
        Mode::Normal(_) => handle_event_normal(editor, &event),
        Mode::Seek(_) => handle_event_seek(editor, &event),
        Mode::Goto(_) => handle_event_goto(editor, &event),
        Mode::Insert(_) => handle_event_insert(editor, &event),
        Mode::Command(_) => handle_event_command(editor, &event)?,
    };

    Ok(handled)
}

pub fn handle_event_normal(editor: &mut Editor, event: &Event) -> bool {
    use crate::mode::{
        SeekDirection::{Next, Prev},
        SeekInclude::{Onto, Until},
        SeekSelect::{Extend, Move},
    };

    let mut handled = true;

    let count = |c: char| {
        let digit = usize::from(
            u8::try_from(c).expect("Pattern match below guarantees an ASCII digit") - b'0',
        );
        let current = editor.mode.count().map_or(0, |count| usize::from(count));
        NonZeroUsize::new(current.saturating_mul(10).saturating_add(digit))
    };

    match event {
        Event::KeyInput(key) => match (key.modifiers, key.code) {
            (m, KeyCode::Char(c @ '0'..='9')) if m.is_empty() => set_count(editor, count(c)),
            _ if is(key, "<esc>") => enter_normal_mode(editor),
            _ if is(key, ":") => enter_command_mode(editor),
            _ if is(key, "i") => enter_insert_mode(editor),
            _ if is(key, "a") => {
                move_right(editor);
                enter_insert_mode(editor);
            }
            _ if is(key, "h") => move_left(editor),
            _ if is(key, "l") => move_right(editor),
            _ if is(key, "k") => move_up(editor),
            _ if is(key, "j") => move_down(editor),
            _ if is(key, "H") => extend_left(editor),
            _ if is(key, "L") => extend_right(editor),
            _ if is(key, "K") => extend_up(editor),
            _ if is(key, "J") => extend_down(editor),
            _ if is(key, "<a-t>") => enter_seek_mode(editor, Move, Until, Prev),
            _ if is(key, "<a-T>") => enter_seek_mode(editor, Extend, Until, Prev),
            _ if is(key, "t") => enter_seek_mode(editor, Move, Until, Next),
            _ if is(key, "T") => enter_seek_mode(editor, Extend, Until, Next),
            _ if is(key, "<a-f>") => enter_seek_mode(editor, Move, Onto, Prev),
            _ if is(key, "<a-F>") => enter_seek_mode(editor, Extend, Onto, Prev),
            _ if is(key, "f") => enter_seek_mode(editor, Move, Onto, Next),
            _ if is(key, "F") => enter_seek_mode(editor, Extend, Onto, Next),
            _ if is(key, "g") => enter_goto_mode(editor),
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

pub fn handle_event_seek(editor: &mut Editor, event: &Event) -> bool {
    let Mode::Seek(_seek_mode) = &editor.mode else {
        panic!("Not in seek mode")
    };

    let Event::KeyInput(key) = event;

    let mut key = *key;
    key.normalize();

    // TODO: Interpret `Return` as `\n` and `Tab` as `\t`.
    if let KeyCode::Char(char) = key.code
        && let Ok(byte) = u8::try_from(char)
        && key.modifiers.is_empty()
    {
        seek(editor, byte);
    }

    enter_normal_mode(editor);

    true
}

pub fn handle_event_goto(editor: &mut Editor, event: &Event) -> bool {
    let Mode::Goto(_goto_mode) = &editor.mode else {
        panic!("Not in goto mode")
    };

    match event {
        Event::KeyInput(key) => match (key.modifiers, key.code) {
            _ => {}
        },
    }

    enter_normal_mode(editor);

    true
}

pub fn handle_event_insert(editor: &mut Editor, event: &Event) -> bool {
    let Mode::Insert(_insert_mode) = &editor.mode else {
        panic!("Not in insert mode")
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

pub fn handle_event_command(editor: &mut Editor, event: &Event) -> anyhow::Result<bool> {
    let Mode::Command(_command_mode) = &editor.mode else {
        panic!("Not in command mode")
    };

    let mut handled = true;

    match event {
        Event::KeyInput(key) => match (key.modifiers, key.code) {
            _ if is(key, "<esc>") => enter_normal_mode(editor),
            _ if is(key, "<bs>") => delete_before(editor),
            _ if is(key, "<ret>") => run_command(editor)?,
            (m, KeyCode::Char(c)) if m.is_empty() => insert_char(editor, c),
            _ if is(key, "<c-c>") => exit(editor, 1),
            _ => handled = false,
        },
    }

    Ok(handled)
}
