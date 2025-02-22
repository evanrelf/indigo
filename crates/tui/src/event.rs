use crate::{
    areas::{position_to_char_index, Areas},
    key::{key_c2i, key_i2c},
    terminal::TerminalGuard,
};
use anyhow::anyhow;
use crossterm::event::{Event, KeyCode, KeyModifiers, MouseButton, MouseEventKind};
use indigo_core::{actions, prelude::*};
use ratatui::layout::Position;
use std::num::NonZeroUsize;

pub fn handle_event(
    editor: &mut Editor,
    terminal: &mut TerminalGuard,
    areas: Areas,
    event: &Event,
) -> anyhow::Result<()> {
    match editor.mode {
        Mode::Normal(_) => handle_event_normal(editor, terminal, areas, event),
        Mode::Insert => handle_event_insert(editor, terminal, areas, event),
    }
}

fn handle_event_normal(
    editor: &mut Editor,
    terminal: &mut TerminalGuard,
    areas: Areas,
    event: &Event,
) -> anyhow::Result<()> {
    let Mode::Normal(ref mut normal_mode) = editor.mode else {
        unreachable!()
    };

    match event {
        Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
            (KeyModifiers::NONE, KeyCode::Char(c @ ('0'..='9'))) => {
                let n = usize::from(u8::try_from(c).unwrap() - b'0');
                normal_mode.count = normal_mode
                    .count
                    .saturating_mul(NonZeroUsize::new(10).unwrap())
                    .saturating_add(n);
            }
            (KeyModifiers::NONE, KeyCode::Esc) => actions::enter_normal_mode(editor),
            (KeyModifiers::NONE, KeyCode::Char('i')) => actions::enter_insert_mode(editor),
            // TODO: Add `a` for entering insert mode with the cursor moved to the right.
            (KeyModifiers::NONE, KeyCode::Char('h')) => actions::move_left(editor),
            (KeyModifiers::NONE, KeyCode::Char('l')) => actions::move_right(editor),
            (KeyModifiers::SHIFT, KeyCode::Char('h' | 'H')) => actions::extend_left(editor),
            (KeyModifiers::SHIFT, KeyCode::Char('l' | 'L')) => actions::extend_right(editor),
            (KeyModifiers::NONE, KeyCode::Char(';')) => actions::reduce(editor),
            (KeyModifiers::ALT, KeyCode::Char(';')) => actions::flip(editor),
            (ms, KeyCode::Char(';')) if ms == KeyModifiers::ALT | KeyModifiers::SHIFT => {
                actions::flip_forward(editor);
            }
            (KeyModifiers::NONE, KeyCode::Char('d')) => actions::delete(editor),
            (KeyModifiers::CONTROL, KeyCode::Char('u')) => actions::scroll_half_page_up(editor),
            (KeyModifiers::CONTROL, KeyCode::Char('d')) => actions::scroll_half_page_down(editor),
            (KeyModifiers::CONTROL, KeyCode::Char('b')) => actions::scroll_full_page_up(editor),
            (KeyModifiers::CONTROL, KeyCode::Char('f')) => actions::scroll_full_page_down(editor),
            (KeyModifiers::CONTROL, KeyCode::Char('l')) => terminal.clear()?,
            (KeyModifiers::CONTROL, KeyCode::Char('c')) => editor.quit = true,
            _ => {}
        },
        Event::Mouse(mouse_event) => match (mouse_event.modifiers, mouse_event.kind) {
            (KeyModifiers::NONE, MouseEventKind::ScrollUp) => actions::scroll_up(editor),
            (KeyModifiers::NONE, MouseEventKind::ScrollDown) => actions::scroll_down(editor),
            // TODO: Kakoune allows creating new selection ranges by control clicking. Would be
            // awesome if Indigo could do the same, but also support control dragging to create
            // vertical lines of selection ranges, akin to Vim's visual block mode. Could snap to
            // the same column? Might be weird in the presence of wide characters.
            (KeyModifiers::NONE, MouseEventKind::Down(MouseButton::Left)) => {
                let position = Position {
                    x: mouse_event.column,
                    y: mouse_event.row,
                };
                if let Some(Err(index) | Ok(index)) = position_to_char_index(
                    position,
                    editor.rope(),
                    editor.vertical_scroll(),
                    areas.text,
                ) {
                    actions::move_to(editor, index);
                }
            }
            (KeyModifiers::NONE, MouseEventKind::Down(MouseButton::Right)) => {
                let position = Position {
                    x: mouse_event.column,
                    y: mouse_event.row,
                };
                if let Some(Err(index) | Ok(index)) = position_to_char_index(
                    position,
                    editor.rope(),
                    editor.vertical_scroll(),
                    areas.text,
                ) {
                    actions::extend_to(editor, index);
                }
            }
            _ => {}
        },
        _ => {}
    }

    Ok(())
}

fn handle_event_insert(
    editor: &mut Editor,
    terminal: &mut TerminalGuard,
    _areas: Areas,
    event: &Event,
) -> anyhow::Result<()> {
    match event {
        Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
            (KeyModifiers::NONE, KeyCode::Esc) => actions::enter_normal_mode(editor),
            (KeyModifiers::NONE, KeyCode::Backspace) => actions::delete_before(editor),
            (KeyModifiers::NONE, KeyCode::Delete) => actions::delete_after(editor),
            (KeyModifiers::NONE, KeyCode::Char(c)) => actions::insert_char(editor, c),
            (KeyModifiers::NONE, KeyCode::Enter) => actions::insert_char(editor, '\n'),
            (KeyModifiers::NONE, KeyCode::Tab) => actions::insert_char(editor, '\t'),
            (KeyModifiers::CONTROL, KeyCode::Char('u')) => actions::scroll_half_page_up(editor),
            (KeyModifiers::CONTROL, KeyCode::Char('d')) => actions::scroll_half_page_down(editor),
            (KeyModifiers::CONTROL, KeyCode::Char('b')) => actions::scroll_full_page_up(editor),
            (KeyModifiers::CONTROL, KeyCode::Char('f')) => actions::scroll_full_page_down(editor),
            (KeyModifiers::CONTROL, KeyCode::Char('l')) => terminal.clear()?,
            (KeyModifiers::CONTROL, KeyCode::Char('c')) => editor.quit = true,
            _ => {}
        },
        Event::Mouse(mouse_event) => match (mouse_event.modifiers, mouse_event.kind) {
            (KeyModifiers::NONE, MouseEventKind::ScrollUp) => actions::scroll_up(editor),
            (KeyModifiers::NONE, MouseEventKind::ScrollDown) => actions::scroll_down(editor),
            _ => {}
        },
        Event::Paste(string) => actions::insert(editor, string),
        _ => {}
    }

    Ok(())
}

mod c {
    pub use crossterm::event::Event;
}

mod i {
    pub use indigo_core::event::Event;
}

pub fn event_c2i(event: &c::Event) -> anyhow::Result<i::Event> {
    match event {
        c::Event::Key(key_event) => Ok(i::Event::Key(key_c2i(*key_event)?)),
        _ => Err(anyhow!(
            "Unsupported crossterm->indigo event conversion: {event:?}"
        )),
    }
}

pub fn event_i2c(event: &i::Event) -> c::Event {
    match event {
        i::Event::Key(key_event) => c::Event::Key(key_i2c(*key_event)),
    }
}
