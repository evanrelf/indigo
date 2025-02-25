use crate::{
    areas::{Areas, position_to_char_index},
    key::{key_c2i, key_i2c},
    terminal::TerminalGuard,
};
use anyhow::anyhow;
use crossterm::event::{Event, KeyCode, KeyModifiers, MouseButton, MouseEventKind};
use indigo_core::{actions, prelude::*};
use ratatui::layout::Position;

pub fn handle_event(
    editor: &mut Editor,
    terminal: &mut TerminalGuard,
    areas: Areas,
    event: &Event,
) -> anyhow::Result<()> {
    if let Ok(event) = event_c2i(event) {
        if indigo_core::event::handle_event(editor, &event) {
            return Ok(());
        }
    }
    match editor.mode {
        Mode::Normal(_) => handle_event_normal(editor, terminal, areas, event),
        Mode::Insert => handle_event_insert(editor, terminal, areas, event),
        Mode::Command(_) => handle_event_command(editor, terminal, areas, event),
    }
}

fn handle_event_normal(
    editor: &mut Editor,
    terminal: &mut TerminalGuard,
    areas: Areas,
    event: &Event,
) -> anyhow::Result<()> {
    let Mode::Normal(ref mut _normal_mode) = editor.mode else {
        unreachable!()
    };

    #[expect(clippy::single_match)]
    match event {
        Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
            (KeyModifiers::CONTROL, KeyCode::Char('l')) => terminal.clear()?,
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
    let Mode::Insert = editor.mode else {
        unreachable!()
    };

    #[expect(clippy::single_match)]
    match event {
        Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
            (KeyModifiers::CONTROL, KeyCode::Char('l')) => terminal.clear()?,
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

#[expect(clippy::unnecessary_wraps)]
fn handle_event_command(
    editor: &mut Editor,
    _terminal: &mut TerminalGuard,
    _areas: Areas,
    _event: &Event,
) -> anyhow::Result<()> {
    let Mode::Command(ref mut _command_mode) = editor.mode else {
        unreachable!()
    };

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
