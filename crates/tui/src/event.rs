use crate::{
    areas::{Areas, position_to_char_index},
    key::{key_i2t, key_t2i},
    terminal::Terminal,
};
use anyhow::anyhow;
use indigo_core::{actions, prelude::*};
use ratatui::{
    crossterm::event::{KeyCode, KeyModifiers, MouseButton, MouseEventKind},
    layout::Position,
};

pub type IndigoEvent = indigo_core::event::Event;

pub type TerminalEvent = ratatui::crossterm::event::Event;

#[must_use]
pub fn should_skip_event(event: &TerminalEvent) -> bool {
    if let TerminalEvent::Mouse(mouse_event) = event {
        matches!(
            mouse_event.kind,
            MouseEventKind::Moved
                | MouseEventKind::Drag(_)
                | MouseEventKind::ScrollLeft
                | MouseEventKind::ScrollRight
        )
    } else {
        false
    }
}

pub fn handle_event(
    editor: &mut Editor,
    terminal: &mut Terminal,
    areas: Areas,
    event: &TerminalEvent,
) -> anyhow::Result<()> {
    if let Ok(event) = event_t2i(event) {
        if indigo_core::event::handle_event(editor, &event) {
            return Ok(());
        }
    }
    match editor.mode {
        Mode::Normal(_) => handle_event_normal(editor, terminal, areas, event),
        Mode::Insert(_) => handle_event_insert(editor, terminal, areas, event),
        Mode::Command(_) => handle_event_command(editor, terminal, areas, event),
    }
}

fn handle_event_normal(
    editor: &mut Editor,
    terminal: &mut Terminal,
    areas: Areas,
    event: &TerminalEvent,
) -> anyhow::Result<()> {
    let Mode::Normal(ref mut _normal_mode) = editor.mode else {
        unreachable!()
    };

    #[expect(clippy::single_match)]
    match event {
        TerminalEvent::Key(key_event) => match (key_event.modifiers, key_event.code) {
            (KeyModifiers::CONTROL, KeyCode::Char('l')) => terminal.clear()?,
            _ => {}
        },
        TerminalEvent::Mouse(mouse_event) => match (mouse_event.modifiers, mouse_event.kind) {
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
    terminal: &mut Terminal,
    _areas: Areas,
    event: &TerminalEvent,
) -> anyhow::Result<()> {
    let Mode::Insert(ref _insert_mode) = editor.mode else {
        unreachable!()
    };

    #[expect(clippy::single_match)]
    match event {
        TerminalEvent::Key(key_event) => match (key_event.modifiers, key_event.code) {
            (KeyModifiers::CONTROL, KeyCode::Char('l')) => terminal.clear()?,
            _ => {}
        },
        TerminalEvent::Mouse(mouse_event) => match (mouse_event.modifiers, mouse_event.kind) {
            (KeyModifiers::NONE, MouseEventKind::ScrollUp) => actions::scroll_up(editor),
            (KeyModifiers::NONE, MouseEventKind::ScrollDown) => actions::scroll_down(editor),
            _ => {}
        },
        TerminalEvent::Paste(string) => actions::insert(editor, string),
        _ => {}
    }

    Ok(())
}

#[expect(clippy::unnecessary_wraps)]
fn handle_event_command(
    editor: &mut Editor,
    _terminal: &mut Terminal,
    _areas: Areas,
    event: &TerminalEvent,
) -> anyhow::Result<()> {
    let Mode::Command(ref mut _command_mode) = editor.mode else {
        unreachable!()
    };

    #[expect(clippy::single_match)]
    match event {
        TerminalEvent::Paste(string) => actions::insert(editor, string),
        _ => {}
    }

    Ok(())
}

pub fn event_t2i(event: &TerminalEvent) -> anyhow::Result<IndigoEvent> {
    match event {
        TerminalEvent::Key(key_event) => Ok(IndigoEvent::Key(key_t2i(*key_event)?)),
        _ => Err(anyhow!(
            "Unsupported crossterm->indigo event conversion: {event:?}"
        )),
    }
}

#[must_use]
pub fn event_i2t(event: &IndigoEvent) -> TerminalEvent {
    match event {
        IndigoEvent::Key(key_event) => TerminalEvent::Key(key_i2t(*key_event)),
    }
}
