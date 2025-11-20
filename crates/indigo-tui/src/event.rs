#![allow(clippy::wildcard_imports)]

use crate::{
    areas::{Areas, position_to_char_offset},
    key::{key_event_i2t, key_event_t2i},
    terminal::TerminalGuard,
};
use anyhow::anyhow;
use indigo_core::{
    action::*,
    mode::{command, insert},
    prelude::*,
};
use ratatui::crossterm::{
    self,
    event::{KeyCode, KeyModifiers, MouseButton, MouseEventKind},
};
use ratatui::layout::Position;

pub type IndigoEvent = indigo_core::event::Event;

pub type TerminalEvent = crossterm::event::Event;

#[must_use]
pub fn should_skip_event(event: &TerminalEvent) -> bool {
    match event {
        TerminalEvent::Key(key_event) => key_event.kind.is_release(),
        TerminalEvent::Mouse(mouse_event) => matches!(
            mouse_event.kind,
            MouseEventKind::Moved | MouseEventKind::ScrollLeft | MouseEventKind::ScrollRight
        ),
        _ => false,
    }
}

pub fn handle_event(
    editor: &mut Editor,
    terminal: &mut TerminalGuard,
    areas: Areas,
    event: TerminalEvent,
) -> anyhow::Result<()> {
    let dismiss_message = match event {
        TerminalEvent::Mouse(mouse) => !matches!(
            mouse.kind,
            MouseEventKind::Moved
                | MouseEventKind::ScrollUp
                | MouseEventKind::ScrollDown
                | MouseEventKind::ScrollLeft
                | MouseEventKind::ScrollRight
        ),
        _ => true,
    };

    if dismiss_message {
        editor.message = None;
    }

    if let Ok(event) = event_t2i(&event) {
        let handled = indigo_core::event::handle_event(editor, event)?;
        if handled {
            return Ok(());
        }
    }

    #[expect(clippy::match_same_arms)]
    let _handled = match editor.mode {
        Mode::Normal(_) => handle_event_normal(editor, terminal, areas, event)?,
        // TODO: Exit seek and goto modes on mouse events.
        Mode::Seek(_) => false,
        Mode::Goto(_) => false,
        Mode::Insert(_) => handle_event_insert(editor, terminal, areas, event)?,
        Mode::Command(_) => handle_event_command(editor, terminal, areas, event)?,
    };

    Ok(())
}

#[expect(clippy::needless_pass_by_value)]
fn handle_event_normal(
    editor: &mut Editor,
    terminal: &mut TerminalGuard,
    areas: Areas,
    event: TerminalEvent,
) -> anyhow::Result<bool> {
    let mut handled = true;

    match event {
        TerminalEvent::Key(key_event) => match (key_event.modifiers, key_event.code) {
            (KeyModifiers::CONTROL, KeyCode::Char('l')) => terminal.clear()?,
            _ => handled = false,
        },
        TerminalEvent::Mouse(mouse_event) => match (mouse_event.modifiers, mouse_event.kind) {
            (KeyModifiers::NONE, MouseEventKind::ScrollUp) => scroll_up(editor),
            (KeyModifiers::NONE, MouseEventKind::ScrollDown) => scroll_down(editor),
            // TODO: Kakoune allows creating new selection ranges by control clicking. Would be
            // awesome if Indigo could do the same, but also support control dragging to create
            // vertical lines of selection ranges, akin to Vim's visual block mode. Could snap to
            // the same column? Might be weird in the presence of wide characters.
            (KeyModifiers::NONE, MouseEventKind::Down(MouseButton::Left)) => {
                let position = Position {
                    x: mouse_event.column,
                    y: mouse_event.row,
                };
                if let Some(Err(char_offset) | Ok(char_offset)) = position_to_char_offset(
                    position,
                    editor.buffer.rope(),
                    editor.window().vertical_scroll(),
                    areas.text,
                ) {
                    editor.buffer.range_mut().move_to(char_offset);
                } else {
                    handled = false;
                }
            }
            (
                KeyModifiers::NONE,
                MouseEventKind::Down(MouseButton::Right)
                | MouseEventKind::Drag(MouseButton::Left | MouseButton::Right),
            ) => {
                let position = Position {
                    x: mouse_event.column,
                    y: mouse_event.row,
                };
                if let Some(Err(char_offset) | Ok(char_offset)) = position_to_char_offset(
                    position,
                    editor.buffer.rope(),
                    editor.window().vertical_scroll(),
                    areas.text,
                ) {
                    editor.buffer.range_mut().extend_to(char_offset);
                } else {
                    handled = false;
                }
            }
            _ => handled = false,
        },
        _ => handled = false,
    }

    Ok(handled)
}

fn handle_event_insert(
    editor: &mut Editor,
    terminal: &mut TerminalGuard,
    _areas: Areas,
    event: TerminalEvent,
) -> anyhow::Result<bool> {
    let mut handled = true;

    match event {
        TerminalEvent::Key(key_event) => match (key_event.modifiers, key_event.code) {
            (KeyModifiers::CONTROL, KeyCode::Char('l')) => terminal.clear()?,
            _ => handled = false,
        },
        // TODO: Add mouse events to core, so it can handle this internally.
        TerminalEvent::Mouse(mouse_event) => match (mouse_event.modifiers, mouse_event.kind) {
            (KeyModifiers::NONE, MouseEventKind::ScrollUp) => scroll_up(editor),
            (KeyModifiers::NONE, MouseEventKind::ScrollDown) => scroll_down(editor),
            _ => handled = false,
        },
        TerminalEvent::Paste(text) => insert::paste(editor, &text),
        _ => handled = false,
    }

    Ok(handled)
}

#[expect(clippy::unnecessary_wraps)]
fn handle_event_command(
    editor: &mut Editor,
    _terminal: &mut TerminalGuard,
    _areas: Areas,
    event: TerminalEvent,
) -> anyhow::Result<bool> {
    let mut handled = true;

    match event {
        TerminalEvent::Paste(text) => command::paste(editor, &text),
        _ => handled = false,
    }

    Ok(handled)
}

pub fn event_t2i(event: &TerminalEvent) -> anyhow::Result<IndigoEvent> {
    match event {
        TerminalEvent::Key(key_event) => Ok(IndigoEvent::Key(key_event_t2i(key_event)?)),
        _ => Err(anyhow!(
            "Unsupported crossterm->indigo event conversion: {event:?}"
        )),
    }
}

pub fn event_i2t(event: &IndigoEvent) -> anyhow::Result<TerminalEvent> {
    match event {
        IndigoEvent::Key(key_event) => Ok(TerminalEvent::Key(key_event_i2t(key_event))),
    }
}
