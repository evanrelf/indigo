use crate::{
    areas::{Areas, position_to_char_index},
    key::{key_i2t, key_t2i},
    terminal::Terminal,
};
use anyhow::anyhow;
use indigo_core::prelude::*;
use ratatui::{
    crossterm::event::{KeyCode, KeyModifiers, MouseButton, MouseEventKind},
    layout::Position,
};
use std::rc::Rc;

pub type IndigoEvent = indigo_event::event::Event;

pub type IndigoAction = indigo_event::action::Action;

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

// TODO: Give the TUI its own actions? Could take a shared reference to the editor, and would lend
// itself to simulation.
pub fn handle_event(
    editor: &mut Editor,
    terminal: &mut Terminal,
    areas: Areas,
    event: TerminalEvent,
) -> anyhow::Result<()> {
    // TODO: Check if TUI handles event before passing to editor?

    if let Ok(event) = event_t2i(&event) {
        let actions = indigo_event::event::handle_event(editor, &event);
        if !actions.is_empty() {
            for action in actions.iter() {
                indigo_event::action::handle_action(editor, action);
            }
            return Ok(());
        }
    }

    let actions = match editor.mode {
        Mode::Normal(_) => handle_event_normal(editor, terminal, areas, event)?,
        Mode::Insert(_) => handle_event_insert(editor, terminal, areas, event)?,
        Mode::Command(_) => handle_event_command(editor, terminal, areas, event)?,
    };

    for action in actions.iter() {
        indigo_event::action::handle_action(editor, action);
    }

    Ok(())
}

#[expect(clippy::needless_pass_by_value)]
fn handle_event_normal(
    editor: &Editor,
    terminal: &mut Terminal,
    areas: Areas,
    event: TerminalEvent,
) -> anyhow::Result<Rc<[IndigoAction]>> {
    let action = match event {
        TerminalEvent::Key(key_event) => match (key_event.modifiers, key_event.code) {
            (KeyModifiers::CONTROL, KeyCode::Char('l')) => {
                terminal.clear()?;
                Rc::from([])
            }
            _ => Rc::from([]),
        },
        TerminalEvent::Mouse(mouse_event) => match (mouse_event.modifiers, mouse_event.kind) {
            (KeyModifiers::NONE, MouseEventKind::ScrollUp) => Rc::from([IndigoAction::ScrollUp]),
            (KeyModifiers::NONE, MouseEventKind::ScrollDown) => {
                Rc::from([IndigoAction::ScrollDown])
            }
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
                    editor.buffer.text(),
                    editor.buffer.vertical_scroll(),
                    areas.text,
                ) {
                    // TODO
                    // Rc::from([IndigoAction::MoveTo(index)])
                    let _ = index;
                    Rc::from([])
                } else {
                    Rc::from([])
                }
            }
            (KeyModifiers::NONE, MouseEventKind::Down(MouseButton::Right)) => {
                let position = Position {
                    x: mouse_event.column,
                    y: mouse_event.row,
                };
                if let Some(Err(index) | Ok(index)) = position_to_char_index(
                    position,
                    editor.buffer.text(),
                    editor.buffer.vertical_scroll(),
                    areas.text,
                ) {
                    // TODO
                    // Rc::from([IndigoAction::ExtendTo(index)])
                    let _ = index;
                    Rc::from([])
                } else {
                    Rc::from([])
                }
            }
            _ => Rc::from([]),
        },
        _ => Rc::from([]),
    };

    Ok(action)
}

fn handle_event_insert(
    _editor: &Editor,
    terminal: &mut Terminal,
    _areas: Areas,
    event: TerminalEvent,
) -> anyhow::Result<Rc<[IndigoAction]>> {
    let action = match event {
        TerminalEvent::Key(key_event) => match (key_event.modifiers, key_event.code) {
            (KeyModifiers::CONTROL, KeyCode::Char('l')) => {
                terminal.clear()?;
                Rc::from([])
            }
            _ => Rc::from([]),
        },
        TerminalEvent::Mouse(mouse_event) => match (mouse_event.modifiers, mouse_event.kind) {
            (KeyModifiers::NONE, MouseEventKind::ScrollUp) => Rc::from([IndigoAction::ScrollUp]),
            (KeyModifiers::NONE, MouseEventKind::ScrollDown) => {
                Rc::from([IndigoAction::ScrollDown])
            }
            _ => Rc::from([]),
        },
        TerminalEvent::Paste(text) => Rc::from([IndigoAction::Insert(Rc::from(text))]),
        _ => Rc::from([]),
    };

    Ok(action)
}

#[expect(clippy::unnecessary_wraps)]
fn handle_event_command(
    _editor: &Editor,
    _terminal: &mut Terminal,
    _areas: Areas,
    event: TerminalEvent,
) -> anyhow::Result<Rc<[IndigoAction]>> {
    let action = match event {
        TerminalEvent::Paste(text) => Rc::from([IndigoAction::Insert(Rc::from(text))]),
        _ => Rc::from([]),
    };

    Ok(action)
}

pub fn event_t2i(event: &TerminalEvent) -> anyhow::Result<IndigoEvent> {
    match event {
        TerminalEvent::Key(key_event) => Ok(IndigoEvent::KeyInput(key_t2i(*key_event)?)),
        _ => Err(anyhow!(
            "Unsupported crossterm->indigo event conversion: {event:?}"
        )),
    }
}

pub fn event_i2t(event: &IndigoEvent) -> anyhow::Result<TerminalEvent> {
    match event {
        IndigoEvent::KeyInput(key_event) => Ok(TerminalEvent::Key(key_i2t(*key_event))),
    }
}
