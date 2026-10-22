use crate::{
    areas::{Areas, position_to_byte_index},
    key::{key_event_i2t, key_event_t2i},
};
use anyhow::anyhow;
use indigo_core::{
    mode::{insert, normal, prompt},
    prelude::*,
    window::{scroll_down, scroll_up},
};
use ratatui::crossterm::{
    self,
    event::{KeyCode, KeyModifiers, MouseButton, MouseEventKind},
};
use ratatui::layout::Position;

pub type IndigoEvent = indigo_core::session::Event;

pub type TerminalEvent = crossterm::event::Event;

#[must_use]
pub fn should_skip_event(event: &TerminalEvent, areas: Areas) -> bool {
    match event {
        TerminalEvent::Key(key_event) => {
            matches!(key_event.code, KeyCode::Modifier(_)) || key_event.kind.is_release()
        }
        TerminalEvent::Mouse(mouse_event) => {
            matches!(
                mouse_event.kind,
                MouseEventKind::Moved | MouseEventKind::ScrollLeft | MouseEventKind::ScrollRight
            )
            // Ignore mouse events outside the UI (e.g. when its size is limited)
            || !areas.ui.contains(Position {
                x: mouse_event.column,
                y: mouse_event.row,
            })
        }
        _ => false,
    }
}

pub fn handle_event(server: &mut Server, areas: Areas, event: TerminalEvent) -> anyhow::Result<()> {
    // Terminals typically send line breaks in pasted text as `\r`, like the Enter key.
    let event = match event {
        TerminalEvent::Paste(text) => {
            TerminalEvent::Paste(text.replace("\r\n", "\n").replace('\r', "\n"))
        }
        event => event,
    };

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
        server.session.message = None;
    }

    if let Ok(event) = event_t2i(&event) {
        let handled = server.handle_event(event)?;
        if handled {
            return Ok(());
        }
    }

    let session = &mut server.session;

    let _handled = match session.mode {
        Mode::Normal => handle_event_normal(session, areas, event),
        Mode::Seek(_) => handle_event_seek(session, areas, event)?,
        Mode::Insert => handle_event_insert(session, areas, event),
        Mode::Prompt(_) => handle_event_prompt(session, areas, event)?,
        Mode::Replace => handle_event_replace(session, areas, event)?,
    };

    Ok(())
}

#[expect(clippy::needless_pass_by_value)]
fn handle_event_normal(session: &mut Session, areas: Areas, event: TerminalEvent) -> bool {
    let mut handled = true;

    match event {
        // TODO(horizontal_scroll)
        TerminalEvent::Mouse(mouse_event) => match (mouse_event.modifiers, mouse_event.kind) {
            (KeyModifiers::NONE, MouseEventKind::ScrollUp) => scroll_up(session),
            (KeyModifiers::NONE, MouseEventKind::ScrollDown) => scroll_down(session),
            // TODO: Kakoune allows creating new selection ranges by control clicking. Would be
            // awesome if Indigo could do the same, but also support control dragging to create
            // vertical lines of selection ranges, akin to Vim's visual block mode. Could snap to
            // the same column? Might be weird in the presence of wide characters.
            (KeyModifiers::NONE, MouseEventKind::Down(MouseButton::Left)) => {
                let position = Position {
                    x: mouse_event.column,
                    y: mouse_event.row,
                };
                if let Some(byte_index) = position_to_byte_index(
                    position,
                    session.focused_buffer().text.rope(),
                    session.focused_window().vertical_scroll(),
                    areas.text,
                )
                .byte_index()
                {
                    let mut window = session.focused_window_mut();
                    window
                        .selection_move()
                        .for_each_move(|mut range| range.move_to(byte_index));
                    window.selection_move().merge_overlapping();
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
                if let Some(byte_index) = position_to_byte_index(
                    position,
                    session.focused_buffer().text.rope(),
                    session.focused_window().vertical_scroll(),
                    areas.text,
                )
                .byte_index()
                {
                    let mut window = session.focused_window_mut();
                    window
                        .selection_move()
                        .for_each_move(|mut range| range.extend_to(byte_index));
                    window.selection_move().merge_overlapping();
                } else {
                    handled = false;
                }
            }
            _ => handled = false,
        },
        _ => handled = false,
    }

    handled
}

#[expect(clippy::needless_pass_by_value)]
#[expect(clippy::unnecessary_wraps)]
fn handle_event_seek(
    session: &mut Session,
    _areas: Areas,
    event: TerminalEvent,
) -> anyhow::Result<bool> {
    let mut handled = true;

    match event {
        // TODO: Add mouse events to core, so it can handle this internally.
        TerminalEvent::Mouse(mouse_event) => match (mouse_event.modifiers, mouse_event.kind) {
            (KeyModifiers::NONE, _) => {
                normal::enter(session);
            }
            _ => handled = false,
        },
        _ => handled = false,
    }

    Ok(handled)
}

#[expect(clippy::needless_pass_by_value)]
#[expect(clippy::unnecessary_wraps)]
fn handle_event_replace(
    session: &mut Session,
    _areas: Areas,
    event: TerminalEvent,
) -> anyhow::Result<bool> {
    let mut handled = true;

    match event {
        // TODO: Add mouse events to core, so it can handle this internally.
        TerminalEvent::Mouse(mouse_event) => match (mouse_event.modifiers, mouse_event.kind) {
            (KeyModifiers::NONE, _) => {
                normal::enter(session);
            }
            _ => handled = false,
        },
        _ => handled = false,
    }

    Ok(handled)
}

fn handle_event_insert(session: &mut Session, _areas: Areas, event: TerminalEvent) -> bool {
    let mut handled = true;

    match event {
        // TODO: Add mouse events to core, so it can handle this internally.
        TerminalEvent::Mouse(mouse_event) => match (mouse_event.modifiers, mouse_event.kind) {
            (KeyModifiers::NONE, MouseEventKind::ScrollUp) => scroll_up(session),
            (KeyModifiers::NONE, MouseEventKind::ScrollDown) => scroll_down(session),
            _ => handled = false,
        },
        TerminalEvent::Paste(text) => insert::paste(session, &text),
        _ => handled = false,
    }

    handled
}

#[expect(clippy::unnecessary_wraps)]
fn handle_event_prompt(
    session: &mut Session,
    _areas: Areas,
    event: TerminalEvent,
) -> anyhow::Result<bool> {
    let mut handled = true;

    match event {
        TerminalEvent::Paste(text) => prompt::paste(session, &text),
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
