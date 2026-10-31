use crate::{
    areas::{Areas, position_to_byte_index},
    key::{key_event_i2t, key_event_t2i},
    scroll_bar::{self, UNITS_PER_CELL},
    state::{MouseCapture, State},
};
use anyhow::anyhow;
use indigo_core::{
    mode::{insert, prompt, view},
    prelude::*,
};
use ratatui::crossterm::{
    self,
    event::{KeyCode, KeyModifiers, MouseButton, MouseEvent, MouseEventKind},
};
use ratatui::layout::Position;

pub type IndigoEvent = indigo_core::client::Event;

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

pub fn handle_event(
    server: &mut Server,
    areas: Areas,
    state: &mut State,
    event: TerminalEvent,
) -> anyhow::Result<()> {
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
        server.client_mut().clear_message();
    }

    if let Ok(event) = event_t2i(&event) {
        let handled = server.handle_event(event)?;
        if handled {
            return Ok(());
        }
    }

    // Pressing a mouse button ends any capture whose release was missed (e.g. outside the UI).
    if let TerminalEvent::Mouse(mouse_event) = &event
        && let MouseEventKind::Down(_) = mouse_event.kind
    {
        state.mouse_capture = None;
    }

    let client = &mut server.client_mut();

    if handle_event_scroll(client, &event) {
        return Ok(());
    }

    if handle_event_scroll_bar(client, areas, &mut state.mouse_capture, &event) {
        return Ok(());
    }

    let _handled = match &client.mode {
        Mode::Normal => handle_event_normal(client, areas, event),
        Mode::Insert => handle_event_insert(client, areas, event),
        Mode::Prompt(_) => handle_event_prompt(client, areas, event)?,
        Mode::View(_) => handle_event_view(client, areas, event),
    };

    Ok(())
}

// TODO: Add mouse events to core, so it can handle this internally.
// TODO(horizontal_scroll)
fn handle_event_scroll(client: &mut ClientMut, event: &TerminalEvent) -> bool {
    let TerminalEvent::Mouse(mouse_event) = event else {
        return false;
    };
    match (mouse_event.modifiers, mouse_event.kind) {
        (KeyModifiers::NONE, MouseEventKind::ScrollUp) => {
            client.focused_window_mut().scroll_up();
            true
        }
        (KeyModifiers::NONE, MouseEventKind::ScrollDown) => {
            client.focused_window_mut().scroll_down();
            true
        }
        _ => false,
    }
}

fn handle_event_scroll_bar(
    client: &mut ClientMut,
    areas: Areas,
    mouse_capture: &mut Option<MouseCapture>,
    event: &TerminalEvent,
) -> bool {
    let TerminalEvent::Mouse(mouse_event) = event else {
        return false;
    };

    match mouse_event.kind {
        MouseEventKind::Down(button) => {
            let Some(capture) = scroll_bar_down(client, areas, *mouse_event, button) else {
                return false;
            };
            *mouse_capture = Some(capture);
            true
        }
        // Don't let drags which started on the scroll bar fall through to the text.
        MouseEventKind::Drag(_) => match *mouse_capture {
            Some(MouseCapture::ScrollBarThumb {
                row_at_grab,
                thumb_start_at_grab,
            }) => {
                scroll_bar_drag(
                    client,
                    areas,
                    row_at_grab,
                    thumb_start_at_grab,
                    mouse_event.row,
                );
                true
            }
            Some(MouseCapture::ScrollBar) => true,
            None => false,
        },
        MouseEventKind::Up(_) => mouse_capture.take().is_some(),
        _ => false,
    }
}

fn scroll_bar_down(
    client: &mut ClientMut,
    areas: Areas,
    mouse_event: MouseEvent,
    button: MouseButton,
) -> Option<MouseCapture> {
    let position = Position {
        x: mouse_event.column,
        y: mouse_event.row,
    };

    if !areas.scroll_bar.contains(position) {
        return None;
    }

    // Don't let scroll bar clicks fall through to the text.
    if (mouse_event.modifiers, button) != (KeyModifiers::NONE, MouseButton::Left) {
        return Some(MouseCapture::ScrollBar);
    }

    let window = client.focused_window();

    let Some(thumb) = scroll_bar::thumb(
        window.buffer().text.rope().len_lines_unix(),
        usize::from(window.height()),
        window.vertical_scroll(),
        usize::from(areas.scroll_bar.height),
    ) else {
        return Some(MouseCapture::ScrollBar);
    };

    let cell_top = usize::from(position.y - areas.scroll_bar.y) * UNITS_PER_CELL;
    let cell_bottom = cell_top + UNITS_PER_CELL;

    if cell_bottom <= thumb.start {
        client.focused_window_mut().scroll_full_page_up();
        Some(MouseCapture::ScrollBar)
    } else if thumb.end <= cell_top {
        client.focused_window_mut().scroll_full_page_down();
        Some(MouseCapture::ScrollBar)
    } else {
        Some(MouseCapture::ScrollBarThumb {
            row_at_grab: position.y,
            thumb_start_at_grab: thumb.start,
        })
    }
}

fn scroll_bar_drag(
    client: &mut ClientMut,
    areas: Areas,
    row_at_grab: u16,
    thumb_start_at_grab: usize,
    row: u16,
) {
    // Move the thumb as far as the mouse moved, so the grabbed spot stays under the mouse.
    let thumb_start = if row < row_at_grab {
        thumb_start_at_grab.saturating_sub(usize::from(row_at_grab - row) * UNITS_PER_CELL)
    } else {
        thumb_start_at_grab + usize::from(row - row_at_grab) * UNITS_PER_CELL
    };

    let window = client.focused_window();

    let Some(line) = scroll_bar::vertical_scroll(
        window.buffer().text.rope().len_lines_unix(),
        usize::from(window.height()),
        thumb_start,
        usize::from(areas.scroll_bar.height),
    ) else {
        return;
    };

    client.focused_window_mut().scroll_to_line(line);
}

#[expect(clippy::needless_pass_by_value)]
fn handle_event_normal(client: &mut ClientMut, areas: Areas, event: TerminalEvent) -> bool {
    let mut handled = true;

    match event {
        TerminalEvent::Mouse(mouse_event) => match (mouse_event.modifiers, mouse_event.kind) {
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
                    client.focused_buffer().text.rope(),
                    client.focused_window().vertical_scroll(),
                    areas.text,
                )
                .byte_index()
                {
                    let mut window = client.focused_window_mut();
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
                    client.focused_buffer().text.rope(),
                    client.focused_window().vertical_scroll(),
                    areas.text,
                )
                .byte_index()
                {
                    let mut window = client.focused_window_mut();
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

fn handle_event_insert(client: &mut ClientMut, _areas: Areas, event: TerminalEvent) -> bool {
    let mut handled = true;

    match event {
        TerminalEvent::Paste(text) => insert::paste(client, &text),
        _ => handled = false,
    }

    handled
}

#[expect(clippy::needless_pass_by_value)]
fn handle_event_view(client: &mut ClientMut, _areas: Areas, event: TerminalEvent) -> bool {
    let mut handled = true;

    match event {
        TerminalEvent::Mouse(mouse_event) => match (mouse_event.modifiers, mouse_event.kind) {
            (KeyModifiers::NONE, MouseEventKind::Down(MouseButton::Left)) => {
                view::drag_start(client, mouse_event.row);
            }
            (KeyModifiers::NONE, MouseEventKind::Drag(MouseButton::Left)) => {
                view::drag_to(client, mouse_event.row);
            }
            (KeyModifiers::NONE, MouseEventKind::Up(MouseButton::Left)) => {
                view::drag_end(client);
            }
            _ => handled = false,
        },
        _ => handled = false,
    }

    handled
}

#[expect(clippy::unnecessary_wraps)]
fn handle_event_prompt(
    client: &mut ClientMut,
    _areas: Areas,
    event: TerminalEvent,
) -> anyhow::Result<bool> {
    let mut handled = true;

    match event {
        TerminalEvent::Paste(text) => prompt::paste(client, &text),
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
        IndigoEvent::Key(key_event) => Ok(TerminalEvent::Key(key_event_i2t(key_event)?)),
    }
}
