#![allow(clippy::uninlined_format_args)]

use indigo_term::{
    Terminal,
    input::{Event, KeyCode, KeyModifiers, physical::Token},
    output::{
        BOLD, CLEAR_SCREEN, CURSOR_HIDE, CURSOR_SHOW, CursorPosition, ModeReset, ModeSet,
        PointerShape, PointerShapeSet, RESET, UNDERLINE,
    },
};
use std::{
    cmp::max,
    io::{self, Write as _},
    iter::zip,
    str,
};

// Report mouse motion even when no button is pressed
// https://invisible-island.net/xterm/ctlseqs/ctlseqs.html#h3-Any-event-tracking
const ANY_EVENT_MOUSE_MODE: u16 = 1003;

// Report mouse events as `CSI < button ; column ; row M`
// https://invisible-island.net/xterm/ctlseqs/ctlseqs.html#h3-Extended-coordinates
const SGR_MOUSE_MODE: u16 = 1006;

// Longest shape name ("vertical-text") plus padding
const CELL_WIDTH: u16 = 15;

// Header goes on row 1, grid starts on row 3
const GRID_TOP: u16 = 3;

fn main() -> io::Result<()> {
    let mut terminal = Terminal::init()?;
    terminal.enter_alternate_screen()?;
    write!(
        terminal,
        "{}{}{}",
        ModeSet(ANY_EVENT_MOUSE_MODE),
        ModeSet(SGR_MOUSE_MODE),
        CURSOR_HIDE
    )?;

    let result = run(&mut terminal);

    write!(
        terminal,
        "{}{}{}",
        CURSOR_SHOW,
        ModeReset(SGR_MOUSE_MODE),
        ModeReset(ANY_EVENT_MOUSE_MODE)
    )?;
    terminal.flush()?;

    result
}

fn run(terminal: &mut Terminal) -> io::Result<()> {
    let (_, mut width) = terminal.size()?;
    let mut hovered = None;

    render(terminal, width, hovered)?;

    loop {
        match terminal.read_event()? {
            Event::KeyPress(key) => match key.code {
                KeyCode::Char('q') | KeyCode::Esc if key.modifiers.is_empty() => break,
                KeyCode::Char('c') if key.modifiers == KeyModifiers::CTRL => break,
                _ => {}
            },
            Event::Resize {
                width: new_width, ..
            } => {
                width = new_width;
                render(terminal, width, hovered)?;
            }
            event => {
                if let Some((row, column)) = mouse_position(&event) {
                    let shape = shape_at(width, row, column);
                    if shape != hovered {
                        hovered = shape;
                        write!(
                            terminal,
                            "{}",
                            PointerShapeSet(hovered.unwrap_or(PointerShape::Default))
                        )?;
                        render(terminal, width, hovered)?;
                    }
                }
            }
        }
    }

    Ok(())
}

fn render(terminal: &mut Terminal, width: u16, hovered: Option<PointerShape>) -> io::Result<()> {
    terminal.begin_sync_update()?;

    write!(
        terminal,
        "{}{}hover over a shape name, press q to quit",
        CLEAR_SCREEN,
        CursorPosition(1, 1)
    )?;

    let columns = columns(width);
    for (index, shape) in zip(0.., PointerShape::ALL) {
        let row = GRID_TOP + index / columns;
        let column = 1 + (index % columns) * CELL_WIDTH;
        write!(terminal, "{}", CursorPosition(row, column))?;
        if Some(shape) == hovered {
            write!(terminal, "{BOLD}{UNDERLINE}{shape}{RESET}")?;
        } else {
            write!(terminal, "{shape}")?;
        }
    }

    terminal.end_sync_update()?;

    Ok(())
}

fn columns(width: u16) -> u16 {
    max(1, width / CELL_WIDTH)
}

/// The shape whose name is displayed at this 1-based position, if any.
fn shape_at(width: u16, row: u16, column: u16) -> Option<PointerShape> {
    let columns = columns(width);
    let grid_row = row.checked_sub(GRID_TOP)?;
    let grid_column = column.saturating_sub(1) / CELL_WIDTH;
    if grid_column >= columns {
        return None;
    }
    let index = usize::from(grid_row * columns + grid_column);
    let shape = *PointerShape::ALL.get(index)?;
    let offset = column.saturating_sub(1) % CELL_WIDTH;
    (usize::from(offset) < shape.to_string().len()).then_some(shape)
}

/// Pick the 1-based (row, column) out of an SGR mouse report, which `indigo-term` doesn't decode
/// yet.
fn mouse_position(event: &Event) -> Option<(u16, u16)> {
    let Event::Unknown(Token::Csi {
        parameter_bytes,
        intermediate_bytes,
        final_byte: b'M' | b'm',
    }) = event
    else {
        return None;
    };
    if !intermediate_bytes.is_empty() {
        return None;
    }
    let parameters = str::from_utf8(parameter_bytes).ok()?.strip_prefix('<')?;
    let mut parameters = parameters.split(';');
    let _button = parameters.next()?;
    let column = parameters.next()?.parse().ok()?;
    let row = parameters.next()?.parse().ok()?;
    Some((row, column))
}
