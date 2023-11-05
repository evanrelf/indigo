mod buffer;
mod command;
mod numbers;
mod status;

use indigo_core::{Editor, RopeExt};
use ratatui::prelude::{Buffer as Surface, *};

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    let areas = areas(editor, area);
    numbers::render(editor, areas.numbers, surface);
    buffer::render(editor, areas.buffer, surface);
    status::render(editor, areas.status, surface);
    command::render(editor, areas.command, surface);
}

pub struct Areas {
    numbers: Rect,
    buffer: Rect,
    status: Rect,
    command: Rect,
}

pub fn areas(editor: &Editor, area: Rect) -> Areas {
    let vertical = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            // numbers + buffer
            Constraint::Min(0),
            // status
            Constraint::Length(1),
            // command
            Constraint::Length(1),
        ])
        .split(area);

    // TODO: Make this better
    #[allow(
        clippy::cast_possible_truncation,
        clippy::cast_precision_loss,
        clippy::cast_sign_loss
    )]
    // Change to work with integers instead of floats once this is stabilized:
    // https://github.com/rust-lang/rust/issues/70887
    let numbers_width = {
        let n = match editor.current_buffer() {
            None => 0,
            Some(buffer) => buffer.contents().len_lines_indigo(),
        };
        // assert!(
        //     n != 0,
        //     "cannot handle rope with single-line file without newline yet"
        // );
        let n = n as f64;
        let digits = 1.0 + n.log10().floor();
        (digits.max(2.0) + 1.0) as u16
    };

    let horizontal = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            // numbers
            Constraint::Length(numbers_width),
            // buffer
            Constraint::Min(0),
        ])
        .split(vertical[0]);

    Areas {
        numbers: horizontal[0],
        buffer: horizontal[1],
        status: vertical[1],
        command: vertical[2],
    }
}
