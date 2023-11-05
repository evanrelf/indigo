mod buffer;
mod command;
mod numbers;
mod status;

use indigo_core::Editor;
use ratatui::prelude::{Buffer as Surface, *};

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    let areas = areas(area);
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

pub fn areas(area: Rect) -> Areas {
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

    let numbers_width = 3; // TODO: Calculate from `Editor`

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
