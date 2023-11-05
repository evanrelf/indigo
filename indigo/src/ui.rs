mod buffer;
mod command;
mod numbers;
mod status;

use indigo_core::Editor;
use ratatui::prelude::{Buffer as Surface, *};

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
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

    numbers::render(editor, horizontal[0], surface);
    buffer::render(editor, horizontal[1], surface);
    status::render(editor, vertical[1], surface);
    command::render(editor, vertical[2], surface);
}
