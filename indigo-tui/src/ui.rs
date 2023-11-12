mod buffer;
mod colors;
mod column;
mod command;
mod numbers;
mod selection;
mod status;

use indigo_core::{Editor, RopeExt};
use ratatui::prelude::{Buffer as Surface, *};
use std::cmp::max;

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    let areas = Areas::new(editor, area);
    numbers::render(editor, areas.numbers, surface);
    buffer::render(editor, areas.buffer, surface);
    selection::render(editor, areas.buffer, surface);
    column::render(editor, areas.buffer, surface);
    status::render(editor, areas.status, surface);
    command::render(editor, areas.command, surface);
}

pub struct Areas {
    numbers: Rect,
    buffer: Rect,
    status: Rect,
    command: Rect,
}

impl Areas {
    pub fn new(editor: &Editor, area: Rect) -> Self {
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

        let numbers_width = {
            let n = editor.current_buffer().contents().len_lines_indigo();
            let digits = 1 + max(1, n).ilog10();
            u16::try_from(max(2, digits) + 1).unwrap()
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

        Self {
            numbers: horizontal[0],
            buffer: horizontal[1],
            status: vertical[1],
            command: vertical[2],
        }
    }
}
