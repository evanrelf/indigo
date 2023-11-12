use crate::ui::command;
use indigo_core::{Buffer, Editor, Mode, Position, RopeExt};
use ratatui::prelude::{Constraint, Direction, Layout, Rect};
use std::cmp::max;

pub struct Areas {
    pub numbers: Rect,
    pub buffer: Rect,
    pub status: Rect,
}

impl Areas {
    pub fn new(editor: &Editor, area: Rect) -> Self {
        let status_height = match editor.mode() {
            Mode::Command(command_mode) => command::lines(command_mode, area),
            _ => 1,
        };

        let vertical = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                // numbers + buffer
                Constraint::Min(0),
                // status
                Constraint::Length(status_height),
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
        }
    }

    pub fn to_buffer_position(&self, position: (u16, u16), buffer: &Buffer) -> Option<Position> {
        let (area_line, area_column) = to_area_position(position, self.buffer)?;

        let buffer_line = usize::from(area_line) + buffer.vertical_scroll();

        let buffer_column = usize::from(area_column) + buffer.horizontal_scroll();

        let naive_buffer_position = Position {
            line: buffer_line,
            column: buffer_column,
        };

        naive_buffer_position.corrected(buffer.contents()).ok()
    }
}

pub fn from_area_position(area_position: (u16, u16), area: Rect) -> (u16, u16) {
    let (area_line, area_column) = area_position;
    (
        from_area_line(area_line, area),
        from_area_column(area_column, area),
    )
}

pub fn from_area_line(area_line: u16, area: Rect) -> u16 {
    area.top() + area_line
}

pub fn from_area_column(area_column: u16, area: Rect) -> u16 {
    area.left() + area_column
}

pub fn to_area_position(absolute_position: (u16, u16), area: Rect) -> Option<(u16, u16)> {
    let (absolute_line, absolute_column) = absolute_position;
    Some((
        to_area_line(absolute_line, area)?,
        to_area_column(absolute_column, area)?,
    ))
}

pub fn to_area_line(absolute_line: u16, area: Rect) -> Option<u16> {
    if !(area.top()..area.bottom()).contains(&absolute_line) {
        return None;
    }
    Some(absolute_line - area.top())
}

pub fn to_area_column(absolute_column: u16, area: Rect) -> Option<u16> {
    if !(area.left()..area.right()).contains(&absolute_column) {
        return None;
    };
    Some(absolute_column - area.left())
}
