use indigo_core::{Buffer, Editor, Position, RopeExt};
use ratatui::prelude::{Constraint, Direction, Layout, Rect};
use std::cmp::max;

pub struct Areas {
    pub numbers: Rect,
    pub buffer: Rect,
    pub status: Rect,
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

    pub fn mouse_to_buffer_position(
        &self,
        mouse_position: (u16, u16),
        buffer: &Buffer,
    ) -> Option<Position> {
        let (area_line, area_column) = mouse_to_area_position(mouse_position, self.buffer)?;

        let buffer_line = usize::from(area_line) + buffer.vertical_scroll();

        let buffer_column = usize::from(area_column) + buffer.horizontal_scroll();

        let naive_buffer_position = Position {
            line: buffer_line,
            column: buffer_column,
        };

        naive_buffer_position.corrected(buffer.contents()).ok()
    }
}

pub fn mouse_to_area_position(mouse_position: (u16, u16), area: Rect) -> Option<(u16, u16)> {
    let (mouse_line, mouse_column) = mouse_position;
    Some((
        mouse_to_area_line(mouse_line, area)?,
        mouse_to_area_column(mouse_column, area)?,
    ))
}

pub fn mouse_to_area_line(mouse_line: u16, area: Rect) -> Option<u16> {
    if !(area.top()..area.bottom()).contains(&mouse_line) {
        return None;
    }
    Some(mouse_line - area.top())
}

pub fn mouse_to_area_column(mouse_column: u16, area: Rect) -> Option<u16> {
    if !(area.left()..area.right()).contains(&mouse_column) {
        return None;
    };
    Some(mouse_column - area.left())
}
