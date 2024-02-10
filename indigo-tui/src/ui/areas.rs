use crate::ui::command;
use indigo_core::{settings::LineNumbers, Editor, Mode, Position, RopeExt, Window};
use ratatui::prelude::{Constraint, Direction, Layout, Rect};
use std::cmp::max;

#[derive(Clone, Copy)]
pub struct Areas {
    pub tildes: Rect,
    pub line_numbers: Rect,
    pub buffer: Rect,
    pub scrollbar: Rect,
    pub status: Rect,
}

impl Areas {
    pub fn empty() -> Self {
        Self {
            tildes: Rect::default(),
            line_numbers: Rect::default(),
            buffer: Rect::default(),
            scrollbar: Rect::default(),
            status: Rect::default(),
        }
    }

    pub fn new(editor: &Editor, area: Rect) -> Self {
        let status_height = match editor.mode() {
            Mode::Command(command_mode) => command::lines(command_mode, area),
            _ => 1,
        };

        let vertical = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                // line_numbers + buffer + scrollbar
                Constraint::Min(0),
                // status
                Constraint::Length(status_height),
            ])
            .split(area);

        let line_numbers_width = match editor.settings.line_numbers {
            LineNumbers::Hidden => 0,
            LineNumbers::Absolute => {
                let n = editor.current_file().buffer().contents().len_lines_indigo();
                let digits = 1 + max(1, n).ilog10();
                u16::try_from(max(2, digits) + 1).unwrap()
            }
        };

        let horizontal = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                // line_numbers
                Constraint::Length(line_numbers_width),
                // buffer
                Constraint::Fill(1),
                // scrollbar
                Constraint::Length(1),
            ])
            .split(vertical[0]);

        let line_numbers = horizontal[0];
        let buffer = horizontal[1];
        let scrollbar = horizontal[2];
        let status = vertical[1];
        let tildes = if line_numbers.width > 0 {
            line_numbers
        } else {
            buffer
        };

        Self {
            tildes,
            line_numbers,
            buffer,
            scrollbar,
            status,
        }
    }

    pub fn to_buffer_position(self, position: (u16, u16), window: &Window) -> Option<Position> {
        let buffer = window.file().buffer();

        let (area_line, area_column) = to_area_position(position, self.buffer)?;

        let buffer_line = usize::from(area_line) + window.vertical_scroll();

        let buffer_column = usize::from(area_column) + window.horizontal_scroll();

        let mut buffer_position = Position {
            line: buffer_line,
            column: buffer_column,
        };

        buffer_position.correct(buffer.contents());

        Some(buffer_position)
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
