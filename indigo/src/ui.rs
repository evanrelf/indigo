mod buffer;
mod command;
mod numbers;
mod status;

use crate::ui::{buffer::Buffer, command::Command, numbers::Numbers, status::Status};
use ratatui::{
    prelude::{Buffer as Surface, *},
    widgets::Widget,
};

pub struct Indigo;

impl Widget for Indigo {
    fn render(self, area: Rect, surface: &mut Surface) {
        let areas = areas(area);
        Numbers.render(areas.numbers, surface);
        Buffer.render(areas.buffer, surface);
        Status.render(areas.status, surface);
        Command.render(areas.command, surface);
    }
}

struct Areas {
    numbers: Rect,
    buffer: Rect,
    status: Rect,
    command: Rect,
}

fn areas(area: Rect) -> Areas {
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
