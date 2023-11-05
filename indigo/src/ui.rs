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

        Numbers.render(horizontal[0], surface);
        Buffer.render(horizontal[1], surface);
        Status.render(vertical[1], surface);
        Command.render(vertical[2], surface);
    }
}
