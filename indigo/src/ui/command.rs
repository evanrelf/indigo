use ratatui::{
    prelude::{Buffer as Surface, *},
    widgets::Widget,
};

pub struct Command;

impl Widget for Command {
    fn render(self, area: Rect, surface: &mut Surface) {
        surface.set_string(area.x, area.y, "command", Style::default());
    }
}
