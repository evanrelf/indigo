use ratatui::{
    prelude::{Buffer as Surface, *},
    widgets::Widget,
};

pub struct Buffer;

impl Widget for Buffer {
    fn render(self, area: Rect, surface: &mut Surface) {
        surface.set_string(area.x, area.y, "Hello, world!", Style::default());
    }
}
