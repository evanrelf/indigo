use ratatui::{
    prelude::{Buffer as Surface, *},
    widgets::Widget,
};

pub struct Numbers;

impl Widget for Numbers {
    fn render(self, area: Rect, surface: &mut Surface) {
        surface.set_string(area.x, area.y, "~", Style::default());
    }
}
