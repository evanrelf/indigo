use ratatui::{
    prelude::{Buffer as Surface, *},
    widgets::Widget,
};

pub struct Status;

impl Widget for Status {
    fn render(self, area: Rect, surface: &mut Surface) {
        surface.set_string(area.x, area.y, "status", Style::default());
    }
}
