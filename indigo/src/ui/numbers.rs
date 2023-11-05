use indigo_core::Editor;
use ratatui::prelude::{Buffer as Surface, *};

pub fn render(_editor: &Editor, area: Rect, surface: &mut Surface) {
    surface.set_string(area.x, area.y, "~", Style::default());
}
