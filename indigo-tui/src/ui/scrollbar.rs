use crate::ui::colors::GRAY_300;
use indigo_core::Editor;
use ratatui::prelude::{Buffer as Surface, *};

pub fn render(_editor: &Editor, area: Rect, surface: &mut Surface) {
    for y in area.top()..(area.bottom() / 2) {
        for x in area.left()..area.right() {
            surface.get_mut(x, y).set_bg(GRAY_300);
        }
    }
}
