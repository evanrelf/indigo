use crate::ui::colors::GRAY_400;
use indigo_core::Editor;
use ratatui::prelude::{Buffer as Surface, *};

pub fn render(_editor: &Editor, area: Rect, surface: &mut Surface) {
    let color = GRAY_400;

    for x in area.left()..area.right() {
        // Up arrow
        surface.get_mut(x, area.top()).set_char('▲').set_fg(color);

        // Down arrow
        surface
            .get_mut(x, area.bottom() - 1)
            .set_char('▼')
            .set_fg(color);
    }

    for y in (area.top() + 1)..(area.bottom() - 1) {
        // TODO: Remove
        if y > (area.bottom() / 2) {
            break;
        }

        for x in area.left()..area.right() {
            surface.get_mut(x, y).set_bg(color);
        }
    }
}
