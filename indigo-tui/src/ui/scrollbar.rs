use crate::ui::colors::GRAY_400;
use indigo_core::{Editor, RopeExt};
use ratatui::prelude::{Buffer as Surface, *};
use std::cmp::min;

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    if area.width == 0 || area.height == 0 {
        return;
    }

    let color = GRAY_400;

    let window = editor.current_window();

    let buffer = window.file().buffer();

    let total_lines = buffer.contents().len_lines_indigo();

    let scrollbar_gutter = usize::from(area.height) - 2;

    let scrollbar_height =
        (min(usize::from(area.height), total_lines) * scrollbar_gutter) / total_lines;

    for x in area.left()..area.right() {
        // Up arrow
        surface.get_mut(x, area.top()).set_char('▲').set_fg(color);

        // Down arrow
        surface
            .get_mut(x, area.bottom() - 1)
            .set_char('▼')
            .set_fg(color);
    }

    let start = area.top() + 1;

    let end = area.bottom() - 1;

    for y in start..end {
        if usize::from(y - start) > scrollbar_height {
            break;
        }

        for x in area.left()..area.right() {
            surface.get_mut(x, y).set_bg(color);
        }
    }
}
