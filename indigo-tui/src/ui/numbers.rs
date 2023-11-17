use crate::ui::colors::BG_GRAY;
use indigo_core::{Editor, RopeExt};
use ratatui::prelude::{Buffer as Surface, *};

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    if area.width == 0 {
        return;
    }

    let window = editor.current_window();

    let vertical_scroll = window.vertical_scroll();

    let buffer = window.file().buffer();

    let total_lines = buffer.contents().len_lines_indigo();

    let number_width = usize::from(area.width) - 1;

    for y in area.top()..area.bottom() {
        let line_number = usize::from(y) + vertical_scroll + 1;

        if line_number <= total_lines {
            surface.set_stringn(
                area.x,
                y,
                format!("{line_number:>number_width$}│"),
                number_width + 1,
                Style::default().bg(BG_GRAY),
            );
        }
    }
}
