use crate::ui::colors::BG_GRAY;
use indigo_core::{Editor, RopeExt};
use ratatui::prelude::{Buffer as Surface, *};

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    let window = editor.current_window();

    let buffer = window.file().buffer();

    let vertical_scroll = window.vertical_scroll();
    let horizontal_scroll = u16::try_from(window.horizontal_scroll()).unwrap();

    let total_lines = buffer.contents().len_lines_indigo();

    if editor.settings.columns.is_empty() {
        return;
    }

    for y in area.top()..area.bottom() {
        let line_number = usize::from(y) + vertical_scroll + 1;

        let line_in_buffer = line_number <= total_lines;

        for column in &editor.settings.columns {
            let column_in_area = usize::from(area.x) + (column - 1) < usize::from(area.right());

            let column_in_buffer = usize::from(horizontal_scroll) < *column;

            if column_in_area && column_in_buffer && line_in_buffer {
                let x = u16::try_from(
                    usize::from(area.x) + ((column - 1) - usize::from(horizontal_scroll)),
                )
                .expect("`column - horizontal_scroll` will not exceed area width");

                surface.get_mut(x, y).set_bg(BG_GRAY);
            }
        }
    }
}
