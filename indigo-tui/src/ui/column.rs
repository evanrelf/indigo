use crate::ui::colors::BG_GRAY;
use indigo_core::{Editor, RopeSliceExt};
use ratatui::prelude::{Buffer as Surface, *};

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    let window = editor.current_window();

    let buffer = window.file().buffer();

    let vertical_scroll = window.vertical_scroll();
    let horizontal_scroll = u16::try_from(window.horizontal_scroll()).unwrap();

    let total_lines = buffer.contents().len_lines_indigo();

    let columns = [81];

    if columns.is_empty() {
        return;
    }

    for y in area.top()..area.bottom() {
        let line_number = usize::from(y) + vertical_scroll + 1;

        let line_in_buffer = line_number <= total_lines;

        for column in columns {
            let column_in_area = area.x + (column - 1) < area.right();

            let column_in_buffer = horizontal_scroll < column;

            if column_in_area && column_in_buffer && line_in_buffer {
                let x = area.x + ((column - 1) - horizontal_scroll);
                surface.get_mut(x, y).set_bg(BG_GRAY);
            }
        }
    }
}
