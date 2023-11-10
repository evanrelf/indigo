use crate::ui::colors::GRAY_LIGHT;
use indigo_core::{Editor, RopeExt};
use ratatui::prelude::{Buffer as Surface, *};

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    let buffer = editor.current_buffer();

    let total_lines = buffer.contents().len_lines_indigo();

    let column = 81;

    for y in area.top()..area.bottom() {
        let line_number = usize::from(y) + buffer.vertical_scroll() + 1;

        let column_in_area = area.x + (column - 1) < area.right();
        let column_in_buffer = buffer.horizontal_scroll() < usize::from(column);
        let line_in_buffer = line_number <= total_lines;

        if column_in_area && column_in_buffer && line_in_buffer {
            let x = area.x + ((column - 1) - u16::try_from(buffer.horizontal_scroll()).unwrap());
            surface.get_mut(x, y).set_bg(GRAY_LIGHT);
        }
    }
}
