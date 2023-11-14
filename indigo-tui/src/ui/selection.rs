use crate::ui::colors::{BG_YELLOW, BG_YELLOW_LIGHT};
use indigo_core::{Editor, Position};
use ratatui::prelude::{Buffer as Surface, *};

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    let buffer = editor.current_file().buffer();

    let rope = &buffer.contents();

    if rope.len_chars() == 0 {
        return;
    }

    for range in buffer.selection().ranges() {
        let range_slice = range.to_rope_slice(rope).unwrap();

        let range_start_index = range.start().to_char_index(rope).unwrap();

        for (index, _) in range_slice.chars().enumerate() {
            let index = range_start_index + index;

            let buffer_position = Position::from_char_index(index, rope).unwrap();
            let buffer_line = buffer_position.line;
            let buffer_column = buffer_position.column;

            let area_line = buffer_line.saturating_sub(buffer.vertical_scroll());
            let area_column = buffer_column.saturating_sub(buffer.horizontal_scroll());

            let position_visible = [
                buffer_line >= buffer.vertical_scroll(),
                buffer_column >= buffer.horizontal_scroll(),
                area_line < usize::from(area.height),
                area_column < usize::from(area.width),
            ]
            .iter()
            .all(|x| *x);

            if position_visible {
                let x = area.left() + u16::try_from(area_column).unwrap();
                let y = area.top() + u16::try_from(area_line).unwrap();
                let color = if buffer_position == range.cursor() {
                    BG_YELLOW
                } else {
                    BG_YELLOW_LIGHT
                };
                surface.get_mut(x, y).set_bg(color);
            }
        }
    }
}
