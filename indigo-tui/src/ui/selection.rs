use crate::ui::colors::{YELLOW, YELLOW_LIGHT};
use indigo_core::{Direction, Editor, Position};
use ratatui::prelude::{Buffer as Surface, *};

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    let buffer = editor.current_buffer();
    let rope = &buffer.contents();

    if rope.len_chars() == 0 {
        return;
    }

    for range in buffer.selection().ranges() {
        let range_slice = range.to_rope_slice(rope).unwrap().into_inner();

        for (i, _) in range_slice.chars().enumerate() {
            let i = match range.direction() {
                Direction::Forward => i + range.anchor().to_char_index(rope).unwrap().into_inner(),
                Direction::Backward => i + range.cursor().to_char_index(rope).unwrap().into_inner(),
            };

            let buffer_position = Position::from_char_index(i, rope).unwrap().into_inner();
            let buffer_line = buffer_position.line;
            let buffer_column = buffer_position.column;

            let view_line = buffer_line.saturating_sub(buffer.vertical_scroll());
            let view_column = buffer_column.saturating_sub(buffer.horizontal_scroll());

            let position_visible = [
                buffer_line >= buffer.vertical_scroll(),
                buffer_column >= buffer.horizontal_scroll(),
                view_line < usize::from(area.height),
                view_column < usize::from(area.width),
            ]
            .iter()
            .all(|x| *x);

            if position_visible {
                let x = area.left() + u16::try_from(view_column).unwrap();
                let y = area.top() + u16::try_from(view_line).unwrap();
                let color = if buffer_position == range.cursor() {
                    YELLOW
                } else {
                    YELLOW_LIGHT
                };
                surface.get_mut(x, y).set_bg(color);
            }
        }
    }
}
