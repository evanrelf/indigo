use indigo_core::Editor;
use ratatui::prelude::{Buffer as Surface, *};
use std::borrow::Cow;

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    let buffer = editor.current_buffer();

    for y in area.top()..area.bottom() {
        let line_index = usize::from(y) + buffer.vertical_scroll();

        let Some(line) = buffer.contents().get_line(line_index) else {
            break;
        };

        let Some(line) = line
            .get_slice(buffer.horizontal_scroll()..)
            .map(Cow::<str>::from)
        else {
            continue;
        };

        surface.set_stringn(area.x, y, line, usize::from(area.width), Style::default());
    }
}
