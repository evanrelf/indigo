use indigo_core::Editor;
use ratatui::prelude::{Buffer as Surface, *};
use std::borrow::Cow;

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    if area.width == 0 {
        return;
    }

    let window = editor.current_window();

    let vertical_scroll = window.vertical_scroll();
    let horizontal_scroll = window.horizontal_scroll();

    let buffer = window.file().buffer();

    for y in area.top()..area.bottom() {
        let line_index = usize::from(y) + vertical_scroll;

        let Some(line) = buffer.contents().get_line(line_index) else {
            break;
        };

        let Some(line) = line.get_slice(horizontal_scroll..).map(Cow::<str>::from) else {
            continue;
        };

        surface.set_stringn(area.x, y, line, usize::from(area.width), Style::default());
    }
}
