use indigo_core::{Editor, RopeSliceExt as _};
use ratatui::prelude::{Buffer as Surface, *};

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    let window = editor.current_window();

    let vertical_scroll = window.vertical_scroll();

    let buffer = window.file().buffer();

    let total_lines = buffer.contents().len_lines_indigo();

    for y in area.top()..area.bottom() {
        let line_number = usize::from(y) + vertical_scroll + 1;

        if line_number > total_lines {
            surface.get_mut(area.x, y).set_char('~');
        }
    }
}
