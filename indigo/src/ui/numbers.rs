use indigo_core::{Editor, RopeExt};
use ratatui::prelude::{Buffer as Surface, *};

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    // TODO: Show numbers for current buffer, show tildes if no buffers
    let Some(buffer) = &editor.buffers.get(0) else {
        return;
    };

    let total_lines = buffer.contents().len_lines_indigo();

    let number_width = usize::from(area.width) - 1;

    for y in area.top()..area.bottom() {
        let line_number = usize::from(y) + buffer.vertical_scroll() + 1;

        if line_number <= total_lines {
            surface.set_stringn(
                area.x,
                y,
                format!("{line_number:>number_width$}│"),
                number_width + 1,
                Style::default(),
            );
        } else {
            surface.get_mut(area.x, y).set_char('~');
        }
    }
}
