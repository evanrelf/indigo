use indigo_core::Editor;
use ratatui::prelude::{Buffer as Surface, *};

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    // TODO: Show contents for current buffer, show nothing if no buffers
    let Some(buffer) = &editor.buffers.get(0) else {
        return;
    };

    for y in area.top()..area.bottom() {
        let line_index = usize::from(y) + buffer.vertical_scroll();

        if let Some(line) = buffer.contents().get_line(line_index) {
            if let Some(line) = line.get_slice(buffer.horizontal_scroll()..) {
                surface.set_stringn(
                    area.x,
                    y,
                    line.to_string(),
                    usize::from(area.width),
                    Style::default(),
                );
            }
        } else {
            break;
        }
    }
}
