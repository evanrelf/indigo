use indigo_core::Editor;
use ratatui::prelude::{Buffer as Surface, *};

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    let buffer = match editor.current {
        None => {
            // TODO: What should be shown when there are no buffers, or there is no current buffer?
            return;
        }
        Some(index) => match editor.buffers.get(index) {
            None => {
                panic!("Current editor buffer does not exist");
            }
            Some(buffer) => buffer,
        },
    };

    for y in area.top()..area.bottom() {
        let line_index = usize::from(y) + buffer.vertical_scroll();

        let Some(line) = buffer.contents().get_line(line_index) else {
            break;
        };

        let Some(line) = line.get_slice(buffer.horizontal_scroll()..) else {
            continue;
        };

        surface.set_stringn(
            area.x,
            y,
            line.to_string(),
            usize::from(area.width),
            Style::default(),
        );
    }
}
