use indigo_core::{Editor, Mode};
use ratatui::prelude::{Buffer as Surface, *};

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    let mode = match editor.mode() {
        Mode::Normal(_) => "normal",
        Mode::Insert(_) => "insert",
    };

    let Some(buffer) = editor.current_buffer() else {
        // TODO: What should be shown when there are no buffers, or there is no current buffer?
        return;
    };

    let path = buffer.path();

    let modified = if buffer.is_modified() { " [+]" } else { "" };

    surface.set_string(
        area.x,
        area.y,
        format!("{mode} {path}{modified}"),
        Style::default(),
    );
}
