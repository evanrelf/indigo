use indigo_core::{Editor, Mode};
use ratatui::prelude::{Buffer as Surface, *};

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    let mode = match editor.mode {
        Mode::Normal(_) => "normal",
        Mode::Insert(_) => "insert",
    };

    // TODO: Show path for current buffer, show something else if no buffers
    let Some(buffer) = &editor.buffers.get(0) else {
        return;
    };

    let path = buffer.path().display();

    let modified = if buffer.is_modified() { " [+]" } else { "" };

    surface.set_string(
        area.x,
        area.y,
        format!("{mode} {path}{modified}"),
        Style::default(),
    );
}
