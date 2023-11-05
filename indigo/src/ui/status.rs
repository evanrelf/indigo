use indigo_core::{Editor, Mode};
use ratatui::prelude::{Buffer as Surface, *};

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    let mode = match editor.mode {
        Mode::Normal(_) => "normal",
        Mode::Insert(_) => "insert",
    };

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

    let path = buffer.path().display();

    let modified = if buffer.is_modified() { " [+]" } else { "" };

    surface.set_string(
        area.x,
        area.y,
        format!("{mode} {path}{modified}"),
        Style::default(),
    );
}
