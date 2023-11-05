use indigo_core::{Conversion, Editor, Mode, Position};
use ratatui::{
    prelude::{Buffer as Surface, *},
    widgets::{Paragraph, Widget as _},
};

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

    let position = {
        let position @ Position { line, column } = buffer.selection().primary().cursor();
        let index = match position.to_char_index(buffer.contents()) {
            Conversion::Invalid => panic!("Position {line}:{column} is not valid char index"),
            Conversion::Corrected(index) | Conversion::Valid(index) => index,
        };
        format!("{line}:{column}#{index}")
    };

    let count = buffer.selection().ranges().len();

    Paragraph::new(format!("{mode} {path}{modified} {position} ({count})"))
        .alignment(Alignment::Right)
        .render(area, surface);
}
