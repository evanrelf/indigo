use indigo_core::{Editor, Mode, Position};
use ratatui::{
    prelude::{Buffer as Surface, *},
    widgets::{Paragraph, Widget as _},
};

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    let buffer = editor.current_buffer();

    let path = buffer.path();

    let modified = if buffer.is_modified() { " [+]" } else { "" };

    let Position { line, column } = buffer.selection().primary().cursor();

    let count = buffer.selection().ranges().len();

    let mode = match editor.mode() {
        Mode::Normal(_) => "n",
        Mode::Insert(_) => "i",
        Mode::Command(_) => "c",
    };

    Paragraph::new(format!("{path}{modified} {line}:{column}/{count} {mode}"))
        .alignment(Alignment::Right)
        .render(area, surface);
}
