use indigo_core::{Editor, Position};
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

    Paragraph::new(format!("{path}{modified} {line}:{column}/{count}"))
        .alignment(Alignment::Right)
        .render(area, surface);
}
