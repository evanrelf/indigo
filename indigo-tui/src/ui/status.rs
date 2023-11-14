use crate::ui::colors::BG_GRAY;
use indigo_core::{Editor, Mode, Position};
use ratatui::{
    prelude::{Buffer as Surface, *},
    widgets::{Paragraph, Widget as _},
};

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    let file = editor.current_file();

    let buffer = file.buffer();

    let modified = if file.is_modified() { "+ " } else { "" };

    let path = file.path();

    let Position { line, column } = buffer.selection().primary().cursor();

    let count = buffer.selection().ranges().len();

    let mode = match editor.mode() {
        Mode::Normal(_) => "n",
        Mode::Insert(_) => "i",
        Mode::Command(_) => "c",
    };

    Paragraph::new(format!("{modified}{path} {line}:{column}/{count} {mode}"))
        .style(Style::default().bg(BG_GRAY))
        .alignment(Alignment::Right)
        .render(area, surface);
}
