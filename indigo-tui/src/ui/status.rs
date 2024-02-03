use crate::ui::colors::{BG_GRAY, FG_GRAY_LIGHT};
use indigo_core::{Editor, Mode, Position};
use ratatui::{
    prelude::{Buffer as Surface, *},
    widgets::Paragraph,
};

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    let file = editor.current_file();

    let buffer = file.buffer();

    let modified = if file.is_modified() { "+ " } else { "" };

    let path = file.path();

    let Position { line, column } = buffer.selection().primary_range().cursor();

    let count = buffer.selection().ranges().len();

    let mode = match editor.mode() {
        Mode::Normal(_) => "n",
        Mode::Insert(_) => "i",
        Mode::Command(_) => "c",
    };

    let fg = if matches!(editor.mode(), Mode::Command(_)) {
        FG_GRAY_LIGHT
    } else {
        Color::Reset
    };

    Paragraph::new(format!("{modified}{path} {line}:{column}/{count} {mode}"))
        .style(Style::default().fg(fg).bg(BG_GRAY))
        .alignment(Alignment::Right)
        .render(area, surface);
}
