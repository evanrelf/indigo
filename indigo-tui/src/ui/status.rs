use crate::ui::colors::{BG_GRAY, FG_GRAY_LIGHT};
use indigo_core::{Editor, Mode, Position, RopeExt};
use ratatui::{
    prelude::{Buffer as Surface, *},
    widgets::Paragraph,
};
use std::cmp::min;

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface, buffer_area: Rect) {
    let window = editor.current_window();

    let file = window.file();

    let buffer = file.buffer();

    let modified = if file.is_modified() { "+ " } else { "" };

    let path = file.path();

    let Position { line, column } = buffer.selection().primary_range().cursor();

    let count = buffer.selection().ranges().len();

    let percent = {
        let bottom_line = window.vertical_scroll() + usize::from(buffer_area.height);

        let total_lines = buffer.contents().len_lines_indigo();

        format!(
            "{:>3}%",
            (min(bottom_line, total_lines) * 100) / total_lines
        )
    };

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

    Paragraph::new(format!(
        "{modified}{path} {line}:{column}/{count} {percent} {mode}"
    ))
    .style(Style::default().fg(fg).bg(BG_GRAY))
    .alignment(Alignment::Right)
    .render(area, surface);
}
