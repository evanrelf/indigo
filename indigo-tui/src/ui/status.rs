use crate::ui::colors::GRAY_LIGHT;
use indigo_core::{Editor, Mode, Position};
use ratatui::{
    prelude::{Buffer as Surface, *},
    widgets::{Paragraph, Widget as _},
};
use std::cmp::min;

pub fn render(editor: &Editor, mut area: Rect, surface: &mut Surface) {
    let buffer = editor.current_buffer();

    let modified = if buffer.is_modified() { "+ " } else { "" };

    let path = buffer.path();

    let Position { line, column } = buffer.selection().primary().cursor();

    let count = buffer.selection().ranges().len();

    let mode = match editor.mode() {
        Mode::Normal(_) => "n",
        Mode::Insert(_) => "i",
        Mode::Command(_) => "c",
    };

    area.y += area.height.saturating_sub(1);
    area.height = min(1, area.height);

    Paragraph::new(format!("{modified}{path} {line}:{column}/{count} {mode}"))
        .style(Style::default().bg(GRAY_LIGHT))
        .alignment(Alignment::Right)
        .render(area, surface);
}
