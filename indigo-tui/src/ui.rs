mod areas;
mod buffer;
mod colors;
mod column;
mod command;
mod numbers;
mod selection;
mod status;
mod tildes;

use crate::ui::areas::Areas;
use indigo_core::Editor;
use ratatui::prelude::{Buffer as Surface, Rect};

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    let areas = Areas::new(editor, area);
    tildes::render(editor, areas.tildes, surface);
    numbers::render(editor, areas.numbers, surface);
    buffer::render(editor, areas.buffer, surface);
    selection::render(editor, areas.buffer, surface);
    column::render(editor, areas.buffer, surface);
    status::render(editor, areas.status, surface);
    command::render(editor, areas.status, surface);
}
