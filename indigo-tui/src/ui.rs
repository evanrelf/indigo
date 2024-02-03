pub mod areas;
pub mod buffer;
pub mod colors;
pub mod column;
pub mod command;
pub mod numbers;
pub mod scrollbar;
pub mod selection;
pub mod status;
pub mod tildes;

use crate::ui::areas::Areas;
use indigo_core::Editor;
use ratatui::prelude::Buffer as Surface;

pub fn render(editor: &Editor, areas: Areas, surface: &mut Surface) {
    tildes::render(editor, areas.tildes, surface);
    numbers::render(editor, areas.numbers, surface);
    buffer::render(editor, areas.buffer, surface);
    selection::render(editor, areas.buffer, surface);
    column::render(editor, areas.buffer, surface);
    scrollbar::render(editor, areas.scrollbar, surface);
    status::render(editor, areas.status, surface);
    command::render(editor, areas.status, surface);
}
