use crate::{areas::char_index_to_area, colors};
use indigo_core::prelude::*;
use ratatui::prelude::{Buffer as Surface, *};
use std::borrow::Cow;

pub struct CommandBar<'a> {
    pub editor: &'a Editor,
}

impl<'a> CommandBar<'a> {
    #[must_use]
    pub fn new(editor: &'a Editor) -> Self {
        Self { editor }
    }
}

impl Widget for CommandBar<'_> {
    #[tracing::instrument(skip_all)]
    fn render(self, mut area: Rect, surface: &mut Surface) {
        let Mode::Command(ref normal_mode) = self.editor.mode else {
            return;
        };

        if let Some(cell) = surface.cell_mut(area.as_position()) {
            cell.set_char(':');
        } else {
            unreachable!();
        }

        area.x += 1;
        area.width -= 1;

        Line::raw(Cow::<str>::from(normal_mode.text().rope())).render(area, surface);

        if let Some(rect) = char_index_to_area(
            normal_mode.cursor().char_offset(),
            normal_mode.text(),
            0,
            area,
        ) {
            surface.set_style(rect, Style::default().bg(colors::DARK_YELLOW));
        }
    }
}
