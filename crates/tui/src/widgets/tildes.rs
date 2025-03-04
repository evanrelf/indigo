use indigo_core::prelude::*;
use ratatui::prelude::{Buffer as Surface, *};

pub struct Tildes<'a> {
    editor: &'a Editor,
}

impl<'a> Tildes<'a> {
    pub fn new(editor: &'a Editor) -> Self {
        Self { editor }
    }
}

impl Widget for Tildes<'_> {
    #[tracing::instrument(skip_all)]
    fn render(self, area: Rect, surface: &mut Surface) {
        let total_lines = self.editor.rope().len_lines_indigo();

        for (i, row) in area.rows().enumerate() {
            let line_number = i + self.editor.vertical_scroll() + 1;

            if line_number <= total_lines {
                continue;
            }

            let bottom = row.y + 1 == area.bottom();

            let style = if bottom {
                Modifier::UNDERLINED
            } else {
                Modifier::empty()
            };

            Line::styled("~", style).render(row, surface);
        }
    }
}
