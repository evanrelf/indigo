use indigo_core::prelude::*;
use ratatui::prelude::{Buffer as Surface, *};

pub struct LineNumbers<'a> {
    pub editor: &'a Editor,
}

impl<'a> LineNumbers<'a> {
    #[must_use]
    pub fn new(editor: &'a Editor) -> Self {
        Self { editor }
    }
}

impl Widget for LineNumbers<'_> {
    fn render(self, area: Rect, surface: &mut Surface) {
        let buffer = &self.editor.buffer;

        let total_lines = buffer.text().len_lines_indigo();

        for (i, row) in area.rows().enumerate() {
            let line_number = i + buffer.vertical_scroll() + 1;

            if line_number > total_lines {
                break;
            }

            Line::raw(format!("{line_number}│"))
                .right_aligned()
                .render(row, surface);
        }
    }
}
