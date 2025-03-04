use indigo_core::prelude::*;
use ratatui::prelude::{Buffer as Surface, *};

pub struct LineNumbers<'a> {
    editor: &'a Editor,
}

impl<'a> LineNumbers<'a> {
    pub fn new(editor: &'a Editor) -> Self {
        Self { editor }
    }
}

impl Widget for LineNumbers<'_> {
    #[tracing::instrument(skip_all)]
    fn render(self, area: Rect, surface: &mut Surface) {
        let total_lines = self.editor.rope().len_lines_indigo();

        for (i, row) in area.rows().enumerate() {
            let line_number = i + self.editor.vertical_scroll() + 1;

            if line_number > total_lines {
                break;
            }

            Line::raw(format!("{line_number}│"))
                .right_aligned()
                .render(row, surface);
        }
    }
}
