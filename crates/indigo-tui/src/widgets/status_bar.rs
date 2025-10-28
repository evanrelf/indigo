use indigo_core::prelude::*;
use ratatui::prelude::{Buffer as Surface, *};

pub struct StatusBar<'a> {
    pub editor: &'a Editor,
}

impl<'a> StatusBar<'a> {
    #[must_use]
    pub fn new(editor: &'a Editor) -> Self {
        Self { editor }
    }
}

impl Widget for StatusBar<'_> {
    fn render(self, area: Rect, surface: &mut Surface) {
        let buffer = &self.editor.buffer;

        let range = buffer.range();

        let anchor = range.anchor().char_offset();

        let head = range.head().char_offset();

        let char_length = range.char_length();

        let grapheme_length = range.grapheme_length();

        let display_width = range.slice().display_width();

        let eof = range.is_eof();

        let mode = match self.editor.mode {
            Mode::Normal(_) => "normal",
            Mode::Insert(_) => "insert",
            Mode::Command(_) => "command",
        };

        let count = self.editor.mode.count();

        let status_bar = [
            format!("anchor={anchor}"),
            format!("head={head}"),
            format!("char_length={char_length}"),
            format!("grapheme_length={grapheme_length}"),
            format!("display_width={display_width}"),
            format!("eof={eof}"),
            format!("mode={mode}"),
            format!("count={count}"),
        ]
        .join(" ");

        Line::raw(status_bar).render(area, surface);
    }
}
