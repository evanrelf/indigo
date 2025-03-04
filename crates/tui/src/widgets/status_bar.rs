use indigo_core::prelude::*;
use ratatui::prelude::{Buffer as Surface, *};

pub struct StatusBar<'a> {
    editor: &'a Editor,
}

impl<'a> StatusBar<'a> {
    pub fn new(editor: &'a Editor) -> Self {
        Self { editor }
    }
}

impl Widget for StatusBar<'_> {
    #[tracing::instrument(skip_all)]
    fn render(self, area: Rect, surface: &mut Surface) {
        let editor = self.editor;

        let anchor = editor.range().anchor();

        let head = editor.range().head();

        let char_length = editor.range().char_length();

        let grapheme_length = editor.range().grapheme_length(editor.rope());

        let display_width = editor.range().rope_slice(editor.rope()).display_width();

        let eof = editor.range().head() == editor.rope().len_chars();

        let mode = match editor.mode {
            Mode::Normal(_) => "normal",
            Mode::Insert => "insert",
            Mode::Command(_) => "command",
        };

        let count = editor.mode.count();

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
