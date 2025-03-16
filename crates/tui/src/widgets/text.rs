use indigo_core::prelude::*;
use ratatui::prelude::{Buffer as Surface, *};

pub struct Text<'a> {
    pub editor: &'a Editor,
}

impl<'a> Text<'a> {
    #[must_use]
    pub fn new(editor: &'a Editor) -> Self {
        Self { editor }
    }
}

impl Widget for Text<'_> {
    #[tracing::instrument(skip_all)]
    fn render(self, area: Rect, surface: &mut Surface) {
        let buffer = &self.editor.buffer;

        let lines = buffer.text().lines_at(buffer.vertical_scroll());

        let rows = area.rows();

        'line: for (line, mut rect) in lines.zip(rows) {
            'grapheme: for grapheme in line.graphemes() {
                let span = match grapheme.get_char(0) {
                    Some('\t') => Span::styled("→       ", Color::Rgb(0xee, 0xee, 0xee)),
                    Some('\n') => Span::styled("¬", Color::Rgb(0xee, 0xee, 0xee)),
                    _ => Span::raw(grapheme),
                };

                let width_usize = grapheme.display_width();

                let width_u16 = u16::try_from(width_usize).unwrap();

                if rect.x + width_u16 > rect.right() {
                    continue 'line;
                }

                if width_usize == 0 {
                    continue 'grapheme;
                }

                span.render(rect, surface);

                rect.x += width_u16;
            }
        }

        if let Some(bottom_row) = area.rows().last() {
            surface.set_style(bottom_row, Modifier::UNDERLINED);
        }
    }
}
