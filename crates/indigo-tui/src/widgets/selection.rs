use crate::{
    areas::{char_index_to_area, line_index_to_area},
    colors,
};
use indigo_core::prelude::*;
use ratatui::prelude::{Buffer as Surface, *};

pub struct Selection<'a> {
    pub editor: &'a Editor,
}

impl<'a> Selection<'a> {
    #[must_use]
    pub fn new(editor: &'a Editor) -> Self {
        Self { editor }
    }
}

impl Widget for Selection<'_> {
    fn render(self, area: Rect, surface: &mut Surface) {
        let buffer = &self.editor.buffer;

        let rope = buffer.text();

        let range = buffer.range();

        let vertical_scroll = buffer.vertical_scroll();

        let start_line = rope.char_to_line(range.start().char_offset());

        let end_line = rope.char_to_line(range.end().char_offset().saturating_sub(1));

        let grapheme_area =
            |char_index| char_index_to_area(char_index, rope, vertical_scroll, area);

        let line_area = |line_index| line_index_to_area(line_index, rope, vertical_scroll, area);

        if range.is_empty() {
            if let Some(rect) = grapheme_area(range.head().char_offset()) {
                surface.set_style(rect, Style::default().bg(colors::RED));
            }
            return;
        }

        for (line_index, mut line_rect) in (start_line..=end_line)
            .filter_map(|line_index| line_area(line_index).map(|rect| (line_index, rect)))
        {
            if line_index == start_line {
                if let Some(start_rect) = grapheme_area(range.start().char_offset()) {
                    let delta = start_rect.x - line_rect.x;
                    line_rect.x += delta;
                    line_rect.width -= delta;
                } else {
                    // TODO: We continue here because we know the range start is off the screen to the
                    // right. Once horizontal scrolling is added, we'll need to handle when the range is
                    // off the screen to the left. `grapheme_area` doesn't say which direction the index
                    // is off screen.
                    continue;
                }
            }
            #[expect(clippy::collapsible_if)]
            if line_index == end_line {
                if let Some(end_rect) = grapheme_area(range.end().char_offset().saturating_sub(1)) {
                    let delta = line_rect.right() - end_rect.right();
                    line_rect.width -= delta;
                }
            }
            surface.set_style(line_rect, Style::default().bg(colors::LIGHT_YELLOW));
        }

        #[expect(clippy::collapsible_else_if)]
        if range.is_backward() {
            if let Some(rect) = grapheme_area(range.head().char_offset()) {
                surface.set_style(rect, Style::default().bg(colors::DARK_YELLOW));
            }
        } else {
            if let Some(rect) = grapheme_area(range.head().char_offset().saturating_sub(1)) {
                surface.set_style(rect, Style::default().bg(colors::DARK_YELLOW));
            }
        }
    }
}
