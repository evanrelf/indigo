use indigo_core::prelude::*;
use ratatui::layout::{Constraint, Layout, Position, Rect};
use std::cmp::max;

#[derive(Clone, Copy, Default)]
pub struct Areas {
    pub navigation_bar: Rect,
    pub line_numbers: Rect,
    pub text: Rect,
    pub status_bar: Rect,
}

impl Areas {
    pub fn new(editor: &Editor, area: Rect) -> Self {
        let vertical_areas = Layout::vertical([
            // navigation_bar
            Constraint::Length(1),
            // line_numbers + text
            Constraint::Fill(1),
            // status_bar
            Constraint::Length(1),
        ])
        .split(area);

        let line_numbers_width = {
            let n = editor.rope().len_lines_indigo();
            let digits = 1 + max(1, n).ilog10();
            u16::try_from(max(2, digits) + 1)
                .expect("Line number width should always be very small")
        };

        let navigation_bar = vertical_areas[0];

        let horizontal_areas = Layout::horizontal([
            // line_numbers
            Constraint::Length(line_numbers_width),
            // text
            Constraint::Fill(1),
        ])
        .split(vertical_areas[1]);

        let status_bar = vertical_areas[2];

        let line_numbers = horizontal_areas[0];

        let text = horizontal_areas[1];

        Self {
            navigation_bar,
            line_numbers,
            text,
            status_bar,
        }
    }
}

/// Map position on the terminal to a character index in the rope indices. Example us is moving a
/// cursor to where a mouse was clicked.
///
/// `None` means the position was not contained within the area. `Some(Ok(_))` means the position
/// was valid in the rope. `Some(Err(_))` means the position was not valid in the rope, but we were
/// able to correct it.
///
/// Examples of corrections: snapping to the beginning of the grapheme, snapping to the end of the
/// line, and snapping to the end of the buffer.
pub fn position_to_char_index(
    position: Position,
    rope: &Rope,
    vertical_scroll: usize,
    area: Rect,
) -> Option<Result<usize, usize>> {
    // TODO: Move this general purpose (x, y) <-> index logic somewhere else.

    if !area.contains(position) {
        return None;
    }

    let x = usize::from(position.x - area.x);

    let y = usize::from(position.y - area.y) + vertical_scroll;

    let Some(line) = rope.get_line(y) else {
        // Position goes beyond last line of rope, so we snap to last character of rope
        return Some(Err(rope.len_chars()));
    };

    let line_char_index = rope
        .try_line_to_char(y)
        .expect("Line is known to exist at this point");

    let line_length = line.len_chars();

    if x > line_length {
        // Position goes beyond last character of line, so we snap to last character of line
        return Some(Err(line_char_index + (line_length - 1)));
    }

    Some(Ok(line_char_index + x))
}
