use indigo_core::prelude::*;
use ratatui::layout::{Constraint, Layout, Position, Rect};
use std::cmp::max;

#[derive(Clone, Copy, Default)]
pub struct Areas {
    pub navigation_bar: Rect,
    pub line_numbers: Rect,
    pub text: Rect,
    pub command_bar: Rect,
    pub status_bar: Rect,
}

impl Areas {
    #[tracing::instrument(skip_all)]
    pub fn new(editor: &Editor, area: Rect) -> Self {
        let (command_bar_height, status_bar_height) = match editor.mode {
            Mode::Normal(_) | Mode::Insert(_) => (0, 1),
            Mode::Command(_) => (1, 0),
        };

        let vertical_areas = Layout::vertical([
            // navigation_bar
            Constraint::Length(1),
            // line_numbers + text
            Constraint::Fill(1),
            // command_bar
            Constraint::Length(command_bar_height),
            // status_bar
            Constraint::Length(status_bar_height),
        ])
        .split(area);

        let line_numbers_width = {
            let n = editor.buffer.text().len_lines_indigo();
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

        let command_bar = vertical_areas[2];

        let status_bar = vertical_areas[3];

        let line_numbers = horizontal_areas[0];

        let text = horizontal_areas[1];

        Self {
            navigation_bar,
            line_numbers,
            text,
            command_bar,
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
#[tracing::instrument(skip_all)]
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

#[tracing::instrument(skip_all)]
pub fn line_index_to_area(
    line_index: usize,
    rope: &Rope,
    vertical_scroll: usize,
    area: Rect,
) -> Option<Rect> {
    if vertical_scroll > line_index {
        return None;
    }

    let y = area.y + u16::try_from(line_index - vertical_scroll).unwrap();

    if !(area.top()..area.bottom()).contains(&y) {
        return None;
    }

    let x = area.x;

    let line = rope.get_line(line_index)?;

    // Assumes a minimum grapheme width of 1
    let width = if line.len_chars() >= usize::from(area.width) {
        // Avoid expensive display width calculation if we know it would exceed the viewport width
        area.width
    } else {
        u16::try_from(line.display_width()).unwrap()
    };

    Some(Rect {
        x,
        y,
        width,
        height: 1,
    })
}

// TODO: Should this be `gap_index` instead of `char_index`?
#[tracing::instrument(skip_all)]
pub fn char_index_to_area(
    char_index: usize,
    rope: &Rope,
    vertical_scroll: usize,
    area: Rect,
) -> Option<Rect> {
    let line_index = rope.try_char_to_line(char_index).ok()?;

    if vertical_scroll > line_index {
        return None;
    }

    let y = area.y + u16::try_from(line_index - vertical_scroll).unwrap();

    if !(area.top()..area.bottom()).contains(&y) {
        return None;
    }

    let line_char_index = rope.line_to_char(line_index);

    let char_index = rope.floor_grapheme_boundary(char_index);

    let prefix_width = rope.slice(line_char_index..char_index).display_width();

    // TODO: When horizontal scroll is introduced, still return portion of rect that is visible.
    // Even if it starts to the left of the area, it might be wide enough to peek into the viewport.
    let x = area.x + u16::try_from(prefix_width).unwrap();

    if !(area.left()..area.right()).contains(&x) {
        return None;
    }

    let width = if rope.len_chars() == char_index {
        // Cursor at EOF
        1
    } else if let Some(grapheme) = rope.get_grapheme(char_index) {
        u16::try_from(grapheme.display_width()).unwrap()
    } else {
        // We're at EOF, but we already checked for that
        unreachable!()
    };

    Some(Rect {
        x,
        y,
        width,
        height: 1,
    })
}
