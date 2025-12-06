use indigo_core::{prelude::*, rope::LINE_TYPE};
use ratatui::layout::{Constraint, Layout, Position, Rect};
use std::cmp::max;

#[derive(Clone, Copy, Default)]
pub struct Areas {
    pub status_bar: Rect,
    pub line_numbers: Rect,
    pub dots: Rect,
    pub text: Rect,
    pub scroll_bar: Rect,
}

impl Areas {
    #[must_use]
    pub fn new(editor: &Editor, area: Rect) -> Self {
        let [status_bar, main] = Layout::vertical([
            // status_bar
            Constraint::Length(1),
            // line_numbers + text
            Constraint::Fill(1),
        ])
        .areas(area);

        let line_numbers_width = {
            let n = editor.window().buffer().rope().len_lines_indigo();
            let digits = 1 + max(1, n).ilog10();
            u16::try_from(max(2, digits) + 1)
                .expect("Line number width should always be very small")
        };

        let [line_numbers, text, scroll_bar] = Layout::horizontal([
            // line_numbers
            Constraint::Length(line_numbers_width),
            // text
            Constraint::Fill(1),
            // scroll_bar
            Constraint::Length(1),
        ])
        .areas(main);

        Self {
            status_bar,
            line_numbers,
            dots: main,
            text,
            scroll_bar,
        }
    }
}

/// Map position on the terminal to a character offset in the rope. Example use is moving a
/// cursor to where a mouse was clicked.
///
/// `None` means the position was not contained within the area. `Some(Ok(_))` means the position
/// was valid in the rope. `Some(Err(_))` means the position was not valid in the rope, but we were
/// able to correct it.
///
/// Examples of corrections: snapping to the beginning of the grapheme, snapping to the end of the
/// line, and snapping to the end of the buffer.
#[must_use]
pub fn position_to_byte_offset(
    position: Position,
    rope: &Rope,
    vertical_scroll: usize,
    // TODO(horizontal_scroll)
    area: Rect,
) -> Option<Result<usize, usize>> {
    // TODO: Move this general purpose (x, y) <-> index logic somewhere else.

    if !area.contains(position) {
        return None;
    }

    // TODO(horizontal_scroll)
    let x = usize::from(position.x - area.x);

    let y = usize::from(position.y - area.y) + vertical_scroll;

    let Some(line) = rope.get_line(y, LINE_TYPE) else {
        // Position goes beyond last line of rope, so we snap to last character of rope
        return Some(Err(rope.len()));
    };

    let line_byte_offset = rope.line_to_byte_idx(y, LINE_TYPE);

    let mut line_prefix = 0;
    let mut byte_offset = line_byte_offset;

    for grapheme in line.graphemes() {
        if grapheme.chars().any(|c| c == '\n' || c == '\r') {
            break;
        }

        let grapheme_width = grapheme.display_width();

        if line_prefix + grapheme_width > x {
            return Some(Ok(byte_offset));
        }

        line_prefix += grapheme_width;
        byte_offset += grapheme.len();
    }

    Some(Err(byte_offset))
}

#[must_use]
pub fn line_index_to_area(
    line_index: usize,
    rope: &Rope,
    vertical_scroll: usize,
    // TODO(horizontal_scroll)
    area: Rect,
) -> Option<Rect> {
    if vertical_scroll > line_index {
        return None;
    }

    let y = area.y
        + u16::try_from(line_index - vertical_scroll)
            .expect("Terminal height is less than u16::MAX");

    if !(area.top()..area.bottom()).contains(&y) {
        return None;
    }

    // TODO(horizontal_scroll)
    let x = area.x;

    let line = rope.get_line(line_index, LINE_TYPE)?;

    // Assumes a minimum grapheme width of 1
    let width = if line.len() >= usize::from(area.width) {
        // Avoid expensive display width calculation if we know it would exceed the viewport width
        area.width
    } else {
        u16::try_from(line.display_width()).expect("Terminal width is less than u16::MAX")
    };

    Some(Rect {
        x,
        y,
        width,
        height: 1,
    })
}

#[must_use]
pub fn byte_index_to_area(
    byte_index: usize,
    rope: &Rope,
    vertical_scroll: usize,
    // TODO(horizontal_scroll)
    area: Rect,
) -> Option<Rect> {
    if byte_index > rope.len() {
        return None;
    }

    let line_index = rope.byte_to_line_idx(byte_index, LINE_TYPE);

    if vertical_scroll > line_index {
        return None;
    }

    let y = area.y
        + u16::try_from(line_index - vertical_scroll)
            .expect("Terminal height is less than u16::MAX");

    if !(area.top()..area.bottom()).contains(&y) {
        return None;
    }

    let line_byte_index = rope.line_to_byte_idx(line_index, LINE_TYPE);

    let byte_index = rope.floor_grapheme_boundary(byte_index);

    let prefix_width = rope.slice(line_byte_index..byte_index).display_width();

    // TODO(horizontal_scroll): When horizontal scroll is introduced, still return portion of rect
    // that is visible. Even if it starts to the left of the area, it might be wide enough to peek
    // into the viewport.
    let x = area.x + u16::try_from(prefix_width).unwrap();

    if !(area.left()..area.right()).contains(&x) {
        return None;
    }

    let width = if rope.len() == byte_index {
        // Cursor at end of text
        1
    } else if let Some(grapheme) = rope.get_grapheme(byte_index) {
        u16::try_from(grapheme.display_width())
            .expect("No grapheme exists with a display width > u16::MAX")
    } else {
        // We're at end of text, but we already checked for that
        unreachable!()
    };

    Some(Rect {
        x,
        y,
        width,
        height: 1,
    })
}
