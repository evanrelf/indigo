use std::{cmp::min, ops::Range};

pub const UNITS_PER_CELL: usize = 8;

const MIN_UNITS: usize = 8;

/// Position of the scroll bar thumb within the track, measured in eighths of a cell.
#[must_use]
pub fn thumb(
    text_lines: usize,
    window_lines: usize,
    vertical_scroll: usize,
    track_cells: usize,
) -> Option<Range<usize>> {
    let track_units = track_cells * UNITS_PER_CELL;
    let thumb_units = thumb_units(text_lines, window_lines, track_units)?;

    let max_scroll = text_lines.saturating_sub(1);

    let thumb_start_units = if max_scroll == 0 || track_units == thumb_units {
        0
    } else {
        ((track_units - thumb_units) * vertical_scroll) / max_scroll
    };
    let thumb_end_units = thumb_start_units + thumb_units;

    Some(thumb_start_units..thumb_end_units)
}

/// Smallest vertical scroll which moves the thumb's start to `thumb_start` (measured in eighths of
/// a cell) or below it.
#[must_use]
pub fn vertical_scroll(
    text_lines: usize,
    window_lines: usize,
    thumb_start: usize,
    track_cells: usize,
) -> Option<usize> {
    let track_units = track_cells * UNITS_PER_CELL;
    let thumb_units = thumb_units(text_lines, window_lines, track_units)?;

    let max_scroll = text_lines.saturating_sub(1);

    if max_scroll == 0 || track_units == thumb_units {
        return Some(0);
    }

    let thumb_start = min(thumb_start, track_units - thumb_units);

    Some((thumb_start * max_scroll).div_ceil(track_units - thumb_units))
}

fn thumb_units(text_lines: usize, window_lines: usize, track_units: usize) -> Option<usize> {
    let scroll_lines = text_lines.saturating_add(window_lines.saturating_sub(1));

    if track_units == 0 || scroll_lines == 0 {
        return None;
    }

    Some(((track_units * window_lines) / scroll_lines).clamp(MIN_UNITS, track_units))
}
