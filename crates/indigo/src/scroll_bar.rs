use std::ops::Range;

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
    let scroll_lines = text_lines.saturating_add(window_lines.saturating_sub(1));
    let track_units = track_cells * UNITS_PER_CELL;

    if track_units == 0 || scroll_lines == 0 {
        return None;
    }

    let thumb_units = ((track_units * window_lines) / scroll_lines).clamp(MIN_UNITS, track_units);

    let max_scroll = text_lines.saturating_sub(1);

    let thumb_start_units = if max_scroll == 0 || track_units == thumb_units {
        0
    } else {
        ((track_units - thumb_units) * vertical_scroll) / max_scroll
    };
    let thumb_end_units = thumb_start_units + thumb_units;

    Some(thumb_start_units..thumb_end_units)
}
