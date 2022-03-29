use crate::{cursor::Cursor, range::Range};
use ropey::Rope;

// TODO: Delete
pub fn is_valid_cursor(rope: &Rope, cursor: &Cursor) -> bool {
    cursor.to_rope_index(rope).is_some()
}

// TODO: Delete
pub fn corrected_cursor(rope: &Rope, cursor: &Cursor) -> Option<Cursor> {
    let line_length = rope.get_line(cursor.line)?.len_chars();
    if line_length == 0 {
        return None;
    }

    #[allow(clippy::collapsible_else_if)]
    if let Some(target_column) = cursor.target_column {
        if line_length > target_column {
            Some(Cursor {
                column: target_column,
                target_column: None,
                ..cursor.clone()
            })
        } else {
            Some(Cursor {
                column: line_length - 1,
                ..cursor.clone()
            })
        }
    } else {
        if line_length > cursor.column {
            Some(cursor.clone())
        } else {
            Some(Cursor {
                column: line_length - 1,
                target_column: Some(cursor.column),
                ..cursor.clone()
            })
        }
    }
}

// TODO: Delete
pub fn corrected_range(rope: &Rope, range: &Range) -> Option<Range> {
    let anchor = corrected_cursor(rope, &range.anchor)?;
    let head = corrected_cursor(rope, &range.head)?;
    Some(Range { anchor, head })
}
