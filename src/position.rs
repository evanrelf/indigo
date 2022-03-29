use crate::{cursor::Cursor, range::Range};
use ropey::{Rope, RopeSlice};

pub fn cursor_to_index(rope: &Rope, cursor: &Cursor) -> Option<usize> {
    let bol_char_index = rope.try_line_to_char(cursor.line).ok()?;
    let line_length = rope.get_line(cursor.line)?.len_chars();
    if line_length < cursor.column {
        return None;
    }
    Some(bol_char_index + cursor.column)
}

#[allow(unused_variables)]
pub fn cursor_to_index_lossy(rope: &Rope, cursor: &Cursor) -> usize {
    todo!()
}

pub fn index_to_cursor(rope: &Rope, index: usize) -> Option<Cursor> {
    let line = rope.try_char_to_line(index).ok()?;
    let column = index - rope.try_line_to_char(line).ok()?;
    Some(Cursor {
        line,
        column,
        target_column: None,
    })
}

#[allow(unused_variables)]
pub fn index_to_cursor_lossy(rope: &Rope, index: usize) -> Cursor {
    todo!()
}

pub fn range_to_slice<'rope>(rope: &'rope Rope, range: &Range) -> Option<RopeSlice<'rope>> {
    let anchor_index = cursor_to_index(rope, &range.anchor)?;
    let head_index = cursor_to_index(rope, &range.head)?;
    if range.is_forwards() {
        rope.get_slice(anchor_index..=head_index)
    } else {
        rope.get_slice(head_index..=anchor_index)
    }
}

#[allow(unused_variables)]
pub fn range_to_slice_lossy<'rope>(rope: &'rope Rope, range: &Range) -> RopeSlice<'rope> {
    todo!()
}


// TODO: Delete
pub fn is_valid_cursor(rope: &Rope, cursor: &Cursor) -> bool {
    cursor_to_index(rope, cursor).is_some()
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_range_to_slice() {
        fn case(s: &str, range: ((usize, usize), (usize, usize)), expected: &str) {
            let rope = Rope::from_str(s);
            let range = Range::new(range.0, range.1);
            let expected = Some(expected);
            let actual = range_to_slice(&rope, &range).and_then(|slice| slice.as_str());
            assert!(
                expected == actual,
                "\nexpected = {:?}\nactual = {:?}\n",
                expected,
                actual
            );
        }

        case("Hello, world!", ((0, 0), (0, 4)), "Hello");
        case("Hello, world!", ((0, 7), (0, 11)), "world");
        case("Fizz\nBuzz", ((1, 0), (1, 3)), "Buzz");
        case("Fizz\nBuzz", ((0, 0), (1, 3)), "Fizz\nBuzz");
    }
}
