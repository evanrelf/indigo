use crate::{Conversion, Position};
use anyhow::Context as _;
use fancy_regex::Regex;
use ropey::{Rope, RopeSlice};
use std::{
    borrow::Cow,
    cmp::{max, min},
};

/// A directional range of positions in a rope.
#[derive(Clone, Copy, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
pub struct Range {
    anchor: Position,
    cursor: Position,
    target_column: Option<usize>,
}

impl Range {
    /// ```text
    /// ▒▒▒▒▒▒▒▒▒▒▒▒▒▒█
    /// ^
    ///
    /// █▒▒▒▒▒▒▒▒▒▒▒▒▒▒
    ///               ^
    /// ```
    #[must_use]
    pub fn anchor(&self) -> Position {
        self.anchor
    }

    /// ```text
    /// ▒▒▒▒▒▒▒▒▒▒▒▒▒▒█
    ///               ^
    ///
    /// █▒▒▒▒▒▒▒▒▒▒▒▒▒▒
    /// ^
    /// ```
    #[must_use]
    pub fn cursor(&self) -> Position {
        self.cursor
    }

    #[must_use]
    pub fn target_column(&self) -> Option<usize> {
        self.target_column
    }

    /// ```text
    /// ▒▒▒▒▒▒▒▒▒▒▒▒▒▒█
    /// ^
    ///
    /// █▒▒▒▒▒▒▒▒▒▒▒▒▒▒
    /// ^
    /// ```
    #[must_use]
    pub fn start(&self) -> Position {
        min(self.anchor, self.cursor)
    }

    /// ```text
    /// ▒▒▒▒▒▒▒▒▒▒▒▒▒▒█
    ///               ^
    ///
    /// █▒▒▒▒▒▒▒▒▒▒▒▒▒▒
    ///               ^
    /// ```
    #[must_use]
    pub fn end(&self) -> Position {
        max(self.anchor, self.cursor)
    }

    /// Whether the range is facing forward (the anchor is behind or overlapping with the cursor).
    ///
    /// ```text
    /// ▒▒▒▒▒▒▒▒▒▒▒▒▒▒█ -> true
    ///
    ///               █ -> true
    ///
    /// █▒▒▒▒▒▒▒▒▒▒▒▒▒▒ -> false
    /// ```
    #[must_use]
    pub fn is_forward(&self) -> bool {
        self.anchor <= self.cursor
    }

    /// Whether the range is facing backward (the anchor is ahead of the cursor).
    ///
    /// ```text
    /// ▒▒▒▒▒▒▒▒▒▒▒▒▒▒█ -> false
    ///
    ///               █ -> false
    ///
    /// █▒▒▒▒▒▒▒▒▒▒▒▒▒▒ -> true
    /// ```
    #[must_use]
    pub fn is_backward(&self) -> bool {
        self.anchor > self.cursor
    }

    /// Whether the range is reduced (the anchor and cursor are overlapping).
    ///
    /// ```text
    /// ▒▒▒▒▒▒▒▒▒▒▒▒▒▒█ -> false
    ///
    ///               █ -> true
    /// ```
    #[must_use]
    pub fn is_reduced(&self) -> bool {
        self.anchor == self.cursor
    }

    /// Whether two ranges are overlapping.
    ///
    /// ```text
    /// ▒▒▒▒▒▒▒▒█       -> true
    ///       ▒▒▒▒▒▒▒▒█
    ///
    /// ▒▒▒▒▒▒█
    ///          ▒▒▒▒▒█ -> false
    /// ```
    #[must_use]
    pub fn is_overlapping(&self, other: &Self) -> bool {
        self.start() <= other.end() && other.start() <= self.end()
    }

    pub fn set_target_column(&mut self, target_column: Option<usize>) -> anyhow::Result<()> {
        if let Some(target_column) = target_column {
            if self.cursor.column >= target_column {
                anyhow::bail!("Target column must be greater than cursor column");
            }
        }
        self.target_column = target_column;
        Ok(())
    }

    /// Swap the anchor and the cursor, reversing the range's direction.
    ///
    /// ```text
    /// ▒▒▒▒▒▒▒▒▒▒▒▒▒▒█ -> █▒▒▒▒▒▒▒▒▒▒▒▒▒▒ -> ▒▒▒▒▒▒▒▒▒▒▒▒▒▒█
    /// ```
    pub fn flip(&mut self) {
        if !self.is_reduced() {
            std::mem::swap(&mut self.anchor, &mut self.cursor);
            self.target_column = None;
        }
    }

    /// If the cursor is behind the anchor, swap the two.
    ///
    /// ```text
    /// █▒▒▒▒▒▒▒▒▒▒▒▒▒▒ -> ▒▒▒▒▒▒▒▒▒▒▒▒▒▒█ -> ▒▒▒▒▒▒▒▒▒▒▒▒▒▒█
    /// ```
    pub fn flip_forward(&mut self) {
        if self.is_backward() {
            self.flip();
        }
    }

    /// If the anchor is behind the cursor, swap the two.
    ///
    /// ```text
    /// ▒▒▒▒▒▒▒▒▒▒▒▒▒▒█ -> █▒▒▒▒▒▒▒▒▒▒▒▒▒▒ -> █▒▒▒▒▒▒▒▒▒▒▒▒▒▒
    /// ```
    pub fn flip_backward(&mut self) {
        if self.is_forward() {
            self.flip();
        }
    }

    /// Move the anchor to the cursor's position, collapsing the range down to a single position.
    ///
    /// ```text
    /// ▒▒▒▒▒▒▒▒▒▒▒▒▒▒█ ->               █
    ///
    /// █▒▒▒▒▒▒▒▒▒▒▒▒▒▒ -> █
    /// ```
    pub fn reduce(&mut self) {
        self.anchor = self.cursor;
    }

    /// ```text
    /// ▒▒▒▒▒▒▒▒█       -> ▒▒▒▒▒▒▒▒▒▒▒▒▒▒█
    ///       ▒▒▒▒▒▒▒▒█
    ///
    /// ▒▒▒▒▒▒█         -> ▒▒▒▒▒▒▒▒▒▒▒▒▒▒█
    ///          ▒▒▒▒▒█
    /// ```
    pub fn merge(&mut self, other: &Self) {
        match (self.is_forward(), other.is_forward()) {
            (true, true) => {
                // Forward
                self.anchor = min(self.anchor, other.anchor);
                if self.cursor < other.cursor {
                    self.cursor = other.cursor;
                    self.target_column = other.target_column;
                }
            }
            (false, false) => {
                // Backward
                self.anchor = max(self.anchor, other.anchor);
                if self.cursor > other.cursor {
                    self.cursor = other.cursor;
                    self.target_column = other.target_column;
                }
            }
            _ => {
                // Mixed
                let mut other = *other;
                other.flip();
                self.merge(&other);
            }
        }
    }

    /// ```text
    ///   Hello, world!                Hello world!
    ///   ▒▒▒▒▒▒▒▒▒▒▒▒█ --- Hello ---> ▒▒▒▒█
    ///
    ///   Hello, world!                Hello world!
    ///   ▒▒▒▒▒▒▒▒▒▒▒▒█ -- Goodbye -->
    ///
    /// [foo, bar, baz]                [foo, bar, baz]
    /// █▒▒▒▒▒▒▒▒▒▒▒▒▒▒ ---- \w+ ---->  █▒▒  █▒▒  █▒▒
    /// ```
    #[must_use]
    pub fn select(&self, rope: &Rope, regex: &Regex) -> Option<Vec<Self>> {
        // TODO: Check if this preserves the range's direction correctly, like the third example in
        // the docs illustrates.

        let offset = self.start().to_char_index(rope).ok()?;

        let rope_slice = self.to_rope_slice(rope)?;

        let string_slice = Cow::<str>::from(rope_slice);

        let ranges = regex
            .find_iter(&string_slice)
            .map(|match_| {
                let match_ = match_.unwrap();

                let start_index = offset + rope.byte_to_char(match_.start());
                let start_position = Position::from_char_index(start_index, rope).unwrap();

                let end_index = offset + rope.byte_to_char(match_.end()).saturating_sub(1);
                let end_position = Position::from_char_index(end_index, rope).unwrap();

                if self.is_forward() {
                    Self::from((start_position, end_position))
                } else {
                    Self::from((end_position, start_position))
                }
            })
            .collect();

        Some(ranges)
    }

    pub fn insert_char(&self, c: char, rope: &mut Rope) -> anyhow::Result<Self> {
        let anchor_index = self.anchor.to_char_index(rope)?;
        let cursor_index = self.cursor.to_char_index(rope)?;

        let mut new_rope = rope.clone();

        self.start().insert_char(c, &mut new_rope)?;

        let mut new_range = *self;

        new_range.anchor = Position::from_char_index_strict(anchor_index + 1, &new_rope)
            .ok()
            .and_then(Conversion::valid)
            .expect("position is valid because it comes from a known valid index");

        new_range.cursor = Position::from_char_index_strict(cursor_index + 1, &new_rope)
            .ok()
            .and_then(Conversion::valid)
            .expect("position is valid because it comes from a known valid index");

        *rope = new_rope;

        Ok(new_range)
    }

    pub fn insert(&self, s: &str, rope: &mut Rope) -> anyhow::Result<Self> {
        let Ok(Conversion::Valid(anchor_index)) = self.anchor.to_char_index_strict(rope) else {
            anyhow::bail!("Anchor position is invalid");
        };
        let Ok(Conversion::Valid(cursor_index)) = self.cursor.to_char_index_strict(rope) else {
            anyhow::bail!("Cursor position is invalid");
        };

        let mut new_rope = rope.clone();

        self.start()
            .insert(s, &mut new_rope)
            .context("Failed to insert at start of range")?;

        let mut new_range = *self;

        new_range.anchor = Position::from_char_index_strict(anchor_index + s.len(), &new_rope)
            .ok()
            .and_then(Conversion::valid)
            .expect("position is valid because it comes from a known valid index");

        new_range.cursor = Position::from_char_index_strict(cursor_index + s.len(), &new_rope)
            .ok()
            .and_then(Conversion::valid)
            .expect("position is valid because it comes from a known valid index");

        *rope = new_rope;

        Ok(new_range)
    }

    #[must_use]
    pub fn to_rope_slice<'rope>(&self, rope: &'rope Rope) -> Option<RopeSlice<'rope>> {
        let anchor_index = self.anchor.to_char_index(rope).ok()?;
        let cursor_index = self.cursor.to_char_index(rope).ok()?;

        let slice = if self.is_forward() {
            rope.get_slice(anchor_index..=cursor_index)?
        } else {
            rope.get_slice(cursor_index..=anchor_index)?
        };

        Some(slice)
    }

    /// Check that the type's invariants hold.
    pub fn assert_valid(&self) {
        self.anchor.assert_valid();

        self.cursor.assert_valid();

        if let Some(target_column) = self.target_column {
            assert!(
                target_column > self.cursor.column,
                "target column must be greater than cursor column"
            );
        }
    }
}

impl From<Position> for Range {
    fn from(position: Position) -> Self {
        Self {
            anchor: position,
            cursor: position,
            target_column: None,
        }
    }
}

impl From<(Position, Position)> for Range {
    fn from((anchor, cursor): (Position, Position)) -> Self {
        Self {
            anchor,
            cursor,
            target_column: None,
        }
    }
}

impl From<Range> for (Position, Position) {
    fn from(range: Range) -> Self {
        (range.anchor, range.cursor)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck_macros::quickcheck;

    #[test]
    fn range_default() {
        assert_eq!(
            Range::default(),
            Range {
                anchor: Position::from((0, 0)),
                cursor: Position::from((0, 0)),
                target_column: None,
            }
        );
    }

    #[quickcheck]
    fn range_start(line1: usize, column1: usize, line2: usize, column2: usize) -> bool {
        let anchor = Position::from((line1, column1));
        let cursor = Position::from((line2, column2));
        let r1 = Range::from((anchor, cursor));
        let mut r2 = r1;
        r2.flip();
        r1.start() == r2.start()
    }

    #[quickcheck]
    fn range_end(line1: usize, column1: usize, line2: usize, column2: usize) -> bool {
        let anchor = Position::from((line1, column1));
        let cursor = Position::from((line2, column2));
        let r1 = Range::from((anchor, cursor));
        let mut r2 = r1;
        r2.flip();
        r1.end() == r2.end()
    }
}
