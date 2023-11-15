use crate::{RopeExt as _, Selection};
use ropey::Rope;
use std::cmp::min;

// TODO: `Buffer` becomes invalid if `Rope` is truncated and `vertical_scroll` goes beyond last line

#[derive(Clone, Debug)]
pub struct Buffer {
    contents: Rope,
    selection: Selection,
    vertical_scroll: usize,
    horizontal_scroll: usize,
}

impl Buffer {
    #[must_use]
    pub fn contents(&self) -> &Rope {
        &self.contents
    }

    #[must_use]
    pub fn selection(&self) -> &Selection {
        &self.selection
    }

    #[must_use]
    pub fn vertical_scroll(&self) -> usize {
        self.vertical_scroll
    }

    #[must_use]
    pub fn horizontal_scroll(&self) -> usize {
        self.horizontal_scroll
    }

    pub fn scroll_up(&mut self, distance: usize) {
        self.scroll_to_line(self.vertical_scroll.saturating_sub(distance));
    }

    pub fn scroll_down(&mut self, distance: usize) {
        self.scroll_to_line(self.vertical_scroll + distance);
    }

    pub fn scroll_left(&mut self, distance: usize) {
        self.scroll_to_column(self.horizontal_scroll.saturating_sub(distance));
    }

    pub fn scroll_right(&mut self, distance: usize) {
        self.scroll_to_column(self.horizontal_scroll + distance);
    }

    pub fn scroll_to_line(&mut self, line: usize) {
        let last_line = self.contents.len_lines_indigo().saturating_sub(1);
        self.vertical_scroll = min(line, last_line);
    }

    pub fn scroll_to_column(&mut self, column: usize) {
        self.horizontal_scroll = column;
    }

    pub fn scroll_to_selection(&mut self, area_height: u16) {
        let line = self.selection.primary().cursor().line;

        let top = self.vertical_scroll;
        let bottom = (top + usize::try_from(area_height).unwrap()) - 1;

        if line < top {
            self.vertical_scroll = line;
        } else if line > bottom {
            self.vertical_scroll = top + (line - bottom);
        };
    }

    pub fn assert_valid(&self) {
        assert!(
            self.contents.len_chars() > 0,
            "`contents` rope is not empty"
        );

        assert!(
            self.vertical_scroll < self.contents.len_lines_indigo(),
            "`vertical_scroll` doesn't go beyond the last line of the `contents` rope"
        );

        // TODO: `selection` is valid in `contents` rope

        // TODO: Buffer validation

        self.selection.assert_valid();
    }
}

impl Default for Buffer {
    fn default() -> Self {
        Self {
            contents: Rope::from("\n"),
            selection: Selection::default(),
            vertical_scroll: 0,
            horizontal_scroll: 0,
        }
    }
}

impl TryFrom<Rope> for Buffer {
    type Error = anyhow::Error;

    fn try_from(rope: Rope) -> Result<Self, Self::Error> {
        if rope.len_chars() == 0 {
            anyhow::bail!("Buffer cannot be empty");
        }
        Ok(Self {
            contents: rope,
            ..Self::default()
        })
    }
}

impl TryFrom<String> for Buffer {
    type Error = anyhow::Error;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        Self::try_from(Rope::from(s))
    }
}

impl TryFrom<&str> for Buffer {
    type Error = anyhow::Error;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        Self::try_from(Rope::from(s))
    }
}
