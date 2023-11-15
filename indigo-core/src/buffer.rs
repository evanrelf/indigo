use crate::{RopeExt as _, Selection};
use ropey::Rope;
use std::{cell::Cell, cmp::min};

#[derive(Clone, Debug)]
pub struct Buffer {
    contents: Rope,
    selection: Selection,
    prev_vertical_scroll: Cell<usize>,
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
        // Just-in-time calculation of vertical scroll offset. Implicitly compensates for changes in
        // rope length, without needing to track rope modifications explicitly, so long as this
        // method is the only way of getting this value.
        //
        // This is the only place `self.prev_vertical_scroll.get()` is allowed.
        let prev_scroll = self.prev_vertical_scroll.get();
        let max_scroll = self.contents.len_lines_indigo().saturating_sub(1);
        let vertical_scroll = min(prev_scroll, max_scroll);
        self.prev_vertical_scroll.set(vertical_scroll);
        vertical_scroll
    }

    #[must_use]
    pub fn horizontal_scroll(&self) -> usize {
        self.horizontal_scroll
    }

    pub fn scroll_up(&mut self, distance: usize) {
        self.scroll_to_line(self.vertical_scroll().saturating_sub(distance));
    }

    pub fn scroll_down(&mut self, distance: usize) {
        self.scroll_to_line(self.vertical_scroll() + distance);
    }

    pub fn scroll_left(&mut self, distance: usize) {
        self.scroll_to_column(self.horizontal_scroll.saturating_sub(distance));
    }

    pub fn scroll_right(&mut self, distance: usize) {
        self.scroll_to_column(self.horizontal_scroll + distance);
    }

    pub fn scroll_to_line(&mut self, line: usize) {
        self.prev_vertical_scroll.set(line);
    }

    pub fn scroll_to_column(&mut self, column: usize) {
        self.horizontal_scroll = column;
    }

    pub fn scroll_to_selection(&mut self, area_height: u16) {
        let line = self.selection.primary().cursor().line;

        let top = self.vertical_scroll();
        let bottom = (top + usize::try_from(area_height).unwrap()) - 1;

        if line < top {
            self.prev_vertical_scroll.set(line);
        } else if line > bottom {
            self.prev_vertical_scroll.set(top + (line - bottom));
        };
    }

    pub fn assert_valid(&self) {
        assert!(
            self.contents.len_chars() > 0,
            "`contents` rope is not empty"
        );

        assert!(
            self.vertical_scroll() < self.contents.len_lines_indigo(),
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
            prev_vertical_scroll: Cell::new(0),
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
