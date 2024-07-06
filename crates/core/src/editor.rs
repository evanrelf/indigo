use crate::range::RangeState;
use crate::{Cursor, CursorMut, Range, RangeMut, RopeExt as _};
use camino::Utf8PathBuf;
use ropey::Rope;
use std::{cmp::min, fs::File, io::BufReader};

pub struct Editor {
    text: Rope,
    range: RangeState,
    vertical_scroll: usize,
}

impl Editor {
    pub fn new(path: Utf8PathBuf) -> anyhow::Result<Self> {
        let file = File::open(path)?;
        let rope = Rope::from_reader(BufReader::new(file))?;
        Ok(Self {
            text: rope,
            range: RangeState::default(),
            vertical_scroll: 0,
        })
    }

    #[must_use]
    pub fn text(&self) -> &Rope {
        &self.text
    }

    #[must_use]
    pub fn range(&self) -> Range {
        Range {
            rope: &self.text,
            state: self.range,
        }
    }

    #[must_use]
    pub fn range_mut(&mut self) -> RangeMut {
        RangeMut {
            rope: &mut self.text,
            state: self.range,
        }
    }

    #[must_use]
    pub fn cursor_at(&self, char_index: usize) -> Option<Cursor> {
        Cursor::from_char_index(&self.text, char_index)
    }

    #[must_use]
    pub fn cursor_mut_at(&mut self, char_index: usize) -> Option<CursorMut> {
        CursorMut::from_char_index(&mut self.text, char_index)
    }

    pub fn with_range<T>(&mut self, func: impl Fn(&mut RangeMut) -> T) -> T {
        let mut range = self.range_mut();
        let result = func(&mut range);
        self.range = range.state;
        result
    }

    #[must_use]
    pub fn vertical_scroll(&self) -> usize {
        self.vertical_scroll
    }

    pub fn scroll_to(&mut self, line: usize) {
        let last_line = self.text().len_lines_indigo().saturating_sub(1);
        self.vertical_scroll = min(line, last_line);
    }
}
