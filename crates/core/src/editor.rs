use crate::{mode::Mode, range::Range, range::RangeMut, range::RawRange, rope::RopeExt as _};
use camino::Utf8PathBuf;
use ropey::Rope;
use std::{cmp::min, fs::File, io::BufReader};

pub struct Editor {
    text: Rope,
    range: RawRange,
    pub mode: Mode,
    pub height: usize,
    vertical_scroll: usize,
    pub quit: bool,
}

impl Editor {
    pub fn new(path: Utf8PathBuf) -> anyhow::Result<Self> {
        let file = File::open(path)?;
        let rope = Rope::from_reader(BufReader::new(file))?;
        Ok(Self {
            text: rope,
            range: RawRange::default(),
            mode: Mode::Normal { count: 0 },
            height: 0,
            vertical_scroll: 0,
            quit: false,
        })
    }

    #[must_use]
    pub fn text(&self) -> &Rope {
        &self.text
    }

    /// DOES NOT SAVE CHANGES FROM THE RANGE TO THE EDITOR.
    #[must_use]
    pub fn range(&self) -> Range {
        Range::new(&self.text, self.range.anchor, self.range.head)
            .expect("Editor range always valid")
    }

    /// DOES NOT SAVE CHANGES FROM THE RANGE TO THE EDITOR.
    #[must_use]
    pub fn range_mut(&mut self) -> RangeMut {
        RangeMut::new(&mut self.text, self.range.anchor, self.range.head)
            .expect("Editor range always valid")
    }

    pub fn with_range<T>(&mut self, func: impl Fn(&mut Range) -> T) -> T {
        let mut range = self.range();
        let result = func(&mut range);
        self.range = range.into_raw();
        result
    }

    pub fn with_range_mut<T>(&mut self, func: impl Fn(&mut RangeMut) -> T) -> T {
        let mut range = self.range_mut();
        let result = func(&mut range);
        self.range = range.into_raw();
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
