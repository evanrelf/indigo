use crate::{range::RangeState, Mode, Range, RangeMut, RopeExt as _};
use camino::Utf8PathBuf;
use ropey::Rope;
use std::{cmp::min, fs::File, io::BufReader};

pub struct Editor {
    text: Rope,
    range: RangeState,
    mode: Mode,
    pub count: usize,
    vertical_scroll: usize,
}

impl Editor {
    pub fn new(path: Utf8PathBuf) -> anyhow::Result<Self> {
        let file = File::open(path)?;
        let rope = Rope::from_reader(BufReader::new(file))?;
        Ok(Self {
            text: rope,
            range: RangeState::default(),
            mode: Mode::default(),
            count: 0,
            vertical_scroll: 0,
        })
    }

    #[must_use]
    pub fn text(&self) -> &Rope {
        &self.text
    }

    #[must_use]
    pub fn range(&self) -> Range {
        Range::from_state(&self.text, self.range).unwrap()
    }

    #[must_use]
    pub fn range_mut(&mut self) -> RangeMut {
        RangeMut::from_state(&mut self.text, self.range).unwrap()
    }

    #[must_use]
    pub fn mode(&self) -> &Mode {
        &self.mode
    }

    #[must_use]
    pub fn mode_mut(&mut self) -> &mut Mode {
        &mut self.mode
    }

    pub fn with_range<T>(&mut self, func: impl Fn(&mut Range) -> T) -> T {
        let mut range = self.range();
        let result = func(&mut range);
        self.range = range.into_state();
        self.count = 0;
        result
    }

    pub fn with_range_mut<T>(&mut self, func: impl Fn(&mut RangeMut) -> T) -> T {
        let mut range = self.range_mut();
        let result = func(&mut range);
        self.range = range.into_state();
        self.count = 0;
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
