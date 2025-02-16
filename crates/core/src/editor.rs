use crate::{
    mode::{Mode, NormalMode},
    range::{Range, RangeMut, RawRange},
    rope::RopeExt as _,
};
use camino::Utf8PathBuf;
use ropey::Rope;
use std::{cmp::min, fs::File, io::BufReader};

pub struct Editor {
    rope: Rope,
    range: RawRange,
    pub mode: Mode,
    pub height: usize,
    vertical_scroll: usize,
    pub quit: bool,
}

impl Editor {
    pub fn new(path: Option<Utf8PathBuf>) -> anyhow::Result<Self> {
        let rope = if let Some(path) = path {
            let file = File::open(path)?;
            Rope::from_reader(BufReader::new(file))?
        } else {
            Rope::new()
        };
        let range = RawRange::new(&rope, 0, 0);
        Ok(Self {
            rope,
            range,
            mode: Mode::Normal(NormalMode { count: 0 }),
            height: 0,
            vertical_scroll: 0,
            quit: false,
        })
    }

    #[must_use]
    pub fn rope(&self) -> &Rope {
        &self.rope
    }

    #[must_use]
    pub fn range(&self) -> &RawRange {
        &self.range
    }

    pub fn with_range<T>(&mut self, func: impl Fn(&mut Range) -> T) -> T {
        let mut range = Range::new(
            &self.rope,
            self.range.anchor.gap_index,
            self.range.head.gap_index,
        );
        assert!(range.is_valid(), "Editor range was invalid");
        let result = func(&mut range);
        assert!(range.is_valid(), "Editor range was made invalid");
        self.range = range.into_raw();
        result
    }

    pub fn with_range_mut<T>(&mut self, func: impl Fn(&mut RangeMut) -> T) -> T {
        let mut range = RangeMut::new(
            &mut self.rope,
            self.range.anchor.gap_index,
            self.range.head.gap_index,
        );
        assert!(range.is_valid(), "Editor range was invalid");
        let result = func(&mut range);
        assert!(range.is_valid(), "Editor range was made invalid");
        self.range = range.into_raw();
        result
    }

    #[must_use]
    pub fn vertical_scroll(&self) -> usize {
        self.vertical_scroll
    }

    pub fn scroll_to(&mut self, line: usize) {
        let last_line = self.rope().len_lines_indigo().saturating_sub(1);
        self.vertical_scroll = min(line, last_line);
    }
}
