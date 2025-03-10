use crate::{
    range::{Range, RangeMut, RawRange},
    rope::RopeExt as _,
};
use ropey::Rope;
use std::cmp::min;

#[derive(Default)]
pub struct Buffer {
    rope: Rope,
    range: RawRange,
    vertical_scroll: usize,
}

impl Buffer {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn rope(&self) -> &Rope {
        &self.rope
    }

    #[must_use]
    pub fn range(&self) -> &RawRange {
        &self.range
    }

    pub fn with_range<T>(&self, func: impl Fn(&Range) -> T) -> T {
        let range = Range::new(
            &self.rope,
            self.range.anchor.gap_index(),
            self.range.head.gap_index(),
        )
        .unwrap();
        func(&range)
    }

    pub fn with_range_mut<T>(&mut self, func: impl Fn(&mut RangeMut) -> T) -> T {
        let mut range = RangeMut::new(
            &mut self.rope,
            self.range.anchor.gap_index(),
            self.range.head.gap_index(),
        )
        .unwrap();
        let result = func(&mut range);
        range.assert_valid();
        self.range = range.raw().clone();
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

impl From<Rope> for Buffer {
    fn from(rope: Rope) -> Self {
        Self {
            range: RawRange::new_snapped(&rope, 0, 0),
            rope,
            ..Self::default()
        }
    }
}
