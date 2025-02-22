use crate::{
    mode::{Mode, NormalMode},
    range::{Range, RangeMut, RawRange},
    rope::RopeExt as _,
};
use ropey::Rope;
use std::{cmp::min, num::NonZeroUsize};

pub struct Editor {
    rope: Rope,
    range: RawRange,
    pub mode: Mode,
    pub height: usize,
    vertical_scroll: usize,
    pub quit: bool,
}

impl Editor {
    #[must_use]
    pub fn from_rope(rope: Rope) -> Self {
        let range = RawRange::new_snapped(&rope, 0, 0);
        Self {
            rope,
            range,
            mode: Mode::Normal(NormalMode {
                count: NonZeroUsize::MIN,
            }),
            height: 0,
            vertical_scroll: 0,
            quit: false,
        }
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
            self.range.anchor.gap_index,
            self.range.head.gap_index,
        )
        .unwrap();
        func(&range)
    }

    pub fn with_range_mut<T>(&mut self, func: impl Fn(&mut RangeMut) -> T) -> T {
        let mut range = RangeMut::new(
            &mut self.rope,
            self.range.anchor.gap_index,
            self.range.head.gap_index,
        )
        .unwrap();
        let result = func(&mut range);
        range.assert_valid();
        self.range = range.raw();
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
