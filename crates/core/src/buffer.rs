use crate::{
    range::{Range, RangeMut, RangeState},
    rope::RopeExt as _,
};
use ropey::Rope;
use std::cmp::min;

#[derive(Default)]
pub struct Buffer {
    text: Rope,
    range: RangeState,
    vertical_scroll: usize,
}

impl Buffer {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn text(&self) -> &Rope {
        &self.text
    }

    pub fn range(&self) -> Range {
        let range = Range::new(&self.text, &self.range).unwrap();
        range.assert_invariants().unwrap();
        range
    }

    pub fn with_range_mut<T>(&mut self, func: impl Fn(&mut RangeMut) -> T) -> T {
        let mut range = RangeMut::new(&mut self.text, &mut self.range).unwrap();
        let result = func(&mut range);
        range.assert_invariants().unwrap();
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

impl From<Rope> for Buffer {
    fn from(text: Rope) -> Self {
        let range = RangeState::default().snapped(&text);
        Self {
            text,
            range,
            ..Self::default()
        }
    }
}
