use crate::{
    history::History,
    ot::{self, EditSeq},
    range::{Range, RangeMut, RangeState},
    rope::RopeExt as _,
    text::Text,
};
use ropey::Rope;
use std::cmp::min;

#[derive(Default)]
pub struct Buffer {
    text: Text,
    // TODO: Push to history
    history: History<(EditSeq, RangeState)>,
    range: RangeState,
    vertical_scroll: usize,
}

impl Buffer {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn text(&self) -> &Text {
        &self.text
    }

    pub fn range(&self) -> Range<'_> {
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

    pub fn undo(&mut self) -> Result<bool, ot::Error> {
        if let Some((edit, range)) = self.history.undo() {
            self.text.edit(edit)?;
            self.range = range.clone();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub fn redo(&mut self) -> Result<bool, ot::Error> {
        if let Some((edit, range)) = self.history.redo() {
            self.text.edit(edit)?;
            self.range = range.clone();
            Ok(true)
        } else {
            Ok(false)
        }
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
    fn from(rope: Rope) -> Self {
        let text = Text::from(rope);
        let range = RangeState::default().snapped(&text);
        Self {
            text,
            range,
            ..Self::default()
        }
    }
}

impl From<Text> for Buffer {
    fn from(text: Text) -> Self {
        let range = RangeState::default().snapped(&text);
        Self {
            text,
            range,
            ..Self::default()
        }
    }
}
