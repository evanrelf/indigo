use crate::{
    history::History,
    ot::{self, EditSeq},
    range::{self, Range, RangeMut, RangeState},
    rope::RopeExt as _,
    text::Text,
};
use camino::Utf8PathBuf;
use ropey::Rope;
use std::cmp::min;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Error from range")]
    Range(#[source] range::Error),
}

#[derive(Default)]
pub struct Buffer {
    pub path: Option<Utf8PathBuf>,
    pub modified: bool,
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

    pub fn range_mut(&mut self) -> RangeMut<'_> {
        RangeMut::new(&mut self.text, &mut self.range)
            .unwrap()
            .guard()
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

    pub(crate) fn assert_invariants(&self) -> Result<(), Error> {
        self.range().assert_invariants().map_err(Error::Range)?;
        Ok(())
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
