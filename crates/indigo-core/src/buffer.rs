use crate::{
    history::History,
    io::Io,
    ot::{self, EditSeq},
    range::{self, Range, RangeMut, RangeState},
    rope::RopeExt as _,
    text::Text,
};
use camino::Utf8Path;
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

    pub fn open<I: Io>(io: &mut I, path: impl AsRef<Utf8Path>) -> Result<Self, I::Error> {
        let path = path.as_ref();
        // TODO: Canonicalize path.
        let exists = io.file_exists(path)?;
        let rope = if exists {
            let bytes = io.read_file(path)?;
            // TODO: Include this error in return type.
            let string = str::from_utf8(&bytes).unwrap();
            Rope::from(string)
        } else {
            Rope::new()
        };
        let mut buffer = Self::from(rope);
        buffer.path = Some(path.to_path_buf());
        Ok(buffer)
    }

    pub fn save<I: Io>(&mut self, io: &mut I) -> Result<(), I::Error> {
        if let Some(path) = &self.path {
            let mut bytes = Vec::with_capacity(self.text.len_bytes());
            self.text
                .write_to(&mut bytes)
                .expect("Write to in-memory buffer considered infallible for now");
            io.write_file(path, &bytes)?;
            self.modified = false;
        }
        Ok(())
    }

    #[must_use]
    pub fn text(&self) -> &Text {
        &self.text
    }

    pub fn range(&self) -> Range<'_> {
        Range::new(&self.text, &self.range)
            .expect("Buffer text and range state are always kept valid")
    }

    pub fn range_mut(&mut self) -> RangeMut<'_> {
        RangeMut::new(&mut self.text, &mut self.range)
            .expect("Buffer text and range state are always kept valid")
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::io::{TestIo, TestIoError};

    #[test]
    fn io() -> Result<(), TestIoError> {
        let mut io = TestIo::default();
        io.write_file("main.rs", b"fn main() {}")?;
        let mut buffer = Buffer::open(&mut io, "main.rs")?;
        assert_eq!(&buffer.text.to_string(), "fn main() {}");
        buffer.path = Some(Utf8PathBuf::from("main2.rs"));
        buffer.save(&mut io)?;
        assert_eq!(io.read_file("main2.rs")?, b"fn main() {}");
        Ok(())
    }
}
