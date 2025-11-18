use crate::{
    fs::Fs,
    range::{Range, RangeMut, RangeState},
    selection::{Selection, SelectionMut, SelectionState},
    text::Text,
};
use camino::Utf8Path;
use camino::Utf8PathBuf;
use ropey::Rope;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Error from range")]
    Range(#[source] anyhow::Error),
}

#[derive(Default)]
pub enum BufferKind {
    #[default]
    Scratch,
    File {
        path: Utf8PathBuf,
        /// Last known state of file on disk. Used for modification tracking.
        on_disk: Rope,
    },
}

#[derive(Default)]
pub struct Buffer {
    kind: BufferKind,
    text: Text,
    // TODO: Track history of range state
    range: RangeState,
    selection: SelectionState,
}

impl Buffer {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn open(fs: &mut impl Fs, path: impl AsRef<Utf8Path>) -> anyhow::Result<Self> {
        let path = path.as_ref();
        // TODO: Canonicalize path.
        let exists = fs.exists(path)?;
        let rope = if exists {
            let bytes = fs.read(path)?;
            let string = str::from_utf8(&bytes)?;
            Rope::from(string)
        } else {
            Rope::new()
        };
        let mut buffer = Self::from(rope.clone());
        buffer.kind = BufferKind::File {
            path: path.to_path_buf(),
            on_disk: rope,
        };
        Ok(buffer)
    }

    pub fn save(&mut self, fs: &mut impl Fs) -> anyhow::Result<()> {
        if let BufferKind::File { path, on_disk } = &mut self.kind {
            let mut bytes = Vec::with_capacity(self.text.len_bytes());
            self.text.write_to(&mut bytes)?;
            fs.write(path, &bytes)?;
            *on_disk = self.text.clone();
        }
        Ok(())
    }

    #[must_use]
    pub fn kind(&self) -> &BufferKind {
        &self.kind
    }

    #[must_use]
    pub fn path(&self) -> Option<&Utf8Path> {
        if let BufferKind::File { path, .. } = &self.kind {
            Some(path)
        } else {
            None
        }
    }

    #[must_use]
    pub fn rope(&self) -> &Rope {
        self.text.rope()
    }

    #[must_use]
    pub fn is_modified(&self) -> Option<bool> {
        if let BufferKind::File { on_disk, .. } = &self.kind {
            Some(self.text.rope() != on_disk)
        } else {
            None
        }
    }

    pub fn range(&self) -> Range<'_> {
        Range::new(&self.text, &self.range)
            .expect("Buffer text and range state are always kept valid")
    }

    pub fn range_mut(&mut self) -> RangeMut<'_> {
        RangeMut::new(&mut self.text, &mut self.range)
            .expect("Buffer text and range state are always kept valid")
            .on_drop(|range| range.assert_invariants().unwrap())
    }

    pub fn selection(&self) -> Selection<'_> {
        Selection::new(&self.text, &self.selection)
            .expect("Buffer text and selection state are always kept valid")
    }

    pub fn selection_mut(&mut self) -> SelectionMut<'_> {
        SelectionMut::new(&mut self.text, &mut self.selection)
            .expect("Buffer text and selection state are always kept valid")
            .on_drop(|selection| selection.assert_invariants().unwrap())
    }

    pub fn commit(&mut self) {
        self.text.commit();
    }

    pub fn undo(&mut self) -> anyhow::Result<bool> {
        if self.text.undo()? {
            self.range.snap(self.text.rope());
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub fn redo(&mut self) -> anyhow::Result<bool> {
        if self.text.redo()? {
            self.range.snap(self.text.rope());
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub(crate) fn assert_invariants(&self) -> anyhow::Result<()> {
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
    use crate::fs::TestFs;

    #[test]
    fn fs() -> anyhow::Result<()> {
        let mut fs = TestFs::default();
        fs.write("main.rs".into(), b"fn main() {}")?;
        let mut buffer = Buffer::open(&mut fs, "main.rs")?;
        assert_eq!(&buffer.text.to_string(), "fn main() {}");
        let BufferKind::File { path, .. } = &mut buffer.kind else {
            unreachable!();
        };
        *path = Utf8PathBuf::from("main2.rs");
        buffer.save(&mut fs)?;
        assert_eq!(fs.read("main2.rs".into())?, b"fn main() {}");
        Ok(())
    }
}
