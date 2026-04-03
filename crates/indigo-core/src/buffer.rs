use crate::{fs::Fs, text::Text};
use camino::Utf8Path;
use ropey::Rope;
use std::sync::Arc;

slotmap::new_key_type! {
    #[must_use]
    pub struct BufferKey;
}

#[derive(Clone, Default)]
pub enum BufferKind {
    /// In-memory scratch buffer.
    #[default]
    Scratch,
    /// Buffer mapping to a file on disk.
    File {
        path: Arc<Utf8Path>,
        /// Last known state of file on disk, used for modification tracking.
        on_disk: Rope,
    },
}

#[derive(Clone, Default)]
pub struct Buffer {
    kind: BufferKind,
    text: Text,
}

impl Buffer {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn open(fs: &impl Fs, path: impl AsRef<Utf8Path>) -> anyhow::Result<Self> {
        let path = path.as_ref();
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
            path: Arc::from(path),
            on_disk: rope,
        };
        Ok(buffer)
    }

    pub fn save(&mut self, fs: &impl Fs) -> anyhow::Result<()> {
        if let BufferKind::File { path, on_disk } = &mut self.kind {
            let mut bytes = Vec::with_capacity(self.text.len());
            self.text.write_to(&mut bytes)?;
            fs.write(path, &bytes)?;
            *on_disk = self.text.rope().clone();
        }
        Ok(())
    }

    pub fn save_as(&mut self, fs: &impl Fs, path: impl AsRef<Utf8Path>) -> anyhow::Result<()> {
        let path = path.as_ref();
        let mut bytes = Vec::with_capacity(self.text.len());
        self.text.write_to(&mut bytes)?;
        fs.write(path, &bytes)?;
        self.kind = BufferKind::File {
            path: Arc::from(path),
            on_disk: self.text.rope().clone(),
        };
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

    #[must_use]
    pub fn is_readonly(&self) -> bool {
        self.text.readonly
    }

    pub fn set_readonly(&mut self, readonly: bool) {
        self.text.readonly = readonly;
    }

    // TODO: Ditch `pub(crate)`?
    pub(crate) fn text(&self) -> &Text {
        &self.text
    }

    // TODO: Ditch `pub(crate)`?
    pub(crate) fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }

    pub fn commit(&mut self) {
        self.text.commit();
    }

    pub fn undo(&mut self) -> anyhow::Result<bool> {
        self.text.undo()
    }

    pub fn redo(&mut self) -> anyhow::Result<bool> {
        self.text.redo()
    }
}

impl From<Rope> for Buffer {
    fn from(rope: Rope) -> Self {
        Self::from(Text::from(rope))
    }
}

impl From<Text> for Buffer {
    fn from(text: Text) -> Self {
        Self {
            text,
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
        let fs = TestFs::default();
        fs.write("main.rs".into(), b"fn main() {}")?;
        let mut buffer = Buffer::open(&fs, "main.rs")?;
        assert_eq!(&buffer.text.to_string(), "fn main() {}");
        let BufferKind::File { path, .. } = &mut buffer.kind else {
            unreachable!();
        };
        *path = Arc::from(Utf8Path::new("main2.rs"));
        buffer.save(&fs)?;
        assert_eq!(fs.read("main2.rs".into())?, b"fn main() {}");
        Ok(())
    }
}
