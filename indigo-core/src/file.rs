use crate::Buffer;
use anyhow::Context as _;
use camino::Utf8PathBuf;
use ropey::Rope;
use std::{io::BufReader, path::PathBuf};

slotmap::new_key_type! { pub struct FileKey; }

// TODO: Add method to refresh `is_modified` and `is_read_only` with the filesystem.

#[derive(Clone, Debug, Default)]
pub struct File {
    path: Utf8PathBuf,
    buffer: Buffer,
    // TODO: Track the hash of on-disk bytes, compare with the hash of in-memory bytes, and if they
    // differ consider it modified. Then modification state will be tracked implicitly, without the
    // need to explicitly track rope modifications.
    //
    // Or maybe `ropey` is efficient enough, with its copy-on-write stuff, that we could keep an
    // original `Rope` (matching the last on-disk state) and use `PartialEq` instead of `Hash`.
    // Ropes will often differ in length, so checking for equality will often be O(1).
    is_modified: bool,
    is_read_only: bool,
}

impl File {
    #[must_use]
    pub fn path(&self) -> &Utf8PathBuf {
        &self.path
    }

    #[must_use]
    pub fn buffer(&self) -> &Buffer {
        &self.buffer
    }

    // TODO: Somehow update `is_modified`
    #[must_use]
    pub fn buffer_mut(&mut self) -> &mut Buffer {
        &mut self.buffer
    }

    #[must_use]
    pub fn is_modified(&self) -> bool {
        self.is_modified
    }

    #[must_use]
    pub fn is_read_only(&self) -> bool {
        self.is_read_only
    }

    pub fn open(path: Utf8PathBuf) -> anyhow::Result<Self> {
        let file =
            std::fs::File::open(PathBuf::from(path.clone())).context("Failed to open file")?;

        let is_read_only = file
            .metadata()
            .context("Failed to get file metadata")?
            .permissions()
            .readonly();

        let rope =
            Rope::from_reader(BufReader::new(file)).context("Failed to read file into rope")?;

        let empty = rope.len_chars() == 0;

        Ok(Self {
            path,
            buffer: Buffer::try_from(rope).unwrap_or_default(),
            is_modified: empty,
            is_read_only,
        })
    }

    pub fn assert_valid(&self) {
        // `File` maintains invariants, but can't check that they're upheld after the fact.
    }
}