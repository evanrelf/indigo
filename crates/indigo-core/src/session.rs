use crate::{
    IndigoBuildHasher,
    buffer::{Buffer, BufferKey},
};
use camino::{Utf8Path, Utf8PathBuf};
use slotmap::SlotMap;
use std::collections::HashMap;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Error from buffer {key:?}")]
    Buffer {
        key: BufferKey,
        #[source]
        source: anyhow::Error,
    },
}

#[derive(Clone, Default)]
pub struct Session {
    buffers: SlotMap<BufferKey, Buffer>,
    pub registers: HashMap<char, Vec<String>, IndigoBuildHasher>,
    pub pwd: Option<Utf8PathBuf>,
}

impl Session {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    // TODO: Figure out a safer/better way to add and remove buffers
    pub(crate) fn insert_buffer(&mut self, buffer: Buffer) -> BufferKey {
        self.buffers.insert(buffer)
    }

    #[must_use]
    pub fn get_buffer(&self, buffer_key: BufferKey) -> Option<&Buffer> {
        self.buffers.get(buffer_key)
    }

    #[must_use]
    pub fn get_buffer_mut(&mut self, buffer_key: BufferKey) -> Option<&mut Buffer> {
        self.buffers.get_mut(buffer_key)
    }

    pub fn buffers(&self) -> impl Iterator<Item = (BufferKey, &Buffer)> {
        self.buffers.iter()
    }

    pub fn buffers_mut(&mut self) -> impl Iterator<Item = (BufferKey, &mut Buffer)> {
        self.buffers.iter_mut()
    }

    #[must_use]
    pub fn absolute_path(&self, path: &Utf8Path) -> Utf8PathBuf {
        match &self.pwd {
            Some(pwd) => pwd.join(path),
            None => path.to_path_buf(),
        }
    }

    pub fn assert_invariants(&self) -> anyhow::Result<()> {
        for (key, buffer) in &self.buffers {
            buffer
                .assert_invariants()
                .map_err(|source| Error::Buffer { key, source })?;
        }
        Ok(())
    }
}
