use crate::buffer::Buffer;
use generational_arena::{Arena, Index};
use std::{io, path::Path};

pub struct Editor {
    buffers: Arena<Buffer>,
    current_buffer_index: Index,
}

impl Editor {
    #[must_use]
    pub fn buffers(&self) -> &Arena<Buffer> {
        &self.buffers
    }

    #[must_use]
    pub fn current_buffer_index(&self) -> Index {
        self.current_buffer_index
    }

    #[must_use]
    pub fn current_buffer(&self) -> &Buffer {
        &self.buffers[self.current_buffer_index]
    }

    pub fn new() -> Self {
        Self::default()
    }

    pub fn open_buffer<P>(&mut self, path: P) -> Result<&Buffer, io::Error>
    where
        P: AsRef<Path>,
    {
        self.current_buffer_index = self.buffers.insert(Buffer::open(path)?);
        Ok(self.buffers.get(self.current_buffer_index).unwrap())
    }

    pub fn close_buffer(&mut self, buffer_index: Index, discard_modifications: bool) {
        if let Some(buffer) = self.buffers.get(buffer_index) {
            if !buffer.is_modified() || discard_modifications {
                self.buffers.remove(buffer_index);
                self.current_buffer_index = match self.buffers.iter().last() {
                    None => self.buffers.insert(Buffer::default()),
                    Some((last_buffer_index, _)) => last_buffer_index,
                }
            }
        }
    }

    #[cfg(debug_assertions)]
    pub fn assert_invariants(&self) {
        debug_assert!(!self.buffers.is_empty(), "must have at least one buffer");
        debug_assert!(
            self.buffers.get(self.current_buffer_index).is_some(),
            "current buffer index must be valid"
        );
    }
}

impl Default for Editor {
    fn default() -> Self {
        let mut buffers = Arena::new();
        let current_buffer_index = buffers.insert(Buffer::default());
        Self {
            buffers,
            current_buffer_index,
        }
    }
}
