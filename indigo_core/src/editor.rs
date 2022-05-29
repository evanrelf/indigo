use crate::{
    buffer::Buffer,
    mode::{Mode, NormalMode},
};
use generational_arena::{Arena, Index};
use std::path::Path;

pub struct Editor {
    buffers: Arena<Buffer>,
    current_buffer_index: Index,
    pub mode: Mode,
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

    #[must_use]
    pub fn current_buffer_mut(&mut self) -> &mut Buffer {
        &mut self.buffers[self.current_buffer_index]
    }

    #[must_use]
    pub fn get_buffer(&self, index: Index) -> Option<&Buffer> {
        self.buffers.get(index)
    }

    #[must_use]
    pub fn get_buffer_mut(&mut self, index: Index) -> Option<&mut Buffer> {
        self.buffers.get_mut(index)
    }

    pub fn open_buffer(&mut self, path: impl AsRef<Path>) -> Result<Index, anyhow::Error> {
        let index = self.buffers.insert(Buffer::open(path)?);
        self.current_buffer_index = index;
        Ok(index)
    }

    pub fn close_buffer(&mut self, index: Index, discard_modifications: bool) {
        if let Some(buffer) = self.buffers.get(index) {
            if !buffer.is_modified() || discard_modifications {
                self.buffers.remove(index);
                self.current_buffer_index = match self.buffers.iter().last() {
                    None => self.buffers.insert(Buffer::default()),
                    Some((last_index, _)) => last_index,
                }
            }
        }
    }

    pub fn assert_invariants(&self) {
        assert!(!self.buffers.is_empty(), "must have at least one buffer");
        assert!(
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
            mode: Mode::Normal(NormalMode::default()),
        }
    }
}
