use crate::buffer::Buffer;
use generational_arena::{Arena, Index};

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
