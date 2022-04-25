use crate::buffer::Buffer;

pub struct Editor {
    buffers: Vec<Buffer>,
    current_buffer: usize,
}

impl Editor {
    #[must_use]
    pub fn buffers(&self) -> &Vec<Buffer> {
        &self.buffers
    }

    #[must_use]
    pub fn current_buffer(&self) -> &Buffer {
        &self.buffers[self.current_buffer]
    }

    #[cfg(debug_assertions)]
    pub fn assert_invariants(&self) {
        debug_assert!(!self.buffers.is_empty(), "must have at least one buffer");
        debug_assert!(
            self.buffers.get(self.current_buffer).is_some(),
            "current buffer index must be valid"
        );
    }
}

impl Default for Editor {
    fn default() -> Self {
        Self {
            buffers: vec![Buffer::default()],
            current_buffer: 0,
        }
    }
}
