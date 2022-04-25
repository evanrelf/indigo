use crate::buffer::Buffer;

pub struct Editor {
    buffers: Vec<Buffer>,
    current_buffer: usize,
}

impl Editor {
    pub fn buffers(&self) -> &Vec<Buffer> {
        &self.buffers
    }

    pub fn current_buffer(&self) -> &Buffer {
        &self.buffers[self.current_buffer]
    }

    #[cfg(debug_assertions)]
    pub fn assert_invariants(&self) {
        debug_assert!(!self.buffers.is_empty(), "Must have at least one buffer");
    }
}
