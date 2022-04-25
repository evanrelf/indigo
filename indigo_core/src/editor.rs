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
}
