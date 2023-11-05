use crate::{buffer::Buffer, mode::Mode};

#[derive(Debug, Default)]
pub struct Editor {
    buffers: Vec<Buffer>,
    current_buffer: Option<usize>,
    mode: Mode,
}

impl Editor {
    // TODO: Delete this, it's bad
    #[must_use]
    pub fn new(buffers: Vec<Buffer>, current_buffer: Option<usize>, mode: Mode) -> Self {
        Self {
            buffers,
            current_buffer,
            mode,
        }
    }

    #[must_use]
    pub fn buffers(&self) -> &Vec<Buffer> {
        &self.buffers
    }

    #[must_use]
    pub fn current_buffer(&self) -> Option<&Buffer> {
        match self.buffers.get(self.current_buffer?) {
            Some(buffer) => Some(buffer),
            None => panic!("omg editor didn't maintain invariant"),
        }
    }

    #[must_use]
    pub fn current_buffer_mut(&mut self) -> Option<&mut Buffer> {
        match self.buffers.get_mut(self.current_buffer?) {
            Some(buffer) => Some(buffer),
            None => panic!("omg editor didn't maintain invariant"),
        }
    }

    #[must_use]
    pub fn mode(&self) -> Mode {
        self.mode
    }
}
