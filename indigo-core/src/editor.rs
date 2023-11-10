use crate::{buffer::Buffer, mode::Mode};

#[derive(Debug)]
pub struct Editor {
    buffers: Vec<Buffer>,
    current_buffer: usize,
    mode: Mode,
}

impl Default for Editor {
    fn default() -> Self {
        Self {
            buffers: vec![Buffer::default()],
            current_buffer: 0,
            mode: Mode::default(),
        }
    }
}

impl Editor {
    // TODO: Delete this, it's bad
    #[must_use]
    pub fn new(buffers: Vec<Buffer>, current_buffer: usize, mode: Mode) -> Self {
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
    pub fn current_buffer(&self) -> &Buffer {
        match self.buffers.get(self.current_buffer) {
            Some(buffer) => buffer,
            None => panic!("omg editor didn't maintain invariant"),
        }
    }

    #[must_use]
    pub fn current_buffer_mut(&mut self) -> &mut Buffer {
        match self.buffers.get_mut(self.current_buffer) {
            Some(buffer) => buffer,
            None => panic!("omg editor didn't maintain invariant"),
        }
    }

    // TODO: Should `mode` just be `pub`? Or are there invariants that need to be enforced, so this
    // should be more restrictive?

    #[must_use]
    pub fn mode(&self) -> &Mode {
        &self.mode
    }

    #[must_use]
    pub fn mode_mut(&mut self) -> &mut Mode {
        &mut self.mode
    }
}