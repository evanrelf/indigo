use crate::buffer::Buffer;

pub struct Editor {
    quit: bool,
    buffers: Vec<Buffer>,
}

impl Editor {
    pub fn new() -> Editor {
        Editor {
            quit: false,
            buffers: Vec::new(),
        }
    }
}
