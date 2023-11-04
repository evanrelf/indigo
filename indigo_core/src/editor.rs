use crate::{buffer::Buffer, mode::Mode};

#[derive(Debug, Default)]
pub struct Editor {
    pub buffers: Vec<Buffer>,
    pub mode: Mode,
}
