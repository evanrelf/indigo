use crate::{buffer::Buffer, mode::Mode};

#[derive(Debug, Default)]
pub struct Editor {
    buffers: Vec<Buffer>,
    mode: Mode,
}
