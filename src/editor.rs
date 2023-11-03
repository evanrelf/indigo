use crate::{buffer::Buffer, mode::Mode};

#[derive(Default)]
pub struct Editor {
    buffers: Vec<Buffer>,
    mode: Mode,
}
