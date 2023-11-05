use crate::{buffer::Buffer, mode::Mode};

#[derive(Debug, Default)]
pub struct Editor {
    // TODO: These shouldn't be `pub`
    pub buffers: Vec<Buffer>,
    pub current: Option<usize>,
    pub mode: Mode,
}
