use crate::{buffer::Buffer, rope::RopeExt as _};
use indigo_wrap::{WMut, WRef, Wrap, WrapMut, WrapRef};
use std::cmp::min;

#[derive(Default)]
pub struct WindowState {
    pub height: u16,
    pub vertical_scroll: usize,
}

#[must_use]
pub struct WindowView<'a, W: Wrap> {
    buffer: W::Wrap<'a, Buffer>,
    state: W::Wrap<'a, WindowState>,
}

pub type Window<'a> = WindowView<'a, WRef>;

pub type WindowMut<'a> = WindowView<'a, WMut>;

impl<'a, W: WrapRef> WindowView<'a, W> {
    pub fn new(buffer: W::WrapRef<'a, Buffer>, state: W::WrapRef<'a, WindowState>) -> Self {
        WindowView { buffer, state }
    }

    #[must_use]
    pub fn height(&self) -> u16 {
        self.state.height
    }

    #[must_use]
    pub fn vertical_scroll(&self) -> usize {
        self.state.vertical_scroll
    }

    #[must_use]
    pub fn buffer(&self) -> &Buffer {
        &self.buffer
    }
}

impl<W: WrapMut> WindowView<'_, W> {
    pub fn set_height(&mut self, height: u16) {
        self.state.height = height;
    }

    pub fn scroll_to(&mut self, line: usize) {
        let last_line = self.buffer.text().len_lines_indigo().saturating_sub(1);
        self.state.vertical_scroll = min(line, last_line);
    }
}
