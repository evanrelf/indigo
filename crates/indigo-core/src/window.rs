use crate::{buffer::Buffer, rope::RopeExt as _};
use indigo_wrap::{WMut, WRef, Wrap, WrapMut, WrapRef};
use std::cmp::min;

// TODO: Define invariants, write `assert_invariants`, make `new` fallible, add function to correct
// invariant violations (e.g. if one window truncates a buffer, another window needs to handle
// that).

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

    pub fn scroll_to_line(&mut self, line: usize) {
        let last_line = self.buffer.rope().len_lines_indigo().saturating_sub(1);
        self.state.vertical_scroll = min(line, last_line);
    }

    pub fn scroll_to_selection(&mut self) {
        let head_char_offset = self.buffer.range().head().char_offset();
        let line = self.buffer.rope().char_to_line(head_char_offset);
        let top = self.state.vertical_scroll;
        let bottom = (top + usize::from(self.state.height)) - 1;
        if line < top {
            self.state.vertical_scroll = line;
        } else if line > bottom {
            self.state.vertical_scroll = top + (line - bottom);
        }
    }
}
