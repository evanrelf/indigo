use crate::{
    buffer::{Buffer, BufferKey},
    editor::Editor,
    range::RangeState,
    rope::{LINE_TYPE, RopeExt as _},
    selection::{Selection, SelectionMut, SelectionState},
};
use indigo_wrap::{WMut, WRef, Wrap, WrapMut, WrapRef};
use std::cmp::min;

slotmap::new_key_type! {
    #[must_use]
    pub struct WindowKey;
}

#[derive(Clone)]
pub struct WindowState {
    pub buffer: BufferKey,
    pub selection: SelectionState,
    pub height: u16,
    pub prev_vertical_scroll: usize,
    // TODO(horizontal_scroll)
}

impl WindowState {
    #[must_use]
    pub fn new(buffer_key: BufferKey, buffer: &Buffer) -> Self {
        let selection = SelectionState {
            ranges: vec![RangeState::default().snapped_to_grapheme_boundaries(buffer.text.rope())],
            primary_range: 0,
        };
        Self {
            buffer: buffer_key,
            selection,
            height: 0,
            prev_vertical_scroll: 0,
        }
    }
}

#[must_use]
pub struct WindowView<'a, W: Wrap> {
    buffer: W::Wrap<'a, Buffer>,
    state: W::Wrap<'a, WindowState>,
}

pub type Window<'a> = WindowView<'a, WRef>;

pub type WindowMut<'a> = WindowView<'a, WMut>;

#[expect(clippy::elidable_lifetime_names)]
impl<'a, W: Wrap> WindowView<'a, W> {
    //
}

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
        let last_line = self.buffer.text.rope().len_lines_indigo().saturating_sub(1);
        min(self.state.prev_vertical_scroll, last_line)
    }

    // TODO(horizontal_scroll)

    #[must_use]
    pub fn buffer(&self) -> &Buffer {
        &self.buffer
    }

    pub fn selection(&self) -> Selection<'_> {
        Selection::new(&self.buffer.text, &self.state.selection)
            .expect("Window text and selection state are always kept valid")
    }
}

impl<W: WrapMut> WindowView<'_, W> {
    pub fn set_height(&mut self, height: u16) {
        self.state.height = height;
    }

    pub fn scroll_to_line(&mut self, line: usize) {
        let last_line = self.buffer.text.rope().len_lines_indigo().saturating_sub(1);
        self.state.prev_vertical_scroll = min(line, last_line);
    }

    // TODO: There's a bug where the cursor can move one line above the viewport without this
    // function scrolling it into view. Try moving up line-by-line in a long file like `Cargo.lock`
    // and watch it pop in and out from the top of the viewport. Moving down works correctly: the
    // cursor never disappears below the bottom of the viewport.
    pub fn scroll_to_selection(&mut self) {
        let state = &self.state.selection;
        let head_byte_offset = state.ranges[state.primary_range].head.byte_offset;
        let line = self
            .buffer
            .text
            .rope()
            .byte_to_line_idx(head_byte_offset, LINE_TYPE);
        let top = self.vertical_scroll();
        let bottom = top + usize::from(self.state.height).saturating_sub(1);
        if line < top {
            self.state.prev_vertical_scroll = line;
        } else if line > bottom {
            self.state.prev_vertical_scroll = top + (line - bottom);
        }
    }

    #[must_use]
    pub fn buffer_mut(&mut self) -> &mut Buffer {
        &mut self.buffer
    }

    pub fn selection_mut(&mut self) -> SelectionMut<'_> {
        SelectionMut::new(&mut self.buffer.text, &mut self.state.selection)
            .expect("Window text and selection state are always kept valid")
            .on_drop(|selection| selection.assert_invariants().unwrap())
    }

    #[tracing::instrument(skip_all)]
    pub fn undo(&mut self) -> anyhow::Result<bool> {
        let version = self.buffer.text.version();
        if self.buffer.text.undo()? {
            if let Some(opss) = self.buffer.text.ops_since(version) {
                for ops in opss {
                    self.state.selection.transform(ops);
                }
            }
            self.selection_mut().for_each_mut(|mut range| {
                if range.snap_to_grapheme_boundaries() {
                    tracing::warn!("wasn't on grapheme boundary after");
                }
            });
            Ok(true)
        } else {
            Ok(false)
        }
    }

    #[tracing::instrument(skip_all)]
    pub fn redo(&mut self) -> anyhow::Result<bool> {
        let version = self.buffer.text.version();
        if self.buffer.text.redo()? {
            if let Some(opss) = self.buffer.text.ops_since(version) {
                for ops in opss {
                    self.state.selection.transform(ops);
                }
            }
            self.selection_mut().for_each_mut(|mut range| {
                if range.snap_to_grapheme_boundaries() {
                    tracing::warn!("wasn't on grapheme boundary after");
                }
            });
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

pub fn scroll_up(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    let line = window.vertical_scroll().saturating_sub(3);
    window.scroll_to_line(line);
}

pub fn scroll_down(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    let line = window.vertical_scroll() + 3;
    window.scroll_to_line(line);
}

// TODO(horizontal_scroll)

pub fn scroll_half_page_up(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    let line = window
        .vertical_scroll()
        .saturating_sub(usize::from(window.height()) / 2);
    window.scroll_to_line(line);
}

pub fn scroll_half_page_down(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    let line = window.vertical_scroll() + usize::from(window.height()) / 2;
    window.scroll_to_line(line);
}

pub fn scroll_full_page_up(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    let line = window
        .vertical_scroll()
        .saturating_sub(usize::from(window.height()));
    window.scroll_to_line(line);
}

pub fn scroll_full_page_down(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    let line = window.vertical_scroll() + usize::from(window.height());
    window.scroll_to_line(line);
}
