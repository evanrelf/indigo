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
    pub node_history: NodeHistory,
    pub height: u16,
    pub prev_vertical_scroll: usize,
    // TODO(horizontal_scroll)
}

impl WindowState {
    #[must_use]
    pub fn new(buffer_key: BufferKey) -> Self {
        let selection = SelectionState {
            ranges: vec![RangeState::default()],
            primary_range: 0,
        };
        Self {
            buffer: buffer_key,
            selection,
            node_history: NodeHistory::default(),
            height: 0,
            prev_vertical_scroll: 0,
        }
    }
}

#[derive(Clone, Default)]
pub struct NodeHistory {
    version: usize,
    selections: Vec<SelectionState>,
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

    pub fn assert_invariants(&self) -> anyhow::Result<()> {
        let _ = Selection::new(&self.buffer.text, &self.state.selection)?;
        Ok(())
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
        let head_byte_index = state.ranges[state.primary_range].head.byte_index;
        let line = self
            .buffer
            .text
            .rope()
            .byte_to_line_idx(head_byte_index, LINE_TYPE);
        let top = self.vertical_scroll();
        let bottom = top + usize::from(self.state.height).saturating_sub(1);
        if line < top {
            self.state.prev_vertical_scroll = line;
        } else if line > bottom {
            self.state.prev_vertical_scroll = top + (line - bottom);
        }
    }

    pub fn scroll_center_selection(&mut self) {
        let state = &self.state.selection;
        let head_byte_index = state.ranges[state.primary_range].head.byte_index;
        let line = self
            .buffer
            .text
            .rope()
            .byte_to_line_idx(head_byte_index, LINE_TYPE);
        let half_height = usize::from(self.state.height) / 2;
        self.state.prev_vertical_scroll = line.saturating_sub(half_height);
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

    pub fn expand_to_outer_node(&mut self, count: usize) -> usize {
        for step in 0..count {
            let before = self.state.selection.clone();
            self.selection_mut().for_each_mut(|mut range| {
                range.expand_to_outer_node();
            });
            let after = &self.state.selection;
            // Mutual containment means the bounds didn't change, so every range is at the root.
            if before.contains(after) && after.contains(&before) {
                return step;
            }
            self.node_history_mut().selections.push(before);
        }
        count
    }

    pub fn shrink_to_inner_node(&mut self, count: usize) {
        for _ in 0..count {
            match self.node_history_mut().selections.pop() {
                Some(previous) if self.state.selection.contains(&previous) => {
                    self.state.selection = previous;
                    continue;
                }
                Some(_) => self.state.node_history.selections.clear(),
                None => {}
            }
            self.selection_mut().for_each_mut(|mut range| {
                range.shrink_to_inner_node();
            });
        }
    }

    fn node_history_mut(&mut self) -> &mut NodeHistory {
        let version = self.buffer.text.version();
        let history = &mut self.state.node_history;
        if history.version != version {
            history.version = version;
            history.selections.clear();
        }
        history
    }

    #[tracing::instrument(skip_all)]
    pub fn undo(&mut self) -> anyhow::Result<bool> {
        let version = self.buffer.text.version();
        if self.buffer.text.undo()? {
            if let Some(opss) = self.buffer.text.ops_since(version) {
                for ops in opss {
                    self.state.selection.transform(ops, &self.buffer.text);
                }
            }
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
                    self.state.selection.transform(ops, &self.buffer.text);
                }
            }
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

#[cfg(all(test, feature = "language-rust"))]
mod tests {
    use super::*;
    use crate::syntax::Language;
    use ropey::Rope;

    const CODE: &str = "\
#[derive(clap::ValueEnum, Clone, Default)]
struct S;

fn main() {
    let x = foo(1, \"two\");
    if x { bar() } else { baz() }
}
";

    fn editor() -> Editor {
        let mut buffer = Buffer::from(Rope::from(CODE));
        buffer.text.set_language(Language::Rust);
        Editor::from(buffer)
    }

    fn bounds(editor: &Editor) -> Vec<(usize, usize)> {
        editor
            .focused_window()
            .selection()
            .state()
            .ranges
            .iter()
            .map(|range| (range.start().byte_index, range.end().byte_index))
            .collect()
    }

    #[hegel::test(test_cases = 300)]
    fn fuzz(tc: hegel::TestCase) {
        use hegel::{TestCase, generators as gs};

        struct StateMachine {
            editor: Editor,
        }
        #[hegel::state_machine]
        #[expect(clippy::needless_pass_by_value)]
        impl StateMachine {
            fn count(tc: &TestCase) -> usize {
                tc.draw(gs::integers::<usize>().min_value(1).max_value(8))
            }
            #[rule]
            fn move_to(&mut self, tc: TestCase) {
                let mut window = self.editor.focused_window_mut();
                let len = window.buffer().text.len();
                let byte_index = tc.draw(gs::integers::<usize>().max_value(len));
                window
                    .selection_mut()
                    .for_each_mut(|mut range| range.move_to(byte_index));
            }
            #[rule]
            fn extend_to(&mut self, tc: TestCase) {
                let mut window = self.editor.focused_window_mut();
                let len = window.buffer().text.len();
                let byte_index = tc.draw(gs::integers::<usize>().max_value(len));
                window
                    .selection_mut()
                    .for_each_mut(|mut range| range.extend_to(byte_index));
            }
            #[rule]
            fn insert(&mut self, tc: TestCase) {
                let string = tc.draw(gs::text());
                let mut window = self.editor.focused_window_mut();
                window.selection_mut().insert(&string);
            }
            #[rule]
            fn expand(&mut self, tc: TestCase) {
                let count = Self::count(&tc);
                let before = bounds(&self.editor);
                self.editor.focused_window_mut().expand_to_outer_node(count);
                let after = bounds(&self.editor);
                for ((old_start, old_end), (new_start, new_end)) in std::iter::zip(before, after) {
                    assert!(new_start <= old_start && old_end <= new_end);
                }
            }
            #[rule]
            fn shrink(&mut self, tc: TestCase) {
                let count = Self::count(&tc);
                let before = bounds(&self.editor);
                self.editor.focused_window_mut().shrink_to_inner_node(count);
                let after = bounds(&self.editor);
                for ((old_start, old_end), (new_start, new_end)) in std::iter::zip(before, after) {
                    assert!(old_start <= new_start && new_end <= old_end);
                }
            }
            #[rule]
            fn expand_then_shrink_round_trips(&mut self, tc: TestCase) {
                let count = Self::count(&tc);
                let before = bounds(&self.editor);
                let steps = self.editor.focused_window_mut().expand_to_outer_node(count);
                self.editor.focused_window_mut().shrink_to_inner_node(steps);
                assert_eq!(bounds(&self.editor), before);
            }
            #[invariant]
            fn invariants(&self, _: TestCase) {
                self.editor.assert_invariants().unwrap();
            }
        }
        hegel::stateful::run(StateMachine { editor: editor() }, tc);
    }

    #[test]
    fn shrink_retraces_history_or_falls_back_to_structure() {
        let mut editor = editor();
        let start = CODE.find("foo").unwrap();
        let call_end = start + "foo(1, \"two\")".len() - 1;
        let one = start + 4;

        // `f` grows to `foo`, then to the call.
        let mut window = editor.focused_window_mut();
        window
            .selection_mut()
            .for_each_mut(|mut range| range.move_to(start));
        window.expand_to_outer_node(2);
        assert_eq!(bounds(&editor), vec![(start, call_end)]);

        // Moving to `1` makes the remembered `foo` unusable, and `1` is a leaf, so shrinking
        // does nothing.
        let mut window = editor.focused_window_mut();
        window
            .selection_mut()
            .for_each_mut(|mut range| range.move_to(one));
        window.shrink_to_inner_node(1);
        assert_eq!(bounds(&editor), vec![(one, one)]);

        // Growing twice from `1` and shrinking three times retraces to `1` and stops there.
        let mut window = editor.focused_window_mut();
        window.expand_to_outer_node(2);
        assert_eq!(bounds(&editor), vec![(start, call_end)]);
        let mut window = editor.focused_window_mut();
        window.shrink_to_inner_node(3);
        assert_eq!(bounds(&editor), vec![(one, one)]);

        // Selecting the call by hand leaves no history, so shrinking descends to `foo`.
        let mut window = editor.focused_window_mut();
        window.selection_mut().for_each_mut(|mut range| {
            range.move_to(start);
            range.extend_to(call_end);
        });
        window.shrink_to_inner_node(1);
        assert_eq!(bounds(&editor), vec![(start, start + 2)]);
    }
}
