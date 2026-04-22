use crate::{
    cursor::CursorState,
    ot::OperationSeq,
    range::{Range, RangeMut, RangeSnapshot, RangeState},
    rope::RegexCursorInput,
    text::Text,
};
use indigo_wrap::{WMut, WRef, Wrap, WrapMut, WrapRef};
use regex_cursor::engines::meta::Regex;
use ropey::Rope;
use std::{
    cmp::{max, min},
    sync::Arc,
    thread,
};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Error from range")]
    Range(#[source] anyhow::Error),
}

#[derive(Clone)]
pub struct SelectionState {
    pub ranges: Vec<RangeState>,
    pub primary_range: usize,
}

impl SelectionState {
    pub fn transform(&mut self, ops: &OperationSeq) {
        let mut offsets: Vec<usize> = self
            .ranges
            .iter()
            .flat_map(|range| [range.tail.byte_offset, range.head.byte_offset])
            .collect();

        ops.transform_byte_offsets_unsorted(&mut offsets);

        for (i, range) in self.ranges.iter_mut().enumerate() {
            range.tail.byte_offset = offsets[i * 2];
            range.head.byte_offset = offsets[i * 2 + 1];
        }
    }

    pub fn snap_to_grapheme_boundaries(&mut self, text: &Rope) -> bool {
        let mut snapped = false;
        for range in &mut self.ranges {
            snapped |= range.snap_to_grapheme_boundaries(text);
        }
        snapped
    }

    #[must_use]
    pub fn save(&self, text: &Text) -> SelectionSnapshot {
        let ranges = self.ranges.iter().map(|range| range.save(text)).collect();
        let primary_range = self.primary_range;
        SelectionSnapshot {
            ranges,
            primary_range,
        }
    }
}

impl Default for SelectionState {
    fn default() -> Self {
        Self {
            ranges: vec![RangeState::default()],
            primary_range: 0,
        }
    }
}

pub struct SelectionSnapshot {
    pub ranges: Vec<RangeSnapshot>,
    pub primary_range: usize,
}

impl SelectionSnapshot {
    #[must_use]
    pub fn restore(&self, text: &Text) -> Option<SelectionState> {
        let ranges = self.ranges.iter().try_fold(
            Vec::with_capacity(self.ranges.len()),
            |mut ranges, range| {
                ranges.push(range.restore(text)?);
                Some(ranges)
            },
        )?;
        let primary_range = self.primary_range;
        Some(SelectionState {
            ranges,
            primary_range,
        })
    }
}

#[must_use]
pub struct SelectionView<'a, W: Wrap> {
    text: W::Wrap<'a, Text>,
    state: W::Wrap<'a, SelectionState>,
    #[expect(clippy::type_complexity)]
    on_drop: Option<Box<dyn FnOnce(&mut Self) + 'a>>,
}

pub type Selection<'a> = SelectionView<'a, WRef>;

pub type SelectionMut<'a> = SelectionView<'a, WMut>;

impl<'a, W: Wrap> SelectionView<'a, W> {
    pub fn on_drop(mut self, f: impl FnOnce(&mut Self) + 'a) -> Self {
        self.on_drop = Some(Box::new(f));
        self
    }
}

impl<'a, W: WrapRef> SelectionView<'a, W> {
    pub fn new(
        text: W::WrapRef<'a, Text>,
        state: W::WrapRef<'a, SelectionState>,
    ) -> anyhow::Result<Self> {
        let selection_view = SelectionView {
            text,
            state,
            on_drop: None,
        };
        selection_view.assert_invariants()?;
        Ok(selection_view)
    }

    pub fn text(&self) -> &Text {
        &self.text
    }

    pub fn state(&self) -> &SelectionState {
        &self.state
    }

    pub fn get(&self, index: usize) -> Option<Range<'_>> {
        let range_state = self.state.ranges.get(index)?;
        let range = Range::new(&self.text, range_state)
            .expect("Selection text and range state are always kept valid");
        Some(range)
    }

    pub fn get_primary(&self) -> Range<'_> {
        self.get(self.state.primary_range)
            .expect("Primary range index is always kept valid")
    }

    pub fn for_each(&self, mut f: impl FnMut(usize, Range<'_>)) {
        for (i, range_state) in self.state.ranges.iter().enumerate() {
            let range = Range::new(&self.text, range_state)
                .expect("Selection text and range state are always kept valid");
            f(i, range);
        }
    }

    #[expect(clippy::unnecessary_wraps)]
    #[expect(clippy::unused_self)]
    pub(crate) fn assert_invariants(&self) -> anyhow::Result<()> {
        Ok(())
    }
}

impl<W: WrapMut> SelectionView<'_, W> {
    fn unchecked_get_mut(&mut self, index: usize) -> Option<RangeMut<'_>> {
        let range_state = self.state.ranges.get_mut(index)?;
        let range = RangeMut::new(&mut self.text, range_state)
            .expect("Selection text and range state are always kept valid")
            .on_drop(|range| range.assert_invariants().unwrap());
        Some(range)
    }

    pub fn for_each_mut(&mut self, mut f: impl FnMut(RangeMut<'_>)) {
        for i in 0..self.state.ranges.len() {
            let version = self.text.version();
            let range = self.unchecked_get_mut(i).unwrap();
            f(range);
            if let Some(opss) = self.text.ops_since(version) {
                for j in 0..self.state.ranges.len() {
                    if i == j {
                        continue;
                    }
                    for ops in opss {
                        self.state.ranges[j].transform(ops);
                    }
                }
            }
        }
    }

    pub fn keep_primary(&mut self) {
        if self.state.primary_range != 0 {
            let index = self.state.primary_range;
            self.state.ranges.swap(0, index);
            self.state.primary_range = 0;
        }
        self.state.ranges.truncate(1);
    }

    pub fn rotate_primary_forward(&mut self, count: usize) {
        self.state.primary_range = (self.state.primary_range + count) % self.state.ranges.len();
    }

    pub fn rotate_primary_backward(&mut self, count: usize) {
        let length = self.state.ranges.len();
        self.state.primary_range = (self.state.primary_range + length - (count % length)) % length;
    }

    pub fn select_regex(&mut self, regex: &Regex) -> bool {
        let mut ranges = Vec::new();
        for range in &self.state.ranges {
            let start = min(range.tail.byte_offset, range.head.byte_offset);
            let end = max(range.tail.byte_offset, range.head.byte_offset);
            let input = regex_cursor::Input::new(RegexCursorInput::from(
                self.text.rope().slice(start..end),
            ));
            for needle in regex.find_iter(input) {
                ranges.push(RangeState {
                    tail: CursorState {
                        byte_offset: start + needle.start(),
                    },
                    head: CursorState {
                        byte_offset: start + needle.end(),
                    },
                    goal_column: 0,
                });
            }
        }
        let matched = !ranges.is_empty();
        if matched {
            self.state.primary_range = ranges.len() - 1;
            self.state.ranges = ranges;
        }
        matched
    }

    pub fn select_all(&mut self) {
        let mut range = RangeState::default();
        range.head.byte_offset = self.text.len();
        self.state.ranges = vec![range];
        self.state.primary_range = 0;
    }

    pub fn snap_to_grapheme_boundaries(&mut self) -> bool {
        self.state.snap_to_grapheme_boundaries(self.text.rope())
    }

    pub fn insert_char(&mut self, char: char) -> OperationSeq {
        self.insert(&char.to_string())
    }

    pub fn insert(&mut self, text: &str) -> OperationSeq {
        debug_assert!(
            self.state
                .ranges
                .is_sorted_by_key(|range| range.start().byte_offset),
            "this function relies on selection ranges' starts being sorted",
            // ...prior to it becoming a type-level invariant
        );
        let text: Arc<str> = Arc::from(text);
        let mut ops = OperationSeq::new();
        let mut previous = 0;
        for range in &self.state.ranges {
            // TODO: Assert grapheme length is 1 (i.e. reduced)
            ops.retain(range.start().byte_offset - previous);
            ops.insert(Arc::clone(&text));
            previous = range.start().byte_offset;
        }
        ops.retain_rest(&self.text);
        self.text.apply(&ops).expect("Operations are well formed");
        self.state.transform(&ops);
        if self.snap_to_grapheme_boundaries() {
            tracing::warn!("wasn't on grapheme boundary after");
        }
        ops
    }

    pub fn delete(&mut self) -> OperationSeq {
        debug_assert!(
            self.state
                .ranges
                .is_sorted_by_key(|range| range.start().byte_offset),
            "this function relies on selection ranges' starts being sorted",
            // ...prior to it becoming a type-level invariant
        );
        let mut ops = OperationSeq::new();
        let mut previous = 0;
        for range in &self.state.ranges {
            ops.retain(range.start().byte_offset - previous);
            ops.delete(range.byte_length());
            previous = range.end().byte_offset;
        }
        ops.retain_rest(&self.text);
        self.text.apply(&ops).expect("Operations are well formed");
        self.state.transform(&ops);
        if self.snap_to_grapheme_boundaries() {
            tracing::warn!("wasn't on grapheme boundary after");
        }
        for i in 0..self.state.ranges.len() {
            let mut range = self.unchecked_get_mut(i).unwrap();
            range.update_goal_column();
        }
        ops
    }
}

impl<W: Wrap> Drop for SelectionView<'_, W> {
    fn drop(&mut self) {
        if !thread::panicking()
            && let Some(f) = self.on_drop.take()
        {
            f(self);
        }
    }
}
