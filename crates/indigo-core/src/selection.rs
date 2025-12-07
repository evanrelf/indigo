use crate::{
    cursor::CursorState,
    range::{Range, RangeMut, RangeState},
    rope::RegexCursorInput,
    text::Text,
};
use indigo_wrap::{WMut, WRef, Wrap, WrapMut, WrapRef};
use regex_cursor::engines::meta::Regex;
use std::{
    cmp::{max, min},
    thread,
};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Error from range")]
    Range(#[source] anyhow::Error),
}

pub struct SelectionState {
    pub ranges: Vec<RangeState>,
    pub primary_range: usize,
}

impl Default for SelectionState {
    fn default() -> Self {
        Self {
            ranges: vec![RangeState::default()],
            primary_range: 0,
        }
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

    pub fn for_each(&self, mut f: impl FnMut(Range<'_>)) {
        for range_state in &self.state.ranges {
            let range = Range::new(&self.text, range_state)
                .expect("Selection text and range state are always kept valid");
            f(range);
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
            if let Some(edits) = self.text.edits_since(version) {
                for j in 0..self.state.ranges.len() {
                    if i == j {
                        continue;
                    }
                    for edit in edits {
                        self.state.ranges[j].transform(edit);
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

    pub fn select_regex(&mut self, regex: &Regex) -> bool {
        let mut ranges = Vec::new();
        for range in &self.state.ranges {
            let start = min(range.anchor.byte_offset, range.head.byte_offset);
            let end = max(range.anchor.byte_offset, range.head.byte_offset);
            let input = regex_cursor::Input::new(RegexCursorInput::from(
                self.text.rope().slice(start..end),
            ));
            for needle in regex.find_iter(input) {
                ranges.push(RangeState {
                    anchor: CursorState {
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
