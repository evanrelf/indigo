use crate::{
    cursor::CursorState,
    ot::OperationSeq,
    range::{Range, RangeMut, RangeSnapshot, RangeState},
    rope::{LINE_TYPE, RegexCursorInput, RopeExt as _},
    text::Text,
};
use indigo_wrap::{WMut, WRef, Wrap, WrapMut, WrapRef};
use regex_cursor::engines::meta::Regex;
use ropey::Rope;
use std::{sync::Arc, thread};
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
    pub fn transform(&mut self, ops: &OperationSeq, text: &Rope) {
        let mut offsets: Vec<usize> = self
            .ranges
            .iter()
            .flat_map(|range| [range.tail.byte_index, range.head.byte_index])
            .collect();

        ops.transform_byte_offsets_unsorted(&mut offsets);

        for (i, range) in self.ranges.iter_mut().enumerate() {
            range.tail.byte_index = text
                .snap_to_grapheme_start(offsets[i * 2])
                .expect("Text is never empty");
            range.head.byte_index = text
                .snap_to_grapheme_start(offsets[i * 2 + 1])
                .expect("Text is never empty");
        }
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

    /// The exclusive byte offset one past a range's last grapheme.
    fn end_exclusive(&self, range: &RangeState) -> usize {
        self.text
            .next_grapheme_boundary(range.end().byte_index)
            .expect("Range end is always on a grapheme")
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
                        self.state.ranges[j].transform(ops, &self.text);
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

    /// Select every regex match within each range. Empty (zero-width) matches become
    /// one-grapheme selections at the match position, as in Kakoune. Returns `false` and leaves
    /// the selection unmodified when nothing matched.
    pub fn select_regex(&mut self, regex: &Regex) -> bool {
        let mut ranges = Vec::new();
        for range_state in &self.state.ranges {
            let rope = self.text.rope();
            let start = range_state.start().byte_index;
            let end_exclusive = rope
                .next_grapheme_boundary(range_state.end().byte_index)
                .expect("Range end is always on a grapheme");
            let slice_length = end_exclusive - start;
            let input =
                regex_cursor::Input::new(RegexCursorInput::from(rope.slice(start..end_exclusive)));
            for needle in regex.find_iter(input) {
                // Skip an empty match one past the searched slice (Kakoune does the same);
                // there is no grapheme there to select.
                if needle.start() >= slice_length {
                    continue;
                }
                let tail_index = rope.floor_grapheme_boundary(start + needle.start());
                let head_index = if needle.start() == needle.end() {
                    // An inclusive range can't be zero-width; select the grapheme at the
                    // match position.
                    tail_index
                } else {
                    // The inclusive end is the last grapheme the match touches.
                    rope.prev_grapheme_boundary(start + needle.end())
                        .unwrap_or(0)
                        .max(tail_index)
                };
                // Preserve the original range's direction, like Kakoune's `keep_direction`.
                let (tail, head) = if range_state.is_forward() {
                    (tail_index, head_index)
                } else {
                    (head_index, tail_index)
                };
                ranges.push(RangeState {
                    tail: CursorState { byte_index: tail },
                    head: CursorState { byte_index: head },
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
        range.head.byte_index = self
            .text
            .last_grapheme_start()
            .expect("Text is never empty");
        self.state.ranges = vec![range];
        self.state.primary_range = 0;
    }

    pub fn split_into_lines(&mut self) {
        self.split_at(|range| {
            let start = range.start().byte_index();
            let end = range.end().byte_index();

            let start_line = range.text().byte_to_line_idx(start, LINE_TYPE);
            let end_line = range.text().byte_to_line_idx(end, LINE_TYPE);

            ((start_line + 1)..=end_line)
                .map(|line| range.text().line_to_byte_idx(line, LINE_TYPE))
                .collect()
        });
    }

    /// Split each range at the returned cut points (exclusive byte offsets strictly inside the
    /// range). A segment ending at a cut point covers up to the grapheme before it.
    fn split_at(&mut self, mut f: impl FnMut(Range<'_>) -> Vec<usize>) {
        let old_primary_range = self.state.primary_range;
        let old_primary_head = self.state.ranges[old_primary_range].head.byte_index;
        let mut primary_range = 0;
        let mut ranges = Vec::new();

        for range_state in &self.state.ranges {
            let range = Range::new(&self.text, range_state)
                .expect("Selection text and range state are always kept valid");

            let start = range.start().byte_index();
            let end = range.end().byte_index();

            let mut boundaries = f(range);
            boundaries.sort_unstable();
            boundaries.dedup();
            boundaries.retain(|boundary| start < *boundary && *boundary <= end);

            let mut segment_start = start;
            for boundary in boundaries {
                let segment_end = self
                    .text
                    .prev_grapheme_boundary(boundary)
                    .expect("Cut point is after the range start");
                ranges.push(range_state.with_bounds(segment_start, segment_end));
                segment_start = boundary;
            }
            ranges.push(range_state.with_bounds(segment_start, end));
        }

        for (i, range) in ranges.iter().enumerate() {
            if range.head.byte_index == old_primary_head {
                primary_range = i;
                break;
            }
        }

        self.state.ranges = ranges;
        self.state.primary_range = primary_range;
    }

    pub fn insert_char(&mut self, char: char) -> OperationSeq {
        self.insert(&char.to_string())
    }

    pub fn insert(&mut self, text: &str) -> OperationSeq {
        debug_assert!(
            self.state
                .ranges
                .is_sorted_by_key(|range| range.start().byte_index),
            "this function relies on selection ranges' starts being sorted",
            // ...prior to it becoming a type-level invariant
        );
        let text: Arc<str> = Arc::from(text);
        let mut ops = OperationSeq::new();
        let mut previous = 0;
        for range in &self.state.ranges {
            // TODO: Assert grapheme length is 1 (i.e. reduced)
            ops.retain(range.start().byte_index - previous);
            ops.insert(Arc::clone(&text));
            previous = range.start().byte_index;
        }
        ops.retain_rest(&self.text);
        self.text.apply(&ops).expect("Operations are well formed");
        self.state.transform(&ops, &self.text);
        self.update_goal_columns();
        ops
    }

    /// Replace each selected grapheme with the given character. Newlines are replaced too, as in
    /// Kakoune's `r`; replacing the text's final newline re-inserts one after it.
    pub fn replace_each(&mut self, byte: u8) -> OperationSeq {
        debug_assert!(
            self.state
                .ranges
                .is_sorted_by_key(|range| range.start().byte_index),
            "this function relies on selection ranges' starts being sorted",
            // ...prior to it becoming a type-level invariant
        );
        let replacement: Arc<str> = Arc::from(char::from(byte).to_string());
        let mut ops = OperationSeq::new();
        let mut previous = 0;
        for range in &self.state.ranges {
            let start = range.start().byte_index;
            let end_exclusive = self
                .text
                .next_grapheme_boundary(range.end().byte_index)
                .expect("Range end is always on a grapheme");
            ops.retain(start - previous);
            for grapheme in self.text.rope().slice(start..end_exclusive).graphemes() {
                ops.delete(grapheme.len());
                ops.insert(Arc::clone(&replacement));
            }
            if end_exclusive == self.text.len() {
                // The final newline was replaced; restore the `Text` invariant.
                ops.insert("\n");
            }
            previous = end_exclusive;
        }
        ops.retain_rest(&self.text);
        self.text.apply(&ops).expect("Operations are well formed");
        self.state.transform(&ops, &self.text);
        self.update_goal_columns();
        ops
    }

    /// Delete the grapheme before each range's start.
    pub fn delete_before(&mut self) -> OperationSeq {
        debug_assert!(
            self.state
                .ranges
                .is_sorted_by_key(|range| range.start().byte_index),
            "this function relies on selection ranges' starts being sorted",
            // ...prior to it becoming a type-level invariant
        );
        let mut ops = OperationSeq::new();
        let mut previous = 0;
        for range in &self.state.ranges {
            let start = range.start().byte_index;
            if let Some(prev_boundary) = self.text.prev_grapheme_boundary(start)
                && prev_boundary >= previous
            {
                ops.retain(prev_boundary - previous);
                ops.delete(start - prev_boundary);
                previous = start;
            }
        }
        ops.retain_rest(&self.text);
        self.text.apply(&ops).expect("Operations are well formed");
        self.state.transform(&ops, &self.text);
        self.update_goal_columns();
        ops
    }

    /// Delete each range's graphemes. Deleting through the end of the text re-inserts the
    /// invariant trailing newline.
    pub fn delete(&mut self) -> OperationSeq {
        debug_assert!(
            self.state
                .ranges
                .is_sorted_by_key(|range| range.start().byte_index),
            "this function relies on selection ranges' starts being sorted",
            // ...prior to it becoming a type-level invariant
        );
        let spans: Vec<(usize, usize)> = self
            .state
            .ranges
            .iter()
            .map(|range| (range.start().byte_index, self.end_exclusive(range)))
            .collect();
        let mut ops = OperationSeq::new();
        let mut previous = 0;
        for (start, end_exclusive) in &spans {
            ops.retain(start - previous);
            ops.delete(end_exclusive - start);
            previous = *end_exclusive;
        }
        // If the deletions reach the end of the text, the last surviving byte must still be a
        // newline; otherwise re-insert one. Walk back through ranges that abut each other to
        // find the last surviving byte.
        if let Some(&(_, last_end_exclusive)) = spans.last()
            && last_end_exclusive == self.text.len()
        {
            let mut position = spans[spans.len() - 1].0;
            for &(start, end_exclusive) in spans[..spans.len() - 1].iter().rev() {
                if end_exclusive == position {
                    position = start;
                } else {
                    break;
                }
            }
            if position == 0 || self.text.byte(position - 1) != b'\n' {
                ops.insert("\n");
            }
        }
        ops.retain_rest(&self.text);
        self.text.apply(&ops).expect("Operations are well formed");
        self.state.transform(&ops, &self.text);
        self.update_goal_columns();
        ops
    }

    /// Delete the grapheme under each range's end cursor, unless it is the text's final newline.
    pub fn delete_after(&mut self) -> OperationSeq {
        debug_assert!(
            self.state
                .ranges
                .is_sorted_by_key(|range| range.start().byte_index),
            "this function relies on selection ranges' starts being sorted",
            // ...prior to it becoming a type-level invariant
        );
        let mut ops = OperationSeq::new();
        let mut previous = 0;
        for range in &self.state.ranges {
            let end = range.end().byte_index;
            let next_boundary = self
                .text
                .next_grapheme_boundary(end)
                .expect("Range end is always on a grapheme");
            if next_boundary == self.text.len() {
                // Deleting the final newline would break the `Text` invariant.
                continue;
            }
            if end >= previous {
                ops.retain(end - previous);
                ops.delete(next_boundary - end);
                previous = next_boundary;
            }
        }
        ops.retain_rest(&self.text);
        self.text.apply(&ops).expect("Operations are well formed");
        self.state.transform(&ops, &self.text);
        self.update_goal_columns();
        ops
    }

    fn update_goal_columns(&mut self) {
        for i in 0..self.state.ranges.len() {
            let mut range = self.unchecked_get_mut(i).unwrap();
            range.update_goal_column();
        }
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

#[cfg(test)]
mod tests {
    use super::*;

    fn range(tail: usize, head: usize) -> RangeState {
        RangeState {
            tail: CursorState { byte_index: tail },
            head: CursorState { byte_index: head },
            goal_column: 0,
        }
    }

    fn split_ranges(text: &str, ranges: Vec<RangeState>) -> SelectionState {
        let mut text = Text::from(text);
        let mut state = SelectionState {
            ranges,
            primary_range: 0,
        };
        SelectionMut::new(&mut text, &mut state)
            .unwrap()
            .split_into_lines();
        state
    }

    #[test]
    fn split_into_lines_keeps_partial_endpoints() {
        // Selects from "b" through the "o" on the third line (inclusive).
        let state = split_ranges("abcdef\nghijkl\nmnopqr\n", vec![range(1, 15)]);

        assert_eq!(state.primary_range, 2);
        assert_eq!(state.ranges.len(), 3);
        assert_eq!(state.ranges[0].tail.byte_index, 1);
        assert_eq!(state.ranges[0].head.byte_index, 6);
        assert_eq!(state.ranges[1].tail.byte_index, 7);
        assert_eq!(state.ranges[1].head.byte_index, 13);
        assert_eq!(state.ranges[2].tail.byte_index, 14);
        assert_eq!(state.ranges[2].head.byte_index, 15);
    }

    #[test]
    fn split_into_lines_preserves_backward_direction() {
        let state = split_ranges("abcdef\nghijkl\nmnopqr\n", vec![range(15, 1)]);

        assert_eq!(state.primary_range, 0);
        assert_eq!(state.ranges.len(), 3);
        assert_eq!(state.ranges[0].tail.byte_index, 6);
        assert_eq!(state.ranges[0].head.byte_index, 1);
        assert_eq!(state.ranges[1].tail.byte_index, 13);
        assert_eq!(state.ranges[1].head.byte_index, 7);
        assert_eq!(state.ranges[2].tail.byte_index, 15);
        assert_eq!(state.ranges[2].head.byte_index, 14);
    }

    #[test]
    fn replace_each_replaces_newlines() {
        // Kakoune's `r` replaces newlines too; replacing the final newline re-inserts one.
        let mut text = Text::from("ab\ncd\n");
        let mut state = SelectionState::default();
        let mut selection = SelectionMut::new(&mut text, &mut state).unwrap();
        selection.select_all();
        selection.replace_each(b'X');
        drop(selection);
        assert_eq!(&text.to_string(), "XXXXXX\n");
    }

    #[test]
    fn delete_abutting_ranges_keeps_trailing_newline() {
        // Two abutting ranges together delete through the end of the text ("\nb\n" in total);
        // the last surviving byte ('a') is not a newline, so one is re-inserted.
        let mut text = Text::from("a\nb\n");
        let mut state = SelectionState {
            ranges: vec![range(1, 1), range(2, 3)],
            primary_range: 0,
        };
        let mut selection = SelectionMut::new(&mut text, &mut state).unwrap();
        selection.delete();
        drop(selection);
        assert_eq!(&text.to_string(), "a\n");
    }

    #[test]
    fn select_regex_empty_matches_select_one_grapheme() {
        // `x*` matches empty at every position; each empty match becomes a one-grapheme
        // selection, and the empty match at the end of the searched slice is skipped.
        let regex = Regex::new("x*").unwrap();
        let mut text = Text::from("abc\n");
        let mut state = SelectionState::default();
        let mut selection = SelectionMut::new(&mut text, &mut state).unwrap();
        selection.select_all();
        assert!(selection.select_regex(&regex));
        drop(selection);
        let positions: Vec<(usize, usize)> = state
            .ranges
            .iter()
            .map(|range| (range.tail.byte_index, range.head.byte_index))
            .collect();
        assert_eq!(positions, vec![(0, 0), (1, 1), (2, 2), (3, 3)]);
    }

    #[test]
    fn select_regex_no_match_leaves_selection_unmodified() {
        let regex = Regex::new("xyz").unwrap();
        let mut text = Text::from("abc\n");
        let mut state = SelectionState::default();
        let mut selection = SelectionMut::new(&mut text, &mut state).unwrap();
        selection.select_all();
        assert!(!selection.select_regex(&regex));
        drop(selection);
        assert_eq!(state.ranges.len(), 1);
        assert_eq!(state.ranges[0].tail.byte_index, 0);
        assert_eq!(state.ranges[0].head.byte_index, 3);
    }

    #[test]
    fn select_regex_match_end_is_inclusive() {
        let regex = Regex::new("o+").unwrap();
        let mut text = Text::from("foo bar\n");
        let mut state = SelectionState::default();
        let mut selection = SelectionMut::new(&mut text, &mut state).unwrap();
        selection.select_all();
        assert!(selection.select_regex(&regex));
        drop(selection);
        assert_eq!(state.ranges.len(), 1);
        assert_eq!(state.ranges[0].tail.byte_index, 1);
        assert_eq!(state.ranges[0].head.byte_index, 2);
    }

    #[test]
    fn split_into_lines_splits_full_lines() {
        // Selecting everything (each line's newline included) splits into whole lines.
        let state = split_ranges("a\nb\nc\n", vec![range(0, 5)]);

        assert_eq!(state.primary_range, 2);
        assert_eq!(state.ranges.len(), 3);
        assert_eq!(state.ranges[0].tail.byte_index, 0);
        assert_eq!(state.ranges[0].head.byte_index, 1);
        assert_eq!(state.ranges[1].tail.byte_index, 2);
        assert_eq!(state.ranges[1].head.byte_index, 3);
        assert_eq!(state.ranges[2].tail.byte_index, 4);
        assert_eq!(state.ranges[2].head.byte_index, 5);
    }
}
