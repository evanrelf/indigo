use crate::{
    range::{Range, RangeMut, RangeState},
    text::Text,
};
use indigo_wrap::{WMut, WRef, Wrap, WrapMut, WrapRef};
use std::thread;
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
    pub fn for_each_mut(&mut self, mut f: impl FnMut(RangeMut<'_>)) {
        for range_state in &mut self.state.ranges {
            let range = RangeMut::new(&mut self.text, range_state)
                .expect("Selection text and range state are always kept valid");
            f(range);
        }
    }

    pub fn select_all(&mut self) {
        let mut range = RangeState::default();
        range.head.char_offset = self.text.len_chars();
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
