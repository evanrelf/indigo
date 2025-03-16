use crate::cursor_view::{Cursor, CursorState};
use indigo_wrap::{WMut, WRef, Wrap, WrapMut, WrapRef};
use ropey::Rope;

#[derive(Default)]
pub struct RangeState {
    pub anchor: CursorState,
    pub head: CursorState,
}

#[must_use]
pub struct RangeView<'a, W: Wrap> {
    text: W::Wrap<'a, Rope>,
    state: W::Wrap<'a, RangeState>,
}

pub type Range<'a> = RangeView<'a, WRef>;

pub type RangeMut<'a> = RangeView<'a, WMut>;

impl<'a, W: WrapRef> RangeView<'a, W> {
    pub fn new(text: W::WrapRef<'a, Rope>, state: W::WrapRef<'a, RangeState>) -> Option<Self> {
        let _ = Cursor::new(&text, &state.anchor)?;
        let _ = Cursor::new(&text, &state.head)?;
        let range_view = RangeView { text, state };
        // TODO: Validate things without panicking. `assert_invariants` will panic here instead of
        // short circuiting with a `None`. I should rename `assert_invariants` and make it return a
        // `Result` so it doubles as 1) validation in constructors like this, and 2) an assert when
        // unwrapped. Can use `thiserror`! Then `CursorView::new` can delegate the grapheme boundary
        // check to its own validator function.
        range_view.assert_invariants();
        Some(range_view)
    }

    pub fn anchor(&self) -> Cursor {
        Cursor::new(&self.text, &self.state.anchor).unwrap()
    }

    pub fn head(&self) -> Cursor {
        Cursor::new(&self.text, &self.state.head).unwrap()
    }

    pub(crate) fn assert_invariants(&self) {
        todo!()
    }
}

impl<W: WrapMut> RangeView<'_, W> {
    //
}
