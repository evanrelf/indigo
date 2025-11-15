use indigo_wrap::{WMut, WRef, Wrap, WrapMut, WrapRef};

#[derive(Default)]
pub struct WindowState {
    pub height: u16,
}

#[must_use]
pub struct WindowView<'a, W: Wrap> {
    state: W::Wrap<'a, WindowState>,
}

pub type Window<'a> = WindowView<'a, WRef>;

pub type WindowMut<'a> = WindowView<'a, WMut>;

impl<'a, W: WrapRef> WindowView<'a, W> {
    pub fn new(state: W::WrapRef<'a, WindowState>) -> Self {
        WindowView { state }
    }

    #[must_use]
    pub fn height(&self) -> u16 {
        self.state.height
    }
}

impl<W: WrapMut> WindowView<'_, W> {
    pub fn set_height(&mut self, height: u16) {
        self.state.height = height;
    }
}
