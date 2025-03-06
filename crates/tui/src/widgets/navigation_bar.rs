use indigo_core::prelude::*;
use ratatui::prelude::{Buffer as Surface, *};

pub struct NavigationBar<'a> {
    pub editor: &'a Editor,
}

impl<'a> NavigationBar<'a> {
    #[must_use]
    pub fn new(editor: &'a Editor) -> Self {
        Self { editor }
    }
}

impl Widget for NavigationBar<'_> {
    #[tracing::instrument(skip_all)]
    fn render(self, area: Rect, surface: &mut Surface) {
        Line::styled(" ", Modifier::UNDERLINED).render(area, surface);
    }
}
