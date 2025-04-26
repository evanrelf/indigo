use crate::{
    areas::Areas,
    widgets::{CommandBar, LineNumbers, NavigationBar, Selection, StatusBar, Text, Tildes},
};
use ratatui::prelude::{Buffer as Surface, *};

pub struct Editor<'a> {
    pub editor: &'a indigo_core::prelude::Editor,
}

impl<'a> Editor<'a> {
    #[must_use]
    pub fn new(editor: &'a indigo_core::prelude::Editor) -> Self {
        Self { editor }
    }
}

impl Widget for Editor<'_> {
    #[tracing::instrument(skip_all)]
    fn render(self, area: Rect, surface: &mut Surface) {
        let areas = Areas::new(self.editor, area);
        NavigationBar::new(self.editor).render(areas.navigation_bar, surface);
        LineNumbers::new(self.editor).render(areas.line_numbers, surface);
        Tildes::new(self.editor).render(areas.line_numbers, surface);
        Text::new(self.editor).render(areas.text, surface);
        Selection::new(self.editor).render(areas.text, surface);
        CommandBar::new(self.editor).render(areas.command_bar, surface);
        StatusBar::new(self.editor).render(areas.status_bar, surface);
    }
}
