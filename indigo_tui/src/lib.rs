#![warn(
    clippy::use_self,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss
)]

use crate::terminal::with_terminal;
use indigo_core::editor::Editor;
use tui::widgets::Widget;

pub mod terminal;

pub fn run(mut editor: Editor) {
    with_terminal(|terminal| loop {
        terminal
            .draw(|frame| frame.render_widget(EditorWidget(&editor), frame.size()))
            .unwrap();

        let event = crossterm::event::read().unwrap();
        handle_event(&mut editor, event);
    });
}

pub struct EditorWidget<'editor>(pub &'editor Editor);

impl Widget for EditorWidget<'_> {
    fn render(self, area: tui::layout::Rect, buffer: &mut tui::buffer::Buffer) {
        render_editor(self.0, area, buffer);
    }
}

pub fn render_editor(editor: &Editor, area: tui::layout::Rect, buffer: &mut tui::buffer::Buffer) {
    todo!();
}

pub fn handle_event(editor: &mut Editor, event: crossterm::event::Event) {
    todo!();
}
