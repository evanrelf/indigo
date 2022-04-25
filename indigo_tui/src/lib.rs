#![warn(
    clippy::use_self,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss
)]

use crate::terminal::with_terminal;
use indigo_core::editor::Editor;
use tui::widgets::Widget;

mod terminal;

pub fn run(editor: Editor) {
    let mut model = Model::new(editor);

    with_terminal(|terminal| {
        while !model.quit {
            terminal
                .draw(|frame| frame.render_widget(&model, frame.size()))
                .unwrap();

            let event = crossterm::event::read().unwrap();
            update(&mut model, event);
        }
    });
}

struct Model {
    editor: Editor,
    quit: bool,
}

impl Model {
    pub fn new(editor: Editor) -> Self {
        Self {
            editor,
            quit: false,
        }
    }
}

impl Widget for &Model {
    fn render(self, area: tui::layout::Rect, buffer: &mut tui::buffer::Buffer) {
        view(self, area, buffer);
    }
}

fn update(model: &mut Model, event: crossterm::event::Event) {
    use crossterm::event::{Event, KeyCode, KeyModifiers};

    match event {
        #[allow(clippy::single_match)]
        Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
            (KeyModifiers::CONTROL, KeyCode::Char('c')) => model.quit = true,
            _ => {}
        },
        Event::Mouse(_) => {}
        Event::Resize(_, _) => {}
    }
}

fn view(model: &Model, area: tui::layout::Rect, buffer: &mut tui::buffer::Buffer) {
    // TODO
}
