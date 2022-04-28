#![warn(
    clippy::use_self,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss
)]

use crate::terminal::Terminal;
use indigo_core::Editor;
use tui::widgets::Widget;

mod terminal;

pub fn run(editor: Editor) {
    let mut terminal = Terminal::new();
    terminal.enter();

    let mut model = Model::new(editor);

    while !model.quit {
        terminal
            .draw(|frame| frame.render_widget(&model, frame.size()))
            .unwrap();

        let event = crossterm::event::read().unwrap();
        update(&mut model, event);
    }
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
        Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
            (KeyModifiers::CONTROL, KeyCode::Char('c')) => model.quit = true,
            (KeyModifiers::CONTROL, KeyCode::Char('p')) => panic!(),
            _ => {}
        },
        Event::Mouse(_) => {}
        Event::Resize(_, _) => {}
    }
}

fn view(model: &Model, area: tui::layout::Rect, buffer: &mut tui::buffer::Buffer) {
    // Tildes
    for y in area.top()..area.bottom() {
        buffer.get_mut(area.x, y).set_char('~');
    }
}
