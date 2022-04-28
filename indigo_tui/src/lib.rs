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
    let mut tui = Tui::new(editor);

    while !tui.quit {
        terminal
            .draw(|frame| frame.render_widget(&tui, frame.size()))
            .unwrap();

        let event = crossterm::event::read().unwrap();
        handle_event(&mut tui, event);
    }
}

struct Tui {
    editor: Editor,
    quit: bool,
}

impl Tui {
    pub fn new(editor: Editor) -> Self {
        Self {
            editor,
            quit: false,
        }
    }
}

impl Widget for &Tui {
    fn render(self, area: tui::layout::Rect, buffer: &mut tui::buffer::Buffer) {
        render(self, area, buffer);
    }
}

fn handle_event(tui: &mut Tui, event: crossterm::event::Event) {
    use crossterm::event::{Event, KeyCode, KeyModifiers};

    match event {
        Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
            (KeyModifiers::CONTROL, KeyCode::Char('c')) => tui.quit = true,
            (KeyModifiers::CONTROL, KeyCode::Char('p')) => panic!(),
            _ => {}
        },
        Event::Mouse(_) => {}
        Event::Resize(_, _) => {}
    }
}

fn render(tui: &Tui, area: tui::layout::Rect, buffer: &mut tui::buffer::Buffer) {
    // Tildes
    for y in area.top()..area.bottom() {
        buffer.get_mut(area.x, y).set_char('~');
    }
}
