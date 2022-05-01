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
    #[allow(dead_code)]
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
    use tui::layout::{Constraint, Direction, Layout};

    let vertical = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            // number + buffer
            Constraint::Min(0),
            // status
            Constraint::Length(1),
            // command
            Constraint::Length(1),
        ].as_ref())
        .split(area);

    let horizontal = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            // number
            Constraint::Length(4),
            // buffer
            Constraint::Min(0),
        ].as_ref())
        .split(vertical[0]);

    let numbers_area = horizontal[0];
    let buffer_area = horizontal[1];
    let status_area = vertical[1];
    let command_area = vertical[2];

    render_numbers(tui, numbers_area, buffer);
    render_buffer(tui, buffer_area, buffer);
    render_status(tui, status_area, buffer);
    render_command(tui, command_area, buffer);
}

fn render_numbers(_tui: &Tui, area: tui::layout::Rect, buffer: &mut tui::buffer::Buffer) {
    // Tildes
    for y in area.top()..area.bottom() {
        buffer.get_mut(area.x, y).set_char('~');
    }
}

fn render_buffer(_tui: &Tui, _area: tui::layout::Rect, _buffer: &mut tui::buffer::Buffer) {
    // TODO
}

fn render_status(_tui: &Tui, _area: tui::layout::Rect, _buffer: &mut tui::buffer::Buffer) {
    // TODO
}

fn render_command(_tui: &Tui, _area: tui::layout::Rect, _buffer: &mut tui::buffer::Buffer) {
    // TODO
}
