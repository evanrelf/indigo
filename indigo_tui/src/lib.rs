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

    let buffer = tui.editor.current_buffer_mut();

    match event {
        Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
            (KeyModifiers::CONTROL, KeyCode::Char('c')) => tui.quit = true,
            (KeyModifiers::CONTROL, KeyCode::Char('p')) => panic!(),
            (KeyModifiers::NONE, KeyCode::Up) => *buffer = buffer.scroll_up(1),
            (KeyModifiers::NONE, KeyCode::Down) => *buffer = buffer.scroll_down(1),
            (KeyModifiers::NONE, KeyCode::Left) => *buffer = buffer.scroll_left(1),
            (KeyModifiers::NONE, KeyCode::Right) => *buffer = buffer.scroll_right(1),
            _ => {}
        },
        Event::Mouse(_) => {}
        Event::Resize(_, _) => {}
    }
}

fn render(tui: &Tui, area: tui::layout::Rect, surface: &mut tui::buffer::Buffer) {
    use tui::layout::{Constraint, Direction, Layout};

    let vertical = Layout::default()
        .direction(Direction::Vertical)
        .constraints(
            [
                // number + buffer
                Constraint::Min(0),
                // status
                Constraint::Length(1),
                // command
                Constraint::Length(1),
            ]
            .as_ref(),
        )
        .split(area);

    let horizontal = Layout::default()
        .direction(Direction::Horizontal)
        .constraints(
            [
                // number
                Constraint::Length(4),
                // buffer
                Constraint::Min(0),
            ]
            .as_ref(),
        )
        .split(vertical[0]);

    let numbers_area = horizontal[0];
    let buffer_area = horizontal[1];
    let status_area = vertical[1];
    let command_area = vertical[2];

    render_numbers(tui, numbers_area, surface);
    render_buffer(tui, buffer_area, surface);
    render_status(tui, status_area, surface);
    render_command(tui, command_area, surface);
}

fn render_numbers(tui: &Tui, area: tui::layout::Rect, surface: &mut tui::buffer::Buffer) {
    use tui::style::Style;

    let buffer = tui.editor.current_buffer();

    let total_lines = buffer.contents().len_lines().saturating_sub(1);

    for y in area.top()..area.bottom() {
        let line_number = buffer.vertical_scroll_offset() + usize::from(y) + 1;

        if line_number <= total_lines {
            surface.set_stringn(
                area.x,
                y,
                format!("{:>3} ", line_number),
                usize::from(area.width),
                Style::default(),
            );
        } else {
            surface.get_mut(area.x, y).set_char('~');
        }
    }
}

fn render_buffer(_tui: &Tui, area: tui::layout::Rect, surface: &mut tui::buffer::Buffer) {
    use tui::style::Style;

    surface.set_string(area.x, area.y, "TODO buffer", Style::default());
}

fn render_status(_tui: &Tui, area: tui::layout::Rect, surface: &mut tui::buffer::Buffer) {
    use tui::style::Style;

    surface.set_string(area.x, area.y, "TODO status", Style::default());
}

fn render_command(_tui: &Tui, area: tui::layout::Rect, surface: &mut tui::buffer::Buffer) {
    use tui::style::Style;

    surface.set_string(area.x, area.y, "TODO command", Style::default());
}
