use crate::macros::key_matches;
use crossterm::event::Event;
use indigo_core::Editor;
use ratatui::{prelude::*, widgets::Paragraph};

#[derive(Default)]
pub struct Tui {
    editor: Editor,
}

pub enum ControlFlow {
    Continue,
    Quit,
}

impl Tui {
    pub fn new(editor: Editor) -> Self {
        Self { editor }
    }

    pub fn update(&mut self, event: Event) -> anyhow::Result<ControlFlow> {
        match event {
            Event::Key(key) if key_matches!(key, CONTROL 'p') => {
                panic!()
            }
            Event::Key(key) if key_matches!(key, CONTROL 'c') => {
                anyhow::bail!("Ctrl-C");
            }
            Event::Key(key) if key_matches!(key, 'q') => {
                return Ok(ControlFlow::Quit);
            }
            _ => {}
        }

        Ok(ControlFlow::Continue)
    }

    pub fn view(&self, frame: &mut Frame) {
        let areas = areas(frame.size());
        frame.render_widget(Paragraph::new("~"), areas.number);
        frame.render_widget(Paragraph::new("Hello, world!"), areas.buffer);
        frame.render_widget(Paragraph::new("status"), areas.status);
        frame.render_widget(Paragraph::new("command"), areas.command);
    }
}

struct Areas {
    number: Rect,
    buffer: Rect,
    status: Rect,
    command: Rect,
}

fn areas(area: Rect) -> Areas {
    let vertical = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            // number + buffer
            Constraint::Min(0),
            // status
            Constraint::Length(1),
            // command
            Constraint::Length(1),
        ])
        .split(area);

    let number_width = 3; // TODO: Calculate from `Editor`

    let horizontal = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            // number
            Constraint::Length(number_width),
            // buffer
            Constraint::Min(0),
        ])
        .split(vertical[0]);

    Areas {
        number: horizontal[0],
        buffer: horizontal[1],
        status: vertical[1],
        command: vertical[2],
    }
}
