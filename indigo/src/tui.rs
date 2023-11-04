use crate::macros::key_matches;
use crossterm::event::Event;
use ratatui::{prelude::Frame, widgets::Paragraph};

#[derive(Default)]
pub struct Tui {
    pub mouse_position: (u16, u16),
}

pub enum ControlFlow {
    Continue,
    Quit,
}

impl Tui {
    pub fn update(&mut self, event: Event) -> anyhow::Result<ControlFlow> {
        match event {
            Event::Mouse(mouse_event) => {
                self.mouse_position = (mouse_event.row, mouse_event.column);
            }
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
        let area = frame.size();

        frame.render_widget(
            Paragraph::new(format!("mouse: {:?}", self.mouse_position)),
            area,
        );
    }
}
