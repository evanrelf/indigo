use crate::macros::key_matches;
use crossterm::event::Event;
use indigo_core::Editor;
use ratatui::{prelude::Frame, widgets::Paragraph};

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
        let area = frame.size();

        frame.render_widget(Paragraph::new(format!("{:#?}", self.editor)), area);
    }
}
