use crate::{macros::key_matches, ui::Indigo};
use crossterm::event::Event;
use indigo_core::Editor;

#[derive(Default)]
pub struct Tui {
    pub editor: Editor,
}

pub enum ControlFlow {
    Continue,
    Quit,
}

impl Tui {
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
}
