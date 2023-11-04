use crate::macros::key_matches;
use anyhow::Context as _;
use crossterm::event::Event;
use ratatui::{
    prelude::{CrosstermBackend, Terminal},
    widgets::Paragraph,
};
use std::{cell::RefCell, io::Stdout};

pub struct Tui {
    pub terminal: RefCell<Terminal<CrosstermBackend<Stdout>>>,
    pub mouse_position: (u16, u16),
}

pub enum ControlFlow {
    Continue,
    Quit,
}

impl Tui {
    pub fn new() -> anyhow::Result<Self> {
        let stdout = std::io::stdout();
        let backend = CrosstermBackend::new(stdout);
        let terminal = Terminal::new(backend).context("Failed to create ratatui terminal")?;
        Ok(Self {
            terminal: RefCell::new(terminal),
            mouse_position: (0, 0),
        })
    }

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

    pub fn view(&self) -> anyhow::Result<()> {
        let mut terminal = self.terminal.borrow_mut();

        terminal.draw(|frame| {
            let area = frame.size();
            frame.render_widget(
                Paragraph::new(format!("mouse: {:?}", self.mouse_position)),
                area,
            );
        })?;

        Ok(())
    }
}
