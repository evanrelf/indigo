use crate::editor;
use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};
use tui::widgets::Widget;

pub struct CommandLine {
    command: String,
}

impl CommandLine {
    pub fn new() -> Self {
        Self {
            command: String::new(),
        }
    }

    pub fn handle_event(&mut self, event: Event) -> Vec<editor::Operation> {
        use editor::Operation::*;

        match event {
            Event::Key(key_event) => {
                let KeyEvent { modifiers, code } = key_event;

                if modifiers == KeyModifiers::CONTROL {
                    match code {
                        KeyCode::Char('c') => {
                            self.command.clear();
                            vec![editor::Operation::ChangeMode(editor::Mode::Normal)]
                        }
                        _ => Vec::new(),
                    }
                } else if modifiers == KeyModifiers::NONE {
                    match code {
                        KeyCode::Esc => {
                            self.command.clear();
                            vec![editor::Operation::ChangeMode(editor::Mode::Normal)]
                        }
                        KeyCode::Char(c) => {
                            self.command.push(c);
                            Vec::new()
                        }
                        KeyCode::Backspace => match self.command.pop() {
                            Some(_) => Vec::new(),
                            None => {
                                self.command.clear();
                                vec![editor::Operation::ChangeMode(editor::Mode::Normal)]
                            }
                        },
                        KeyCode::Enter => {
                            self.command.clear();
                            vec![
                                ChangeMode(editor::Mode::Normal),
                                RunCommand(self.command.clone()),
                            ]
                        }
                        _ => Vec::new(),
                    }
                } else {
                    Vec::new()
                }
            }
            _ => Vec::new(),
        }
    }
}

impl Default for CommandLine {
    fn default() -> Self {
        Self::new()
    }
}

impl Widget for &CommandLine {
    fn render(self, area: tui::layout::Rect, buffer: &mut tui::buffer::Buffer) {
        use tui::style::Color;
        use tui::widgets::Block;

        Block::default()
            .title(format!(":{}", self.command))
            .render(area, buffer);

        buffer
            .get_mut(area.left() + self.command.len() as u16 + 1, area.top())
            .set_bg(Color::White);
    }
}
