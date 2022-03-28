use crate::editor::{self, Event};
use crossterm::event::{KeyCode, KeyModifiers};

#[derive(Default)]
pub struct CommandLine {
    command: String,
}

impl CommandLine {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn command(&self) -> &String {
        &self.command
    }

    pub fn handle_event(&mut self, event: Event) -> Vec<editor::Operation> {
        use editor::{Event::*, Mode::*, Operation::*};

        match event {
            Key(key_event) if key_event.modifiers == KeyModifiers::CONTROL => {
                match key_event.code {
                    KeyCode::Char('c') => {
                        self.command.clear();
                        vec![ChangeMode(Normal)]
                    }
                    _ => Vec::new(),
                }
            }
            Key(key_event) if key_event.modifiers == KeyModifiers::NONE => match key_event.code {
                KeyCode::Esc => {
                    self.command.clear();
                    vec![ChangeMode(Normal)]
                }
                KeyCode::Char(c) => {
                    self.command.push(c);
                    Vec::new()
                }
                KeyCode::Backspace => match self.command.pop() {
                    Some(_) => Vec::new(),
                    None => {
                        self.command.clear();
                        vec![ChangeMode(Normal)]
                    }
                },
                KeyCode::Enter => {
                    let operations = self.run_command();
                    self.command.clear();
                    operations
                }
                _ => Vec::new(),
            },
            Key(_) | Mouse(_) => Vec::new(),
        }
    }

    fn run_command(&self) -> Vec<editor::Operation> {
        use editor::{Mode::*, Operation::*};

        match self.command.as_str() {
            "quit" | "q" => {
                vec![Quit]
            }
            "" => vec![ChangeMode(Normal)],
            _ => {
                unimplemented!("Unknown command: {}", self.command);
            }
        }
    }
}
