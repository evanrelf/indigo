use bevy::prelude::*;
use std::time::Duration;

#[derive(Deref, DerefMut, Resource)]
pub struct Terminal(pub ratatui::DefaultTerminal);

impl Terminal {
    pub fn read_input_system(mut terminal_event: EventWriter<TerminalEvent>) -> Result {
        use crossterm::event;

        // TODO: `crossterm` says `poll` and `read` have to always run on the same thread, but neither
        // Bevy nor I am guaranteeing this here!
        while event::poll(Duration::ZERO)? {
            let event = event::read()?;
            terminal_event.write(TerminalEvent(event));
        }

        Ok(())
    }
}

impl Drop for Terminal {
    fn drop(&mut self) {
        ratatui::restore();
    }
}

#[derive(Deref, DerefMut, Event)]
pub struct TerminalEvent(pub crossterm::event::Event);

pub struct TerminalPlugin;

impl Plugin for TerminalPlugin {
    fn build(&self, app: &mut App) {
        let terminal = ratatui::init();
        app.insert_resource(Terminal(terminal))
            .add_event::<TerminalEvent>()
            .add_systems(PreUpdate, Terminal::read_input_system);
    }
}
