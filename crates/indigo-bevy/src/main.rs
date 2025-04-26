#![allow(clippy::needless_pass_by_value)]

use bevy::prelude::*;
use clap::Parser as _;
use ratatui::crossterm;
use std::time::Duration;

#[derive(clap::Parser, Resource)]
struct Args {}

#[derive(Deref, DerefMut, Resource)]
struct Terminal(ratatui::DefaultTerminal);

#[derive(Deref, DerefMut, Resource)]
struct Text(String);

#[derive(Deref, DerefMut, Event)]
struct TerminalEvent(crossterm::event::Event);

fn main() {
    let args = Args::parse();

    let terminal = Terminal(ratatui::init());

    let text = Text(String::from("Hello, world!"));

    App::new()
        .add_plugins(MinimalPlugins)
        .insert_resource(args)
        .insert_resource(terminal)
        .insert_resource(text)
        .add_event::<TerminalEvent>()
        .add_systems(
            Update,
            (
                render_system,
                (
                    read_input_system,
                    handle_input_system.run_if(on_event::<TerminalEvent>),
                )
                    .chain(),
            ),
        )
        .run();

    ratatui::restore();
}

fn render_system(text: Res<Text>, mut terminal: ResMut<Terminal>) -> Result {
    use ratatui::text::Text;

    terminal.draw(|frame| {
        let text = Text::raw(&**text);
        frame.render_widget(text, frame.area());
    })?;

    Ok(())
}

fn read_input_system(mut terminal_event: EventWriter<TerminalEvent>) -> Result {
    use crossterm::event;

    // TODO: `crossterm` says `poll` and `read` have to always run on the same thread, but neither
    // Bevy nor I am guaranteeing this here!
    while event::poll(Duration::ZERO)? {
        let event = event::read()?;
        terminal_event.write(TerminalEvent(event));
    }

    Ok(())
}

fn handle_input_system(
    mut text: ResMut<Text>,
    mut terminal_event: EventReader<TerminalEvent>,
    mut exit_event: EventWriter<AppExit>,
) {
    use crossterm::event::{Event, KeyCode, KeyModifiers};

    for TerminalEvent(event) in terminal_event.read() {
        #[expect(clippy::single_match)]
        match event {
            Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
                (m, KeyCode::Char('c')) if m == KeyModifiers::CONTROL => {
                    exit_event.write_default();
                }
                (_, KeyCode::Char(c)) => {
                    text.push(c);
                }
                (m, KeyCode::Backspace) if m == KeyModifiers::NONE => {
                    text.pop();
                }
                _ => {}
            },
            _ => {}
        }
    }
}
