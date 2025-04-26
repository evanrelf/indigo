#![allow(clippy::needless_pass_by_value)]

mod terminal;

use crate::terminal::{Terminal, TerminalEvent, TerminalPlugin};
use bevy::prelude::*;
use clap::Parser as _;

#[derive(clap::Parser, Resource)]
struct Args {}

#[derive(Deref, DerefMut, Resource)]
struct Text(String);

fn main() {
    let args = Args::parse();

    let text = Text(String::from("Hello, world!"));

    App::new()
        .add_plugins((MinimalPlugins, TerminalPlugin))
        .insert_resource(args)
        .insert_resource(text)
        .add_systems(
            Update,
            (
                render_system,
                handle_input_system.run_if(on_event::<TerminalEvent>),
            ),
        )
        .run();
}

fn render_system(text: Res<Text>, mut terminal: ResMut<Terminal>) -> Result {
    use ratatui::text::Text;

    terminal.draw(|frame| {
        let text = Text::raw(&**text);
        frame.render_widget(text, frame.area());
    })?;

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
