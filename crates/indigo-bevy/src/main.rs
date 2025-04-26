#![allow(clippy::needless_pass_by_value)]

mod terminal;

use crate::terminal::{Terminal, TerminalKeyEvent, TuiPlugin};
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
        .add_plugins((MinimalPlugins, TuiPlugin))
        .insert_resource(args)
        .insert_resource(text)
        .add_systems(
            Update,
            (
                render_system,
                handle_key_input_system.run_if(on_event::<TerminalKeyEvent>),
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

fn handle_key_input_system(
    mut text: ResMut<Text>,
    mut key_events: EventReader<TerminalKeyEvent>,
    mut exit_events: EventWriter<AppExit>,
) {
    use crossterm::event::{KeyCode, KeyModifiers};

    for TerminalKeyEvent(key_event) in key_events.read() {
        match (key_event.modifiers, key_event.code) {
            (m, KeyCode::Char('c')) if m == KeyModifiers::CONTROL => {
                exit_events.write_default();
            }
            (_, KeyCode::Char(c)) => {
                text.push(c);
            }
            (m, KeyCode::Backspace) if m == KeyModifiers::NONE => {
                text.pop();
            }
            _ => {}
        }
    }
}
