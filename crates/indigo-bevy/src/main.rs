#![allow(clippy::needless_pass_by_value)]

mod park;
mod tui;

use crate::{
    park::{ParkPlugin, ParkSettings},
    tui::{Key, Resize, Terminal, TuiPlugin},
};
use bevy::prelude::*;
use clap::Parser as _;
use std::time::Duration;

#[derive(clap::Parser, Resource)]
struct Args {}

#[derive(Deref, DerefMut, Resource)]
struct Text(String);

#[derive(Default, Event)]
struct RequestRender;

fn main() {
    let args = Args::parse();

    let text = Text(String::from("Hello, world!"));

    App::new()
        .add_plugins((MinimalPlugins, ParkPlugin, TuiPlugin))
        .insert_resource(ParkSettings {
            // TODO: Remove once `ParkPlugin` unparking is fixed.
            duration: Some(Duration::from_secs(1)),
        })
        .insert_resource(args)
        .insert_resource(text)
        .add_event::<RequestRender>()
        .add_systems(Startup, render_system)
        .add_systems(
            Update,
            (
                render_system.run_if(on_event::<RequestRender>.or(on_event::<Resize>)),
                handle_key_input_system.run_if(on_event::<Key>),
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
    mut key: EventReader<Key>,
    mut render: EventWriter<RequestRender>,
    mut exit: EventWriter<AppExit>,
) {
    use crossterm::event::{KeyCode, KeyModifiers};

    let mut should_render = false;

    for Key(key) in key.read() {
        match (key.modifiers, key.code) {
            (m, KeyCode::Char('c')) if m == KeyModifiers::CONTROL => {
                exit.write_default();
            }
            (_, KeyCode::Char(c)) => {
                text.push(c);
            }
            (m, KeyCode::Backspace) if m == KeyModifiers::NONE => {
                text.pop();
            }
            _ => continue,
        }

        should_render = true;
    }

    if should_render {
        render.write_default();
    }
}
