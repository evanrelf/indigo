mod terminal;

use clap::Parser as _;
use crossterm::{event, style, ExecutableCommand as _};
use std::path::PathBuf;
use tokio_stream::StreamExt as _;

#[derive(clap::Parser)]
struct Args;

#[tokio::main]
async fn main() {
    let _args = Args::parse();

    let _terminal = terminal::enter();

    let mut stdout = std::io::stdout();

    stdout.execute(style::Print("Hello, world!")).unwrap();

    let mut event_stream = event::EventStream::new();

    loop {
        use crossterm::event::{Event::*, KeyCode::*, KeyModifiers as M};

        let event = match event_stream.next().await {
            Some(Ok(event)) => event,
            Some(Err(error)) => panic!("Error: {error}"),
            None => break,
        };

        match event {
            Key(key) if key.modifiers == M::CONTROL && key.code == Char('p') => panic!(),
            Key(key) if key.modifiers == M::CONTROL && key.code == Char('c') => break,
            Key(key) if key.modifiers == M::NONE && key.code == Char('q') => break,
            _ => continue,
        }
    }
}
