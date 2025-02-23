use clap::Parser as _;
use indigo_core::{
    event::{handle_event, Event},
    prelude::*,
};
use std::io;

#[derive(Debug, clap::Parser)]
struct Args {
    // TODO: Is there a more idiomatic way of writing this defaulting?
    #[clap(default_value_t = Keys::default())]
    keys: Keys,
}

fn main() -> anyhow::Result<()> {
    #[cfg(debug_assertions)]
    if std::env::var("RUST_BACKTRACE").is_err() {
        // SAFETY: At this point the program is single-threaded. There are no other threads that
        // could be reading from or writing to the environment.
        // TODO: Replace with `std::panic::set_backtrace_style` once it stabilizes.
        // https://github.com/rust-lang/rust/issues/93346
        unsafe {
            std::env::set_var("RUST_BACKTRACE", "1");
        }
    }

    let args = Args::parse();

    if !args.keys.0.is_empty() {
        eprintln!("{}", args.keys);
    }

    let rope = Rope::from_reader(io::BufReader::new(io::stdin()))?;

    let mut editor = Editor::from_rope(rope);

    for key in args.keys.0 {
        handle_event(&mut editor, &Event::Key(key));
    }

    editor.rope().write_to(io::LineWriter::new(io::stdout()))?;

    Ok(())
}
