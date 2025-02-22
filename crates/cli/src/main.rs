use clap::Parser as _;
use indigo_core::prelude::*;
use std::io;

#[derive(Debug, clap::Parser)]
struct Args {
    keys: Option<Keys>,
}

fn main() -> anyhow::Result<()> {
    #[cfg(debug_assertions)]
    if std::env::var("RUST_BACKTRACE").is_err() {
        // SAFETY: At this point the program is single-threaded. There are no other threads that
        // could be reading from or writing to the environment.
        unsafe {
            std::env::set_var("RUST_BACKTRACE", "1");
        }
    }

    let args = Args::parse();

    if let Some(keys) = args.keys {
        eprintln!("{keys}");
    }

    let rope = Rope::from_reader(io::BufReader::new(io::stdin()))?;

    let editor = Editor::from_rope(rope);

    editor.rope().write_to(io::stdout())?;

    Ok(())
}
