use clap::Parser as _;
use indigo_core::prelude::*;
use std::{io, process::ExitCode};
use tracing_subscriber::EnvFilter;

#[derive(Debug, clap::Parser)]
struct Args {
    #[clap(default_value_t)]
    keys: Keys,

    #[clap(long, env = "INDIGO_LOG", default_value_t)]
    log_filter: String,
}

fn main() -> anyhow::Result<ExitCode> {
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

    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::new(args.log_filter))
        .with_writer(io::stderr)
        .init();

    if !args.keys.0.is_empty() {
        tracing::debug!(keys = %args.keys);
    }

    let rope = Rope::from_reader(io::BufReader::new(io::stdin()))?;

    let mut editor = Editor::from(Buffer::from(rope));

    for key in args.keys {
        editor.trigger(key);
    }

    editor
        .buffer
        .text()
        .write_to(io::LineWriter::new(io::stdout()))?;

    if let Some(exit_code) = editor.exit {
        Ok(ExitCode::from(exit_code))
    } else {
        Ok(ExitCode::SUCCESS)
    }
}
