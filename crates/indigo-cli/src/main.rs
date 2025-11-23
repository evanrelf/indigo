use clap::Parser as _;
use indigo_core::{event::handle_event, key::is, prelude::*};
use std::{io, process::ExitCode, sync::Arc};
use tracing_subscriber::EnvFilter;

#[derive(Debug, clap::Parser)]
struct Args {
    keys: Keys,

    /// Enable debugging functionality
    ///
    /// - Enter `<c-l>` to print handled keys and a diff since the last `<c-l>` key was encountered.
    #[arg(long)]
    debug: bool,

    #[arg(long, env = "INDIGO_LOG", default_value_t)]
    log_filter: Arc<str>,
}

fn main() -> anyhow::Result<ExitCode> {
    #[cfg(debug_assertions)]
    if std::env::var("RUST_BACKTRACE").is_err() {
        // SAFETY: At this point the program is single-threaded. There are no other threads that
        // could be reading from or writing to the environment.
        //
        // NOTE: Replace with `std::panic::set_backtrace_style` once it stabilizes.
        // https://github.com/rust-lang/rust/issues/93346
        #[expect(unsafe_code)]
        unsafe {
            std::env::set_var("RUST_BACKTRACE", "1");
        }
    }

    let args = Args::parse();

    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::new(args.log_filter))
        .with_writer(io::stderr)
        .init();

    let rope = Rope::from_reader(io::BufReader::new(io::stdin()))?;

    let mut editor = Editor::from(Buffer::from(rope));

    let mut debug_keys = vec![];
    let mut debug_rope = editor.window().buffer().rope().clone();

    for key in args.keys.0 {
        let event = Event::Key(KeyEvent {
            key,
            kind: KeyEventKind::Press,
        });

        handle_event(&mut editor, event)?;

        if args.debug {
            debug_keys.push(key);
        }

        if args.debug && is(&key, "<c-l>") {
            let left = debug_rope.to_string();
            let right = editor.window().buffer().rope().to_string();
            eprint!("keys: ");
            for key in &debug_keys {
                if !is(key, "<c-l>") {
                    eprint!("{key}");
                }
            }
            eprintln!("\ntext:\n```diff");
            for diff in diff::lines(&left, &right) {
                match diff {
                    diff::Result::Left(l) => eprintln!("-{l}"),
                    diff::Result::Both(l, _) => eprintln!(" {l}"),
                    diff::Result::Right(r) => eprintln!("+{r}"),
                }
            }
            eprintln!("```");
            debug_keys.clear();
            debug_rope = editor.window().buffer().rope().clone();
        }
    }

    let _result = editor
        .window()
        .buffer()
        .rope()
        .write_to(io::LineWriter::new(io::stdout()));

    let exit_code = editor.exit_code().unwrap_or(ExitCode::SUCCESS);

    Ok(exit_code)
}
