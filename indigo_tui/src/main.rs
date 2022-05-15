use clap::Parser as _;
use indigo_core::Editor;
use std::{fs::File, path::PathBuf};

#[derive(clap::Parser)]
struct Args {
    #[clap(long)]
    trace: Option<PathBuf>,
    files: Vec<PathBuf>,
}

fn main() {
    #[cfg(debug_assertions)]
    if std::env::var("RUST_BACKTRACE").is_err() {
        std::env::set_var("RUST_BACKTRACE", "1");
    }

    let args = Args::parse();

    if let Some(trace_path) = args.trace {
        let trace_file = if trace_path.exists() {
            File::open(trace_path).unwrap()
        } else {
            File::create(trace_path).unwrap()
        };
        tracing_subscriber::fmt()
            .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
            .with_writer(trace_file)
            .init();
    }

    let mut editor = Editor::default();

    for path in args.files {
        editor.open_buffer(path).unwrap();
    }

    indigo_tui::run(editor);
}
