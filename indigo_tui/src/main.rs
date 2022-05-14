use clap::Parser as _;
use indigo_core::Editor;
use std::path::PathBuf;

#[derive(clap::Parser)]
struct Args {
    files: Vec<PathBuf>,
}

fn main() {
    #[cfg(debug_assertions)]
    if std::env::var("RUST_BACKTRACE").is_err() {
        std::env::set_var("RUST_BACKTRACE", "1");
    }

    let args = Args::parse();

    let mut editor = Editor::default();

    for path in args.files {
        editor.open_buffer(path).unwrap();
    }

    indigo_tui::run(editor);
}
