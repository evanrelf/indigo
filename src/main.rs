use indigo::{editor::Editor, terminal::run};

fn main() {
    #[cfg(debug_assertions)]
    if std::env::var("RUST_BACKTRACE").is_err() {
        std::env::set_var("RUST_BACKTRACE", "1");
    }

    let mut editor = Editor::new();

    for path in std::env::args().skip(1) {
        editor.open(path);
    }

    run(editor);
}
