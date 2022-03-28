use indigo::{editor::Editor, terminal::run};

fn main() {
    #[cfg(debug_assertions)]
    std::env::set_var("RUST_BACKTRACE", "1");

    let mut editor = Editor::new();

    if let Some(path) = std::env::args().nth(1) {
        editor.load_file(path);
    }

    run(editor);
}
