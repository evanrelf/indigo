use indigo_core::Editor;

fn main() {
    #[cfg(debug_assertions)]
    if std::env::var("RUST_BACKTRACE").is_err() {
        std::env::set_var("RUST_BACKTRACE", "1");
    }

    let mut editor = Editor::default();

    for path in std::env::args().skip(1) {
        editor.open_buffer(path).unwrap();
    }

    indigo_tui::run(editor);
}
