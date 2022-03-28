use indigo::{editor::Editor, terminal::run};

fn main() {
    let mut editor = Editor::new();

    if let Some(path) = std::env::args().nth(1) {
        editor.load_file(path);
    }

    run(editor);
}
