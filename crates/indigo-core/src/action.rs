use crate::editor::Editor;
use std::process::ExitCode;

pub fn exit(editor: &mut Editor, exit_code: u8) {
    editor.exit = Some(ExitCode::from(exit_code));
}
