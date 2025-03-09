use crate::editor::Editor;

pub enum Action {
    EnterNormalMode,
    EnterInsertMode,
    EnterCommandMode,
    MoveLeft,
    MoveRight,
    MoveTo(usize),
    ExtendLeft,
    ExtendRight,
    ExtendTo(usize),
    Flip,
    FlipForward,
    Reduce,
    ScrollUp,
    ScrollDown,
    ScrollHalfPageUp,
    ScrollHalfPageDown,
    ScrollFullPageUp,
    ScrollFullPageDown,
    InsertChar(char),
    Insert(String),
    DeleteBefore,
    Delete,
    DeleteAfter,
}

pub fn handle_action(_editor: &mut Editor, action: &Action) {
    #[expect(clippy::match_same_arms)]
    match action {
        Action::EnterNormalMode => {}
        Action::EnterInsertMode => {}
        Action::EnterCommandMode => {}
        Action::MoveLeft => {}
        Action::MoveRight => {}
        Action::MoveTo(_index) => {}
        Action::ExtendLeft => {}
        Action::ExtendRight => {}
        Action::ExtendTo(_index) => {}
        Action::Flip => {}
        Action::FlipForward => {}
        Action::Reduce => {}
        Action::ScrollUp => {}
        Action::ScrollDown => {}
        Action::ScrollHalfPageUp => {}
        Action::ScrollHalfPageDown => {}
        Action::ScrollFullPageUp => {}
        Action::ScrollFullPageDown => {}
        Action::InsertChar(_char) => {}
        Action::Insert(_string) => {}
        Action::DeleteBefore => {}
        Action::Delete => {}
        Action::DeleteAfter => {}
    }
}
