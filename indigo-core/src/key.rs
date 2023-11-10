use std::collections::HashSet;

pub struct Key {
    pub modifiers: HashSet<Modifier>,
    pub code: Code,
}

pub enum Modifier {
    Shift,
    Ctrl,
    Alt,
    Super,
}

pub enum Code {
    Backspace,
    Enter,
    Left,
    Right,
    Up,
    Down,
    Tab,
    Char(char),
    Escape,
    Modifier(Modifier),
}
