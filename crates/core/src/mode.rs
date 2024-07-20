use std::fmt::Display;

#[derive(Default)]
pub enum Mode {
    #[default]
    Normal,
    Insert,
}

impl Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::Normal => write!(f, "normal"),
            Self::Insert => write!(f, "insert"),
        }
    }
}
