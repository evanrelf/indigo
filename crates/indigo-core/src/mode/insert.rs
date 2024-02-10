#[derive(Clone, Copy, Debug, Default)]
pub struct InsertMode {}

impl InsertMode {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
}
