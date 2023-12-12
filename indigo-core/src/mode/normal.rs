#[derive(Clone, Copy, Debug, Default)]
pub struct NormalMode {
    count: usize,
}

impl NormalMode {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
}
