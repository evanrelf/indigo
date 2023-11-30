#[derive(Debug)]
pub struct Settings {
    pub numbers: bool,
    pub columns: Vec<usize>,
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            numbers: true,
            columns: vec![81],
        }
    }
}
