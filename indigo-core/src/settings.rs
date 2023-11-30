#[derive(Debug)]
pub struct Settings {
    pub numbers: Numbers,
    pub columns: Vec<usize>,
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            numbers: Numbers::Absolute,
            columns: vec![81],
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Numbers {
    Hidden,
    Absolute,
}
