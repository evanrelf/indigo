#[derive(Debug)]
pub struct Settings {
    pub line_numbers: LineNumbers,
    pub columns: Vec<usize>,
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            line_numbers: LineNumbers::Absolute,
            columns: vec![81],
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum LineNumbers {
    Hidden,
    Absolute,
}
