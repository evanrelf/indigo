use ropey::Rope;

#[derive(Debug, Default)]
pub struct Editor {
    pub text: Rope,
    pub cursor: (usize, usize),
    pub scroll: usize,
}
