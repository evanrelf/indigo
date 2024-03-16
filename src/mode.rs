#[derive(Debug)]
pub enum Mode {
    Normal,
    Insert { after: bool },
}
