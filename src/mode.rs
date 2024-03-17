#[derive(Debug)]
pub enum Mode {
    Normal,
    Goto,
    Insert { after: bool },
}
