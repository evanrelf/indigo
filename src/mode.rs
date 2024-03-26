#[derive(Debug)]
pub enum Mode {
    Normal,
    Goto,
    // TODO: How to insert text at EOF? Cursor inserts to the left, and must live on a valid rope
    // position.
    Insert { after: bool },
}
