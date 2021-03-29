use ropey::Rope;

pub struct Selection {
    anchor_index: usize,
    cursor_index: usize,
}

pub struct Buffer {
    contents: Rope,
    selections: Vec<Selection>,
}
