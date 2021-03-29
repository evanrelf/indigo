use ropey::Rope;

pub struct Selection {
    anchor_index: usize,
    cursor_index: usize,
}

impl Selection {
    pub fn new() -> Selection {
        Selection {
            anchor_index: 0,
            cursor_index: 0,
        }
    }
}

pub struct Buffer {
    contents: Rope,
    selections: Vec<Selection>,
}

impl Buffer {
    pub fn new() -> Buffer {
        Buffer {
            contents: Rope::new(),
            selections: Vec::new(),
        }
    }
}
