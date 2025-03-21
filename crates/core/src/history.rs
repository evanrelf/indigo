use crate::ot::EditSeq;

#[derive(Default)]
pub struct History {
    edits: Vec<EditSeq>,
    index: usize,
}

impl History {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, edit: EditSeq) {
        self.edits.truncate(self.index);
        self.edits.push(edit);
        self.index += 1;
    }

    pub fn undo(&mut self) -> Option<&EditSeq> {
        if self.index == 0 {
            return None;
        }
        let edit = &self.edits[self.index];
        self.index -= 1;
        Some(edit)
    }

    pub fn redo(&mut self) -> Option<&EditSeq> {
        let edit = self.edits.get(self.index)?;
        self.index += 1;
        Some(edit)
    }
}
