use crate::{selection::SelectionState, Selection, SelectionMut};
use ropey::Rope;

#[derive(Clone, Debug)]
pub struct Buffer {
    contents: Rope,
    selection: SelectionState,
}

impl Buffer {
    #[must_use]
    pub fn contents(&self) -> &Rope {
        &self.contents
    }

    #[must_use]
    pub fn selection(&self) -> Selection {
        let rope = &self.contents;
        let state = &self.selection;
        Selection::new(rope, state)
    }

    #[must_use]
    pub fn selection_mut(&mut self) -> SelectionMut {
        let rope = &mut self.contents;
        let state = &mut self.selection;
        SelectionMut::new(rope, state)
    }

    pub fn assert_valid(&self) {
        assert!(
            self.contents.len_chars() > 0,
            "`contents` rope is not empty"
        );

        self.selection().assert_valid();
        // `Selection` and `SelectionMut` share the same invariants, but the former doesn't require
        // exclusive references, so we don't bother checking the latter.
        // self.selection_mut().assert_valid();

        // TODO: Buffer validation
    }
}

impl Default for Buffer {
    fn default() -> Self {
        Self {
            contents: Rope::from("\n"),
            selection: SelectionState::default(),
        }
    }
}

impl TryFrom<Rope> for Buffer {
    type Error = anyhow::Error;

    fn try_from(rope: Rope) -> Result<Self, Self::Error> {
        if rope.len_chars() == 0 {
            anyhow::bail!("Buffer cannot be empty");
        }
        Ok(Self {
            contents: rope,
            ..Self::default()
        })
    }
}

impl TryFrom<String> for Buffer {
    type Error = anyhow::Error;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        Self::try_from(Rope::from(s))
    }
}

impl TryFrom<&str> for Buffer {
    type Error = anyhow::Error;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        Self::try_from(Rope::from(s))
    }
}
