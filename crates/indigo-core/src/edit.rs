use indigo_kernel::edit::{Bias, Edit};
use ropey::Rope;

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct OperationSeq {
    edit: Edit,
}

impl OperationSeq {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.edit.is_empty()
    }

    pub fn retain(&mut self, byte_length: usize) {
        self.edit.retain(byte_length);
    }

    pub fn retain_rest(&mut self, rope: &Rope) -> anyhow::Result<()> {
        self.edit.retain_rest(rope)
    }

    pub fn delete(&mut self, text: &str) {
        self.edit.delete(text);
    }

    pub fn insert(&mut self, text: &str) {
        self.edit.insert(text);
    }

    pub fn compose(&self, other: &Self) -> anyhow::Result<Self> {
        Ok(Self {
            edit: self.edit.compose(&other.edit)?,
        })
    }

    #[must_use]
    pub fn invert(&self) -> Self {
        Self {
            edit: self.edit.invert(),
        }
    }

    pub fn apply(&self, rope: &mut Rope) -> anyhow::Result<()> {
        self.edit.apply(rope)
    }

    #[must_use]
    pub fn transform_byte_offset(&self, byte_offset: usize) -> usize {
        self.edit.map_position(byte_offset, Bias::Forward)
    }

    pub fn transform_byte_offsets_unsorted(&self, byte_offsets: &mut [usize]) {
        self.edit.map_positions(byte_offsets, Bias::Forward);
    }

    pub fn transform_byte_offsets_sorted(&self, byte_offsets: &mut [usize]) {
        self.edit.map_positions_sorted(byte_offsets, Bias::Forward);
    }
}
