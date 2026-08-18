use indigo_kernel::edit2::{Bias, Edit};
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
        let mut offsets = [byte_offset];
        self.transform_byte_offsets_sorted(&mut offsets);
        offsets[0]
    }

    pub fn transform_byte_offsets_unsorted(&self, byte_offsets: &mut [usize]) {
        // Pair each offset with its original index so we can scatter results back
        // after transforming in sorted order.
        let mut indexed: Vec<(usize, usize)> = byte_offsets.iter().copied().enumerate().collect();
        indexed.sort_by_key(|&(_, offset)| offset);

        let mut sorted_offsets: Vec<usize> = indexed.iter().map(|&(_, offset)| offset).collect();
        self.transform_byte_offsets_sorted(&mut sorted_offsets);

        for (sorted_idx, &(orig_idx, _)) in indexed.iter().enumerate() {
            byte_offsets[orig_idx] = sorted_offsets[sorted_idx];
        }
    }

    pub fn transform_byte_offsets_sorted(&self, byte_offsets: &mut [usize]) {
        self.edit
            .transform_byte_indexes(byte_offsets, Bias::Forward);
    }
}
