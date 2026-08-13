use ropey::Rope;
use std::{iter::zip, ops::Range};

#[derive(Clone, Copy, PartialEq)]
enum OpKind {
    Retain,
    Delete,
    Insert,
}

impl OpKind {
    fn invert(self) -> Self {
        match self {
            Self::Retain => Self::Retain,
            Self::Delete => Self::Insert,
            Self::Insert => Self::Delete,
        }
    }
}

#[derive(Clone, Default)]
pub struct Edits {
    op_kinds: Vec<OpKind>,
    op_lengths: Vec<u32>,
    op_texts: String,

    length_before: usize,
    length_after: usize,
}

impl Edits {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    fn push_op(&mut self, kind: OpKind, byte_length: usize) {
        if self.op_kinds.last() == Some(&kind) {
            let last = self.op_lengths.last_mut().unwrap();
            *last = to_u32(to_usize(*last) + byte_length);
        } else {
            self.op_kinds.push(kind);
            self.op_lengths.push(to_u32(byte_length));
        }
    }

    pub fn retain(&mut self, byte_length: usize) {
        if byte_length == 0 {
            return;
        }

        self.length_before += byte_length;
        self.length_after += byte_length;

        self.push_op(OpKind::Retain, byte_length);
    }

    pub fn retain_rest(&mut self, rope: &Rope) -> anyhow::Result<()> {
        anyhow::ensure!(
            self.length_before <= rope.len(),
            "edit input length {} <= rope length {}",
            self.length_before,
            rope.len()
        );

        self.retain(rope.len() - self.length_before);

        Ok(())
    }

    pub fn delete(&mut self, text: &str) {
        if text.is_empty() {
            return;
        }

        self.length_before += text.len();

        self.push_op(OpKind::Delete, text.len());
        self.op_texts.push_str(text);
    }

    pub fn insert(&mut self, text: &str) {
        if text.is_empty() {
            return;
        }

        self.length_after += text.len();

        self.push_op(OpKind::Insert, text.len());
        self.op_texts.push_str(text);
    }

    #[must_use]
    pub fn compose(&self, _other: &Self) -> Self {
        todo!()
    }

    #[must_use]
    pub fn invert(&self) -> Self {
        Self {
            op_kinds: self.op_kinds.iter().map(|kind| kind.invert()).collect(),
            op_lengths: self.op_lengths.clone(),
            op_texts: self.op_texts.clone(),
            length_before: self.length_after,
            length_after: self.length_before,
        }
    }

    #[must_use]
    pub fn rebase(&self, _over: &Self, _bias: Bias) -> Self {
        // Requires OT TP1 but not TP2
        // <https://en.wikipedia.org/wiki/Operational_transformation#Transformation_properties>
        todo!()
    }

    pub fn apply(&self, rope: &mut Rope) -> anyhow::Result<()> {
        self.apply_impl(rope, false)
    }

    /// Equivalent to `self.invert().apply(rope)`, but without allocating.
    pub fn apply_inverse(&self, rope: &mut Rope) -> anyhow::Result<()> {
        self.apply_impl(rope, true)
    }

    fn apply_impl(&self, rope: &mut Rope, invert: bool) -> anyhow::Result<()> {
        let (length_before, length_after) = if invert {
            (self.length_after, self.length_before)
        } else {
            (self.length_before, self.length_after)
        };

        anyhow::ensure!(
            length_before == rope.len(),
            "edit input length {} != rope length {}",
            length_before,
            rope.len()
        );

        let mut rope_index = 0;
        let mut op_texts_index = 0;

        for (kind, length) in zip(&self.op_kinds, &self.op_lengths) {
            let kind = if invert { kind.invert() } else { *kind };
            let length = to_usize(*length);
            match kind {
                OpKind::Retain => {
                    rope_index += length;
                }
                OpKind::Delete => {
                    let range = rope_index..rope_index + length;
                    #[cfg(debug_assertions)]
                    verify_deleted_text(
                        rope,
                        range.clone(),
                        &self.op_texts[op_texts_index..op_texts_index + length],
                    )?;
                    op_texts_index += length;
                    rope.try_remove(range)?;
                }
                OpKind::Insert => {
                    let text = &self.op_texts[op_texts_index..op_texts_index + length];
                    op_texts_index += length;
                    rope.try_insert(rope_index, text)?;
                    rope_index += length;
                }
            }
        }

        anyhow::ensure!(
            length_after == rope.len() && rope_index == rope.len(),
            "edit output length {} != rope length {}",
            length_after,
            rope.len()
        );

        Ok(())
    }
}

#[derive(Clone, Copy)]
pub enum Bias {
    Backward,
    Forward,
}

#[cfg(debug_assertions)]
fn verify_deleted_text(rope: &Rope, range: Range<usize>, text: &str) -> anyhow::Result<()> {
    let mut remaining = text;
    for chunk in rope.slice(range).chunks() {
        let (head, tail) = remaining.split_at(chunk.len());
        anyhow::ensure!(head == chunk, "deleted text `{text}` not in rope");
        remaining = tail;
    }
    Ok(())
}

fn to_u32(n: usize) -> u32 {
    u32::try_from(n).expect("operation length fits in u32")
}

fn to_usize(n: u32) -> usize {
    usize::try_from(n).expect("u32 fits in usize")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() -> anyhow::Result<()> {
        let mut rope = Rope::from("Hello, world!");
        let mut edits = Edits::new();
        edits.retain(7);
        edits.delete("world");
        edits.insert("Evan");
        edits.delete("!");
        edits.insert("...");
        edits.insert("?");

        edits.apply(&mut rope)?;
        assert_eq!(rope, Rope::from("Hello, Evan...?"));

        edits.invert().apply(&mut rope)?;
        assert_eq!(rope, Rope::from("Hello, world!"));

        edits.apply(&mut rope)?;
        edits.apply_inverse(&mut rope)?;
        assert_eq!(rope, Rope::from("Hello, world!"));

        let mut rope = Rope::from("Hello, world!");
        edits.retain(1);
        assert!(edits.apply(&mut rope).is_err());

        Ok(())
    }
}
