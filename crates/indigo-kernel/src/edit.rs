use ropey::Rope;
use std::{cmp::min, iter::zip, ops::Range};

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

    pub fn compose(&self, other: &Self) -> anyhow::Result<Self> {
        anyhow::ensure!(
            self.length_after == other.length_before,
            "left edit output length {} != right edit input length {}",
            self.length_after,
            other.length_before,
        );

        let mut a = OpCursor {
            edits: self,
            op_index: 0,
            consumed: 0,
            text_index: 0,
        };
        let mut b = OpCursor {
            edits: other,
            op_index: 0,
            consumed: 0,
            text_index: 0,
        };
        let mut result = Self::new();

        loop {
            match (a.kind(), b.kind()) {
                // `self` deleted text `other` never saw.
                (Some(OpKind::Delete), _) => {
                    let n = a.remaining();
                    result.delete(a.text(n));
                    a.advance(n);
                }
                // `other` inserted text `self` never saw.
                (_, Some(OpKind::Insert)) => {
                    let n = b.remaining();
                    result.insert(b.text(n));
                    b.advance(n);
                }
                (None, None) => break,
                (Some(a_kind), Some(b_kind)) => {
                    let n = min(a.remaining(), b.remaining());
                    match (a_kind, b_kind) {
                        (OpKind::Retain, OpKind::Retain) => result.retain(n),
                        (OpKind::Retain, OpKind::Delete) => result.delete(b.text(n)),
                        (OpKind::Insert, OpKind::Retain) => result.insert(a.text(n)),
                        // `other` deleted text `self` inserted; the ops cancel out
                        (OpKind::Insert, OpKind::Delete) => debug_assert_eq!(a.text(n), b.text(n)),
                        (OpKind::Delete, _) | (_, OpKind::Insert) => unreachable!(),
                    }
                    a.advance(n);
                    b.advance(n);
                }
                (None, Some(_)) | (Some(_), None) => unreachable!(),
            }
        }

        Ok(result)
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

struct OpCursor<'a> {
    edits: &'a Edits,
    op_index: usize,
    consumed: usize,
    text_index: usize,
}

impl<'a> OpCursor<'a> {
    fn kind(&self) -> Option<OpKind> {
        self.edits.op_kinds.get(self.op_index).copied()
    }

    fn remaining(&self) -> usize {
        to_usize(self.edits.op_lengths[self.op_index]) - self.consumed
    }

    fn text(&self, n: usize) -> &'a str {
        let start = self.text_index + self.consumed;
        &self.edits.op_texts[start..start + n]
    }

    fn advance(&mut self, n: usize) {
        self.consumed += n;
        if self.consumed == to_usize(self.edits.op_lengths[self.op_index]) {
            if self.kind() != Some(OpKind::Retain) {
                self.text_index += self.consumed;
            }
            self.op_index += 1;
            self.consumed = 0;
        }
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

    #[test]
    fn test_compose() -> anyhow::Result<()> {
        let rope = Rope::from("Hello, world!");

        let mut a = Edits::new();
        a.retain(7);
        a.delete("world");
        a.insert("Evan");
        a.retain_rest(&rope)?;

        let mut b = Edits::new();
        b.retain(7);
        b.insert("dear ");
        b.retain(4);
        b.delete("!");
        b.insert("?");

        // Composing is equivalent to applying sequentially.
        let mut sequential = rope.clone();
        a.apply(&mut sequential)?;
        b.apply(&mut sequential)?;
        assert_eq!(sequential, Rope::from("Hello, dear Evan?"));

        let ab = a.compose(&b)?;
        let mut composed = rope.clone();
        ab.apply(&mut composed)?;
        assert_eq!(composed, sequential);

        // A composed edit inverts cleanly.
        ab.invert().apply(&mut composed)?;
        assert_eq!(composed, rope);

        // An insert deleted by the next edit cancels out entirely.
        let mut a = Edits::new();
        a.retain(5);
        a.insert("abc");
        a.retain_rest(&rope)?;

        let mut intermediate = rope.clone();
        a.apply(&mut intermediate)?;

        let mut b = Edits::new();
        b.retain(6);
        b.delete("b");
        b.retain_rest(&intermediate)?;

        let ab = a.compose(&b)?;
        let mut composed = rope.clone();
        ab.apply(&mut composed)?;
        assert_eq!(composed, Rope::from("Helloac, world!"));

        Ok(())
    }
}
