use ropey::Rope;
use std::{cmp::min, iter::zip, ops::Range};

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Edit {
    op_kinds: Vec<OpKind>,
    op_lengths: Vec<u32>,
    op_texts: String,

    length_before: usize,
    length_after: usize,
}

impl Edit {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.op_kinds.is_empty()
    }

    pub fn retain(&mut self, byte_length: usize) {
        if byte_length == 0 {
            return;
        }

        self.length_before += byte_length;
        self.length_after += byte_length;

        if self.op_kinds.last() == Some(&OpKind::Retain) {
            let last = self.op_lengths.last_mut().unwrap();
            *last = to_u32(to_usize(*last) + byte_length);
        } else {
            self.op_kinds.push(OpKind::Retain);
            self.op_lengths.push(to_u32(byte_length));
        }
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
        let byte_length = text.len();

        if byte_length == 0 {
            return;
        }

        self.length_before += byte_length;

        // Note [Canonical form]
        let last = self.op_kinds.last().copied();
        let second_last = self.op_kinds.len().checked_sub(2).map(|i| self.op_kinds[i]);

        match (second_last, last) {
            (_, Some(OpKind::Delete)) => {
                let last = self.op_lengths.last_mut().unwrap();
                *last = to_u32(to_usize(*last) + byte_length);
                self.op_texts.push_str(text);
            }
            (Some(OpKind::Delete), Some(OpKind::Insert)) => {
                let insert_length = to_usize(*self.op_lengths.last().unwrap());
                let delete_index = self.op_lengths.len() - 2;
                self.op_lengths[delete_index] =
                    to_u32(to_usize(self.op_lengths[delete_index]) + byte_length);
                self.op_texts
                    .insert_str(self.op_texts.len() - insert_length, text);
            }
            (_, Some(OpKind::Insert)) => {
                let insert_length = to_usize(*self.op_lengths.last().unwrap());
                let insert_index = self.op_kinds.len() - 1;
                self.op_kinds.insert(insert_index, OpKind::Delete);
                self.op_lengths.insert(insert_index, to_u32(byte_length));
                self.op_texts
                    .insert_str(self.op_texts.len() - insert_length, text);
            }
            (_, Some(OpKind::Retain) | None) => {
                self.op_kinds.push(OpKind::Delete);
                self.op_lengths.push(to_u32(byte_length));
                self.op_texts.push_str(text);
            }
        }
    }

    pub fn insert(&mut self, text: &str) {
        let byte_length = text.len();

        if byte_length == 0 {
            return;
        }

        self.length_after += byte_length;

        if self.op_kinds.last() == Some(&OpKind::Insert) {
            let last = self.op_lengths.last_mut().unwrap();
            *last = to_u32(to_usize(*last) + byte_length);
        } else {
            self.op_kinds.push(OpKind::Insert);
            self.op_lengths.push(to_u32(byte_length));
        }

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
            edit: self,
            op_index: 0,
            consumed: 0,
            text_index: 0,
        };
        let mut b = OpCursor {
            edit: other,
            op_index: 0,
            consumed: 0,
            text_index: 0,
        };
        let mut edit = Self::new();

        loop {
            match (a.kind(), b.kind()) {
                // `self` deleted text `other` never saw.
                (Some(OpKind::Delete), _) => {
                    let n = a.remaining();
                    edit.delete(a.text(n));
                    a.advance(n);
                }
                // `other` inserted text `self` never saw.
                (_, Some(OpKind::Insert)) => {
                    let n = b.remaining();
                    edit.insert(b.text(n));
                    b.advance(n);
                }
                (None, None) => break,
                (Some(a_kind), Some(b_kind)) => {
                    let n = min(a.remaining(), b.remaining());
                    match (a_kind, b_kind) {
                        (OpKind::Retain, OpKind::Retain) => edit.retain(n),
                        (OpKind::Retain, OpKind::Delete) => edit.delete(b.text(n)),
                        (OpKind::Insert, OpKind::Retain) => edit.insert(a.text(n)),
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

        #[cfg(debug_assertions)]
        edit.assert_is_canonical();

        Ok(edit)
    }

    #[must_use]
    pub fn invert(&self) -> Self {
        // Note [Canonical form]
        let mut edit = Self::new();
        let mut text_index = 0;

        for (kind, length) in zip(&self.op_kinds, &self.op_lengths) {
            let length = to_usize(*length);
            match kind {
                OpKind::Retain => edit.retain(length),
                OpKind::Delete => {
                    edit.insert(&self.op_texts[text_index..text_index + length]);
                    text_index += length;
                }
                OpKind::Insert => {
                    edit.delete(&self.op_texts[text_index..text_index + length]);
                    text_index += length;
                }
            }
        }

        #[cfg(debug_assertions)]
        edit.assert_is_canonical();

        edit
    }

    pub fn rebase(&self, onto: &Self, bias: Bias) -> anyhow::Result<Self> {
        anyhow::ensure!(
            self.length_before == onto.length_before,
            "self edit input length {} != onto edit input length {}",
            self.length_before,
            onto.length_before,
        );

        let mut a = OpCursor {
            edit: self,
            op_index: 0,
            consumed: 0,
            text_index: 0,
        };
        let mut b = OpCursor {
            edit: onto,
            op_index: 0,
            consumed: 0,
            text_index: 0,
        };
        let mut edit = Self::new();

        loop {
            match (a.kind(), b.kind()) {
                // Both inserted at the same position; `bias` decides whose text comes first.
                (Some(OpKind::Insert), Some(OpKind::Insert)) => match bias {
                    Bias::Backward => {
                        let n = a.remaining();
                        edit.insert(a.text(n));
                        a.advance(n);
                    }
                    Bias::Forward => {
                        let n = b.remaining();
                        edit.retain(n);
                        b.advance(n);
                    }
                },
                // `self` inserted text `onto` never saw.
                (Some(OpKind::Insert), _) => {
                    let n = a.remaining();
                    edit.insert(a.text(n));
                    a.advance(n);
                }
                // `onto` inserted text `self` never saw; skip over it.
                (_, Some(OpKind::Insert)) => {
                    let n = b.remaining();
                    edit.retain(n);
                    b.advance(n);
                }
                (None, None) => break,
                (Some(a_kind), Some(b_kind)) => {
                    let n = min(a.remaining(), b.remaining());
                    match (a_kind, b_kind) {
                        (OpKind::Retain, OpKind::Retain) => edit.retain(n),
                        // `onto` deleted this text; there's nothing left to retain
                        (OpKind::Retain, OpKind::Delete) => {}
                        (OpKind::Delete, OpKind::Retain) => edit.delete(a.text(n)),
                        // `onto` already deleted this text
                        (OpKind::Delete, OpKind::Delete) => debug_assert_eq!(a.text(n), b.text(n)),
                        (OpKind::Insert, _) | (_, OpKind::Insert) => unreachable!(),
                    }
                    a.advance(n);
                    b.advance(n);
                }
                (None, Some(_)) | (Some(_), None) => unreachable!(),
            }
        }

        #[cfg(debug_assertions)]
        edit.assert_is_canonical();

        Ok(edit)
    }

    /// Input must be sorted.
    pub fn transform_byte_indexes(&self, byte_indexes: &mut [usize], bias: Bias) {
        fn apply_delta(base: usize, delta: isize) -> usize {
            if delta >= 0 {
                base + delta.unsigned_abs()
            } else {
                base - delta.unsigned_abs()
            }
        }

        debug_assert!(byte_indexes.is_sorted());

        let mut position: usize = 0;
        let mut delta: isize = 0;
        let mut i = 0;

        for (kind, length) in zip(&self.op_kinds, &self.op_lengths) {
            if i >= byte_indexes.len() {
                break;
            }
            let length = to_usize(*length);
            match kind {
                OpKind::Retain => {
                    while i < byte_indexes.len() && byte_indexes[i] < position + length {
                        byte_indexes[i] = apply_delta(byte_indexes[i], delta);
                        i += 1;
                    }
                    position += length;
                }
                OpKind::Insert => {
                    if let Bias::Backward = bias {
                        while i < byte_indexes.len() && byte_indexes[i] == position {
                            byte_indexes[i] = apply_delta(position, delta);
                            i += 1;
                        }
                    }
                    delta += length.cast_signed();
                }
                OpKind::Delete => {
                    while i < byte_indexes.len() && byte_indexes[i] < position + length {
                        byte_indexes[i] = apply_delta(position, delta);
                        i += 1;
                    }
                    position += length;
                    delta -= length.cast_signed();
                }
            }
        }

        while i < byte_indexes.len() {
            byte_indexes[i] = apply_delta(byte_indexes[i], delta);
            i += 1;
        }
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

    // Note [Canonical form]
    #[cfg(debug_assertions)]
    fn assert_is_canonical(&self) {
        assert_eq!(self.op_kinds.len(), self.op_lengths.len());

        let mut length_before = 0;
        let mut length_after = 0;
        let mut text_length = 0;

        for (i, (kind, length)) in zip(&self.op_kinds, &self.op_lengths).enumerate() {
            let length = to_usize(*length);
            assert_ne!(length, 0, "zero-length op");
            if let Some(prev) = i.checked_sub(1).map(|i| self.op_kinds[i]) {
                assert_ne!(prev, *kind, "adjacent ops of the same kind");
                assert!(
                    !(prev == OpKind::Insert && *kind == OpKind::Delete),
                    "insert before delete at the same position"
                );
            }
            match kind {
                OpKind::Retain => {
                    length_before += length;
                    length_after += length;
                }
                OpKind::Delete => {
                    length_before += length;
                    text_length += length;
                }
                OpKind::Insert => {
                    length_after += length;
                    text_length += length;
                }
            }
        }

        assert_eq!(length_before, self.length_before);
        assert_eq!(length_after, self.length_after);
        assert_eq!(text_length, self.op_texts.len());
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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

#[derive(Clone, Copy)]
pub enum Bias {
    Backward,
    Forward,
}

struct OpCursor<'a> {
    edit: &'a Edit,
    op_index: usize,
    consumed: usize,
    text_index: usize,
}

impl<'a> OpCursor<'a> {
    fn kind(&self) -> Option<OpKind> {
        self.edit.op_kinds.get(self.op_index).copied()
    }

    fn remaining(&self) -> usize {
        to_usize(self.edit.op_lengths[self.op_index]) - self.consumed
    }

    fn text(&self, n: usize) -> &'a str {
        let start = self.text_index + self.consumed;
        &self.edit.op_texts[start..start + n]
    }

    fn advance(&mut self, n: usize) {
        self.consumed += n;
        if self.consumed == to_usize(self.edit.op_lengths[self.op_index]) {
            if self.kind() != Some(OpKind::Retain) {
                self.text_index += self.consumed;
            }
            self.op_index += 1;
            self.consumed = 0;
        }
    }
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

/*
Note [Canonical form]
---------------------

Claude: An edit is canonical when it has no zero-length ops, no adjacent ops of the same kind, and,
at a given position, deletion before insertion. Canonical form makes the representation unique:
equivalent edits are structurally equal.

Uniqueness is more than tidiness. Deleting "d" and inserting "x" at one position produces the same
document in either order, but when two concurrent edits are reconciled their ops are matched up
positionally, and the two orderings interleave concurrent text on opposite sides of the same
position. If both were representable, equivalent edits could reconcile to different documents
depending on how they happened to be built. Delete-before-insert is an arbitrary choice; what
matters is that exactly one order is representable.
*/

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() -> anyhow::Result<()> {
        let mut rope = Rope::from("Hello, world!");
        let mut edit = Edit::new();
        edit.retain(7);
        edit.delete("world");
        edit.insert("Evan");
        edit.delete("!");
        edit.insert("...");
        edit.insert("?");

        edit.apply(&mut rope)?;
        assert_eq!(rope, Rope::from("Hello, Evan...?"));

        edit.invert().apply(&mut rope)?;
        assert_eq!(rope, Rope::from("Hello, world!"));

        edit.apply(&mut rope)?;
        edit.apply_inverse(&mut rope)?;
        assert_eq!(rope, Rope::from("Hello, world!"));

        let mut rope = Rope::from("Hello, world!");
        edit.retain(1);
        assert!(edit.apply(&mut rope).is_err());

        Ok(())
    }

    #[test]
    fn test_compose() -> anyhow::Result<()> {
        let rope = Rope::from("Hello, world!");

        let mut a = Edit::new();
        a.retain(7);
        a.delete("world");
        a.insert("Evan");
        a.retain_rest(&rope)?;

        let mut b = Edit::new();
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
        let mut a = Edit::new();
        a.retain(5);
        a.insert("abc");
        a.retain_rest(&rope)?;

        let mut intermediate = rope.clone();
        a.apply(&mut intermediate)?;

        let mut b = Edit::new();
        b.retain(6);
        b.delete("b");
        b.retain_rest(&intermediate)?;

        let ab = a.compose(&b)?;
        let mut composed = rope.clone();
        ab.apply(&mut composed)?;
        assert_eq!(composed, Rope::from("Helloac, world!"));

        Ok(())
    }

    #[test]
    fn test_transform_byte_indexes() -> anyhow::Result<()> {
        let mut rope = Rope::from("Hello, world!");

        let mut edit = Edit::new();
        edit.delete("Hello, ");
        edit.retain(5);
        edit.insert("!!!");
        edit.retain(1);

        edit.apply(&mut rope)?;
        assert_eq!(rope, Rope::from("world!!!!"));

        // "H" and "," collapse to the deletion point; "r" follows the retained "world".
        let mut indexes = [0, 5, 9];
        edit.transform_byte_indexes(&mut indexes, Bias::Forward);
        assert_eq!(indexes, [0, 0, 2]);

        // An index at an insertion point stays before or moves after the inserted text,
        // depending on bias.
        let mut edit = Edit::new();
        edit.retain(5);
        edit.insert("???");
        edit.retain_rest(&rope)?;

        let mut indexes = [5];
        edit.transform_byte_indexes(&mut indexes, Bias::Backward);
        assert_eq!(indexes, [5]);

        let mut indexes = [5];
        edit.transform_byte_indexes(&mut indexes, Bias::Forward);
        assert_eq!(indexes, [8]);

        Ok(())
    }

    // Note [Canonical form]
    #[test]
    fn test_canonical_delete_before_insert() -> anyhow::Result<()> {
        let rope = Rope::from("Hello, world!");

        // Equivalent edits built in either order have the same representation...
        let mut insert_first = Edit::new();
        insert_first.retain(7);
        insert_first.insert("there");
        insert_first.delete("world");
        insert_first.retain(1);

        let mut delete_first = Edit::new();
        delete_first.retain(7);
        delete_first.delete("world");
        delete_first.insert("there");
        delete_first.retain(1);

        assert_eq!(insert_first, delete_first);

        let mut applied = rope.clone();
        insert_first.apply(&mut applied)?;
        assert_eq!(applied, Rope::from("Hello, there!"));

        // ...and therefore rebase identically.
        let mut concurrent = Edit::new();
        concurrent.retain(7);
        concurrent.insert("big ");
        concurrent.retain_rest(&rope)?;

        assert_eq!(
            insert_first.rebase(&concurrent, Bias::Forward)?,
            delete_first.rebase(&concurrent, Bias::Forward)?
        );

        // Deletes merge across an intervening insert, and inverting stays canonical.
        let mut edit = Edit::new();
        edit.delete("He");
        edit.insert("Ye");
        edit.delete("llo");
        edit.retain_rest(&rope)?;

        let mut applied = rope.clone();
        edit.apply(&mut applied)?;
        assert_eq!(applied, Rope::from("Ye, world!"));

        edit.invert().apply(&mut applied)?;
        assert_eq!(applied, rope);

        Ok(())
    }

    #[test]
    fn test_rebase() -> anyhow::Result<()> {
        let rope = Rope::from("Hello, world!");

        // Concurrent inserts at the same position: bias decides whose text comes first.
        let mut a = Edit::new();
        a.retain(7);
        a.insert("brave ");
        a.retain_rest(&rope)?;

        let mut b = Edit::new();
        b.retain(7);
        b.insert("new ");
        b.retain_rest(&rope)?;

        let mut backward = rope.clone();
        b.apply(&mut backward)?;
        a.rebase(&b, Bias::Backward)?.apply(&mut backward)?;
        assert_eq!(backward, Rope::from("Hello, brave new world!"));

        let mut forward = rope.clone();
        b.apply(&mut forward)?;
        a.rebase(&b, Bias::Forward)?.apply(&mut forward)?;
        assert_eq!(forward, Rope::from("Hello, new brave world!"));

        // TP1 convergence: rebasing with opposite biases converges regardless of which edit is
        // applied first.
        let mut converged = rope.clone();
        a.apply(&mut converged)?;
        b.rebase(&a, Bias::Forward)?.apply(&mut converged)?;
        assert_eq!(converged, backward);

        // Overlapping deletes don't delete twice.
        let mut a = Edit::new();
        a.retain(7);
        a.delete("world");
        a.retain(1);

        let mut b = Edit::new();
        b.retain(5);
        b.delete(", world");
        b.retain(1);

        let mut one = rope.clone();
        a.apply(&mut one)?;
        b.rebase(&a, Bias::Backward)?.apply(&mut one)?;
        assert_eq!(one, Rope::from("Hello!"));

        let mut two = rope.clone();
        b.apply(&mut two)?;
        a.rebase(&b, Bias::Forward)?.apply(&mut two)?;
        assert_eq!(one, two);

        Ok(())
    }
}
