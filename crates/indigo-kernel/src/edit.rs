use ropey::Rope;
use std::{cmp::min, iter::zip, ops::Range};

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Edit {
    op_kinds: Vec<OpKind>,
    op_lengths: Vec<u32>,
    delete_texts: String,
    insert_texts: String,

    length_before: usize,
    length_after: usize,
}

impl Edit {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    fn with_capacity(
        op_count: usize,
        delete_text_length: usize,
        insert_text_length: usize,
    ) -> Self {
        Self {
            op_kinds: Vec::with_capacity(op_count),
            op_lengths: Vec::with_capacity(op_count),
            delete_texts: String::with_capacity(delete_text_length),
            insert_texts: String::with_capacity(insert_text_length),
            length_before: 0,
            length_after: 0,
        }
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
        self.delete_texts.push_str(text);

        // Note [Canonical form]
        let last = self.op_kinds.last().copied();
        let second_last = self.op_kinds.len().checked_sub(2).map(|i| self.op_kinds[i]);

        match (second_last, last) {
            (_, Some(OpKind::Delete)) => {
                let last = self.op_lengths.last_mut().unwrap();
                *last = to_u32(to_usize(*last) + byte_length);
            }
            (Some(OpKind::Delete), Some(OpKind::Insert)) => {
                let delete_index = self.op_lengths.len() - 2;
                self.op_lengths[delete_index] =
                    to_u32(to_usize(self.op_lengths[delete_index]) + byte_length);
            }
            (_, Some(OpKind::Insert)) => {
                let insert_index = self.op_kinds.len() - 1;
                self.op_kinds.insert(insert_index, OpKind::Delete);
                self.op_lengths.insert(insert_index, to_u32(byte_length));
            }
            (_, Some(OpKind::Retain) | None) => {
                self.op_kinds.push(OpKind::Delete);
                self.op_lengths.push(to_u32(byte_length));
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

        self.insert_texts.push_str(text);
    }

    pub fn compose(&self, other: &Self) -> anyhow::Result<Self> {
        anyhow::ensure!(
            self.length_after == other.length_before,
            "left edit output length {} != right edit input length {}",
            self.length_after,
            other.length_before,
        );

        let mut a = OpCursor::new(self);
        let mut b = OpCursor::new(other);
        let mut edit = Self::with_capacity(
            self.op_kinds.len() + other.op_kinds.len(),
            self.delete_texts.len() + other.delete_texts.len(),
            self.insert_texts.len() + other.insert_texts.len(),
        );

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
        let mut edit = Self::with_capacity(
            self.op_kinds.len(),
            self.insert_texts.len(),
            self.delete_texts.len(),
        );
        let mut delete_texts_index = 0;
        let mut insert_texts_index = 0;

        for (kind, length) in zip(&self.op_kinds, &self.op_lengths) {
            let length = to_usize(*length);
            match kind {
                OpKind::Retain => edit.retain(length),
                OpKind::Delete => {
                    edit.insert(
                        &self.delete_texts[delete_texts_index..delete_texts_index + length],
                    );
                    delete_texts_index += length;
                }
                OpKind::Insert => {
                    edit.delete(
                        &self.insert_texts[insert_texts_index..insert_texts_index + length],
                    );
                    insert_texts_index += length;
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

        let mut a = OpCursor::new(self);
        let mut b = OpCursor::new(onto);
        let mut edit = Self::with_capacity(
            self.op_kinds.len() + onto.op_kinds.len(),
            self.delete_texts.len(),
            self.insert_texts.len(),
        );

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

    /// Maps byte indexes into the document before the edit to byte indexes into the document
    /// after the edit. Input must be sorted and within the edit's input length.
    pub fn transform_byte_indexes(&self, byte_indexes: &mut [usize], bias: Bias) {
        fn apply_delta(base: usize, delta: isize) -> usize {
            if delta >= 0 {
                base + delta.unsigned_abs()
            } else {
                base - delta.unsigned_abs()
            }
        }

        debug_assert!(byte_indexes.is_sorted());
        debug_assert!(
            byte_indexes
                .last()
                .is_none_or(|last| *last <= self.length_before)
        );

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

        debug_assert!(byte_indexes.is_sorted());
        debug_assert!(
            byte_indexes
                .last()
                .is_none_or(|last| *last <= self.length_after)
        );
    }

    pub fn apply(&self, rope: &mut Rope) -> anyhow::Result<()> {
        self.apply_impl(rope, false)
    }

    /// Equivalent to `self.invert().apply(rope)`, but without allocating.
    pub fn apply_inverse(&self, rope: &mut Rope) -> anyhow::Result<()> {
        self.apply_impl(rope, true)
    }

    fn apply_impl(&self, rope: &mut Rope, invert: bool) -> anyhow::Result<()> {
        // Note [Canonical form]
        #[cfg(debug_assertions)]
        self.assert_is_canonical();

        let (length_before, length_after, insert_texts) = if invert {
            (self.length_after, self.length_before, &self.delete_texts)
        } else {
            (self.length_before, self.length_after, &self.insert_texts)
        };
        #[cfg(debug_assertions)]
        let delete_texts = if invert {
            &self.insert_texts
        } else {
            &self.delete_texts
        };

        anyhow::ensure!(
            length_before == rope.len(),
            "edit input length {} != rope length {}",
            length_before,
            rope.len()
        );

        let mut rope_index = 0;
        let mut insert_texts_index = 0;
        #[cfg(debug_assertions)]
        let mut delete_texts_index = 0;

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
                    {
                        verify_deleted_text(
                            rope,
                            range.clone(),
                            &delete_texts[delete_texts_index..delete_texts_index + length],
                        )?;
                        delete_texts_index += length;
                    }
                    rope.try_remove(range)?;
                }
                OpKind::Insert => {
                    let text = &insert_texts[insert_texts_index..insert_texts_index + length];
                    insert_texts_index += length;
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
        let mut delete_text_length = 0;
        let mut insert_text_length = 0;

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
                    delete_text_length += length;
                }
                OpKind::Insert => {
                    length_after += length;
                    insert_text_length += length;
                }
            }
        }

        assert_eq!(length_before, self.length_before);
        assert_eq!(length_after, self.length_after);
        assert_eq!(delete_text_length, self.delete_texts.len());
        assert_eq!(insert_text_length, self.insert_texts.len());
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
    delete_text_index: usize,
    insert_text_index: usize,
}

impl<'a> OpCursor<'a> {
    fn new(edit: &'a Edit) -> Self {
        Self {
            edit,
            op_index: 0,
            consumed: 0,
            delete_text_index: 0,
            insert_text_index: 0,
        }
    }

    fn kind(&self) -> Option<OpKind> {
        self.edit.op_kinds.get(self.op_index).copied()
    }

    fn remaining(&self) -> usize {
        to_usize(self.edit.op_lengths[self.op_index]) - self.consumed
    }

    fn text(&self, n: usize) -> &'a str {
        let (texts, text_index) = match self.kind() {
            Some(OpKind::Delete) => (&self.edit.delete_texts, self.delete_text_index),
            Some(OpKind::Insert) => (&self.edit.insert_texts, self.insert_text_index),
            Some(OpKind::Retain) | None => unreachable!("text of non-text op"),
        };
        let start = text_index + self.consumed;
        &texts[start..start + n]
    }

    fn advance(&mut self, n: usize) {
        self.consumed += n;
        if self.consumed == to_usize(self.edit.op_lengths[self.op_index]) {
            match self.kind() {
                Some(OpKind::Delete) => self.delete_text_index += self.consumed,
                Some(OpKind::Insert) => self.insert_text_index += self.consumed,
                Some(OpKind::Retain) | None => {}
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
    use hegel::generators as gs;

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

        Ok(())
    }

    /// Draws a byte index in `(index, doc.len()]` lying on a `char` boundary.
    fn draw_str_end(tc: &hegel::TestCase, doc: &str, index: usize) -> usize {
        let length = gs::integers::<usize>()
            .min_value(1)
            .max_value(doc.len() - index);
        let mut end = index + tc.draw(length);
        while !doc.is_char_boundary(end) {
            end += 1;
        }
        end
    }

    /// A random canonical edit whose input document is exactly `doc`, paired with the document
    /// applying it should produce. The expected document is built from the generation choices
    /// directly, so it's an oracle independent of the `Edit` representation.
    #[hegel::composite]
    fn gen_edit_and_text(tc: hegel::TestCase, doc: String) -> (Edit, String) {
        let mut edit = Edit::new();
        let mut expected = String::new();
        let mut index = 0;
        while index < doc.len() {
            match tc.draw(gs::integers::<u8>().max_value(2)) {
                0 => {
                    let end = draw_str_end(&tc, &doc, index);
                    edit.retain(end - index);
                    expected.push_str(&doc[index..end]);
                    index = end;
                }
                1 => {
                    let end = draw_str_end(&tc, &doc, index);
                    edit.delete(&doc[index..end]);
                    index = end;
                }
                _ => {
                    let text: String = tc.draw(gs::text());
                    edit.insert(&text);
                    expected.push_str(&text);
                }
            }
        }
        if tc.draw(gs::booleans()) {
            let text: String = tc.draw(gs::text());
            edit.insert(&text);
            expected.push_str(&text);
        }
        (edit, expected)
    }

    /// The edit from `gen_edit_and_text`, for tests that don't need the expected document.
    #[hegel::composite]
    fn gen_edit(tc: hegel::TestCase, doc: String) -> Edit {
        tc.draw(gen_edit_and_text(doc.clone())).0
    }

    #[hegel::test(test_cases = 2_000)]
    fn apply_produces_expected_document(tc: hegel::TestCase) {
        let doc: String = tc.draw(gs::text());
        let (edit, expected) = tc.draw(gen_edit_and_text(doc.clone()));

        let mut rope = Rope::from(doc.as_str());
        edit.apply(&mut rope).unwrap();

        assert_eq!(rope, Rope::from(expected.as_str()));
    }

    #[hegel::test(test_cases = 2_000)]
    fn invert_roundtrips(tc: hegel::TestCase) {
        let doc: String = tc.draw(gs::text());
        let edit = tc.draw(gen_edit(doc.clone()));

        let before = Rope::from(doc.as_str());
        let mut after = before.clone();
        edit.apply(&mut after).unwrap();

        // Applying the inverse undoes the edit...
        let mut inverted = after.clone();
        edit.invert().apply(&mut inverted).unwrap();
        assert_eq!(inverted, before);

        // ...`apply_inverse` agrees with `invert` + `apply`...
        let mut uninverted = after.clone();
        edit.apply_inverse(&mut uninverted).unwrap();
        assert_eq!(uninverted, before);

        // ...and inverting is an involution.
        assert_eq!(edit.invert().invert(), edit);
    }

    // Note [Canonical form]
    #[hegel::test(test_cases = 2_000)]
    fn canonical_form_is_unique(tc: hegel::TestCase) {
        /// A `char` boundary near the middle of `s`.
        fn midpoint(s: &str) -> usize {
            let mut index = s.len() / 2;
            while !s.is_char_boundary(index) {
                index += 1;
            }
            index
        }

        let doc: String = tc.draw(gs::text());

        // Build the same edit two ways: `one` deletes before inserting, in whole ops; `two`
        // inserts before deleting, in split ops. Canonical form erases the difference.
        let mut one = Edit::new();
        let mut two = Edit::new();
        let mut index = 0;
        while index < doc.len() {
            let end = draw_str_end(&tc, &doc, index);
            if tc.draw(gs::booleans()) {
                one.retain(end - index);
                let middle = index + midpoint(&doc[index..end]);
                two.retain(middle - index);
                two.retain(end - middle);
            } else {
                let deleted = &doc[index..end];
                let inserted: String = tc.draw(gs::text());
                one.delete(deleted);
                one.insert(&inserted);
                let (delete_head, delete_tail) = deleted.split_at(midpoint(deleted));
                let (insert_head, insert_tail) = inserted.split_at(midpoint(&inserted));
                two.insert(insert_head);
                two.insert(insert_tail);
                two.delete(delete_head);
                two.delete(delete_tail);
            }
            index = end;
        }

        assert_eq!(one, two);
        one.assert_is_canonical();
    }

    #[hegel::test(test_cases = 2_000)]
    fn compose_agrees_with_sequential_application(tc: hegel::TestCase) {
        let doc: String = tc.draw(gs::text());
        let a = tc.draw(gen_edit(doc.clone()));

        let mut sequential = Rope::from(doc.as_str());
        a.apply(&mut sequential).unwrap();
        let b = tc.draw(gen_edit(sequential.to_string()));
        b.apply(&mut sequential).unwrap();

        let mut composed = Rope::from(doc.as_str());
        a.compose(&b).unwrap().apply(&mut composed).unwrap();

        assert_eq!(composed, sequential);
    }

    #[hegel::test(test_cases = 2_000)]
    fn compose_is_associative(tc: hegel::TestCase) {
        let doc: String = tc.draw(gs::text());
        let mut rope = Rope::from(doc.as_str());

        let a = tc.draw(gen_edit(rope.to_string()));
        a.apply(&mut rope).unwrap();
        let b = tc.draw(gen_edit(rope.to_string()));
        b.apply(&mut rope).unwrap();
        let c = tc.draw(gen_edit(rope.to_string()));

        assert_eq!(
            a.compose(&b).unwrap().compose(&c).unwrap(),
            a.compose(&b.compose(&c).unwrap()).unwrap(),
        );
    }

    #[hegel::test(test_cases = 2_000)]
    fn compose_with_identity_is_identity(tc: hegel::TestCase) {
        let doc: String = tc.draw(gs::text());
        let edit = tc.draw(gen_edit(doc.clone()));

        let mut before = Edit::new();
        before.retain(edit.length_before);
        assert_eq!(before.compose(&edit).unwrap(), edit);

        let mut after = Edit::new();
        after.retain(edit.length_after);
        assert_eq!(edit.compose(&after).unwrap(), edit);
    }

    // TP1 convergence: rebasing with opposite biases converges regardless of which edit is applied
    // first.
    #[hegel::test(test_cases = 2_000)]
    fn rebase_converges(tc: hegel::TestCase) {
        let doc: String = tc.draw(gs::text());
        let a = tc.draw(gen_edit(doc.clone()));
        let b = tc.draw(gen_edit(doc.clone()));

        let mut a_first = Rope::from(doc.as_str());
        a.apply(&mut a_first).unwrap();
        b.rebase(&a, Bias::Forward)
            .unwrap()
            .apply(&mut a_first)
            .unwrap();

        let mut b_first = Rope::from(doc.as_str());
        b.apply(&mut b_first).unwrap();
        a.rebase(&b, Bias::Backward)
            .unwrap()
            .apply(&mut b_first)
            .unwrap();

        assert_eq!(a_first, b_first);
    }

    #[hegel::test(test_cases = 2_000)]
    fn transform_byte_indexes_preserves_order_and_validity(tc: hegel::TestCase) {
        let doc: String = tc.draw(gs::text());
        let edit = tc.draw(gen_edit(doc.clone()));

        let mut indexes: Vec<usize> =
            tc.draw(gs::vecs(gs::integers::<usize>().max_value(doc.len())));
        for index in &mut indexes {
            while !doc.is_char_boundary(*index) {
                *index -= 1;
            }
        }
        indexes.sort_unstable();

        let mut rope = Rope::from(doc.as_str());
        edit.apply(&mut rope).unwrap();
        let after = rope.to_string();

        let mut backward = indexes.clone();
        edit.transform_byte_indexes(&mut backward, Bias::Backward);
        let mut forward = indexes;
        edit.transform_byte_indexes(&mut forward, Bias::Forward);

        // Transformed indexes are valid positions in the new document (sortedness and bounds are
        // debug-asserted inside `transform_byte_indexes` itself), and backward bias never lands
        // after forward bias.
        for (backward, forward) in zip(&backward, &forward) {
            assert!(after.is_char_boundary(*backward));
            assert!(after.is_char_boundary(*forward));
            assert!(backward <= forward);
        }
    }
}
