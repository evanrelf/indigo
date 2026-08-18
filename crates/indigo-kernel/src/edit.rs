use ropey::Rope;
use std::cmp::min;

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Edit {
    ops: Vec<Op>,
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
        self.ops.is_empty()
    }

    pub fn retain(&mut self, byte_length: usize) {
        if byte_length == 0 {
            return;
        }

        self.length_before += byte_length;
        self.length_after += byte_length;

        if let Some(Op::Retain(last)) = self.ops.last_mut() {
            *last += byte_length;
        } else {
            self.ops.push(Op::Retain(byte_length));
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
        if text.is_empty() {
            return;
        }

        self.length_before += text.len();

        // Note [Canonical form]
        let insert = match self.ops.pop() {
            Some(Op::Insert(insert)) => Some(insert),
            Some(op) => {
                self.ops.push(op);
                None
            }
            None => None,
        };

        if let Some(Op::Delete(last)) = self.ops.last_mut() {
            last.push_str(text);
        } else {
            self.ops.push(Op::Delete(text.to_owned()));
        }

        if let Some(insert) = insert {
            self.ops.push(Op::Insert(insert));
        }
    }

    pub fn insert(&mut self, text: &str) {
        if text.is_empty() {
            return;
        }

        self.length_after += text.len();

        if let Some(Op::Insert(last)) = self.ops.last_mut() {
            last.push_str(text);
        } else {
            self.ops.push(Op::Insert(text.to_owned()));
        }
    }

    pub fn compose(&self, other: &Self) -> anyhow::Result<Self> {
        anyhow::ensure!(
            self.length_after == other.length_before,
            "left edit output length {} != right edit input length {}",
            self.length_after,
            other.length_before,
        );

        let mut a = Cursor::new(self);
        let mut b = Cursor::new(other);
        let mut edit = Self::new();

        loop {
            match (a.peek(), b.peek()) {
                // `self` deleted text `other` never saw.
                (Some(Op::Delete(_)), _) => {
                    let n = a.remaining();
                    edit.delete(a.text(n));
                    a.advance(n);
                }
                // `other` inserted text `self` never saw.
                (_, Some(Op::Insert(_))) => {
                    let n = b.remaining();
                    edit.insert(b.text(n));
                    b.advance(n);
                }
                (None, None) => break,
                (Some(a_op), Some(b_op)) => {
                    let n = min(a.remaining(), b.remaining());
                    match (a_op, b_op) {
                        (Op::Retain(_), Op::Retain(_)) => edit.retain(n),
                        (Op::Retain(_), Op::Delete(_)) => edit.delete(b.text(n)),
                        (Op::Insert(_), Op::Retain(_)) => edit.insert(a.text(n)),
                        // `other` deleted text `self` inserted; the ops cancel out
                        (Op::Insert(_), Op::Delete(_)) => debug_assert_eq!(a.text(n), b.text(n)),
                        _ => unreachable!(),
                    }
                    a.advance(n);
                    b.advance(n);
                }
                (None, Some(_)) | (Some(_), None) => unreachable!(),
            }
        }

        Ok(edit)
    }

    #[must_use]
    pub fn invert(&self) -> Self {
        let mut edit = Self::new();
        for op in &self.ops {
            match op {
                Op::Retain(length) => edit.retain(*length),
                Op::Delete(text) => edit.insert(text),
                Op::Insert(text) => edit.delete(text),
            }
        }
        edit
    }

    pub fn rebase(&self, onto: &Self, bias: Bias) -> anyhow::Result<Self> {
        anyhow::ensure!(
            self.length_before == onto.length_before,
            "self edit input length {} != onto edit input length {}",
            self.length_before,
            onto.length_before,
        );

        let mut a = Cursor::new(self);
        let mut b = Cursor::new(onto);
        let mut edit = Self::new();

        loop {
            match (a.peek(), b.peek()) {
                // Both inserted at the same position; `bias` decides whose text comes first.
                (Some(Op::Insert(_)), Some(Op::Insert(_))) => match bias {
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
                (Some(Op::Insert(_)), _) => {
                    let n = a.remaining();
                    edit.insert(a.text(n));
                    a.advance(n);
                }
                // `onto` inserted text `self` never saw; skip over it.
                (_, Some(Op::Insert(_))) => {
                    let n = b.remaining();
                    edit.retain(n);
                    b.advance(n);
                }
                (None, None) => break,
                (Some(a_op), Some(b_op)) => {
                    let n = min(a.remaining(), b.remaining());
                    match (a_op, b_op) {
                        (Op::Retain(_), Op::Retain(_)) => edit.retain(n),
                        // `onto` deleted this text; there's nothing left to retain
                        (Op::Retain(_), Op::Delete(_)) => {}
                        (Op::Delete(_), Op::Retain(_)) => edit.delete(a.text(n)),
                        // `onto` already deleted this text
                        (Op::Delete(_), Op::Delete(_)) => debug_assert_eq!(a.text(n), b.text(n)),
                        _ => unreachable!(),
                    }
                    a.advance(n);
                    b.advance(n);
                }
                (None, Some(_)) | (Some(_), None) => unreachable!(),
            }
        }

        Ok(edit)
    }

    /// Input must be sorted and within the edit's input length.
    pub fn map_positions(&self, byte_indexes: &mut [usize], bias: Bias) {
        let mut position: usize = 0;
        let mut delta: isize = 0;
        let mut i = 0;

        for op in &self.ops {
            if i >= byte_indexes.len() {
                break;
            }
            match op {
                Op::Retain(length) => {
                    while i < byte_indexes.len() && byte_indexes[i] < position + length {
                        byte_indexes[i] = byte_indexes[i].checked_add_signed(delta).unwrap();
                        i += 1;
                    }
                    position += length;
                }
                Op::Insert(text) => {
                    if let Bias::Backward = bias {
                        while i < byte_indexes.len() && byte_indexes[i] == position {
                            byte_indexes[i] = position.checked_add_signed(delta).unwrap();
                            i += 1;
                        }
                    }
                    delta += text.len().cast_signed();
                }
                Op::Delete(text) => {
                    while i < byte_indexes.len() && byte_indexes[i] < position + text.len() {
                        byte_indexes[i] = position.checked_add_signed(delta).unwrap();
                        i += 1;
                    }
                    position += text.len();
                    delta -= text.len().cast_signed();
                }
            }
        }

        while i < byte_indexes.len() {
            byte_indexes[i] = byte_indexes[i].checked_add_signed(delta).unwrap();
            i += 1;
        }
    }

    pub fn apply(&self, rope: &mut Rope) -> anyhow::Result<()> {
        anyhow::ensure!(
            self.length_before == rope.len(),
            "edit input length {} != rope length {}",
            self.length_before,
            rope.len()
        );

        let mut index = 0;

        for op in &self.ops {
            match op {
                Op::Retain(length) => index += length,
                Op::Delete(text) => {
                    rope.try_remove(index..index + text.len())?;
                }
                Op::Insert(text) => {
                    rope.try_insert(index, text)?;
                    index += text.len();
                }
            }
        }

        anyhow::ensure!(
            self.length_after == rope.len() && index == rope.len(),
            "edit output length {} != rope length {}",
            self.length_after,
            rope.len()
        );

        Ok(())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum Op {
    Retain(usize),
    Delete(String),
    Insert(String),
}

impl Op {
    fn len(&self) -> usize {
        match self {
            Self::Retain(length) => *length,
            Self::Delete(text) | Self::Insert(text) => text.len(),
        }
    }
}

#[derive(Clone, Copy)]
pub enum Bias {
    Backward,
    Forward,
}

struct Cursor<'a> {
    ops: &'a [Op],
    op_index: usize,
    consumed: usize,
}

impl<'a> Cursor<'a> {
    fn new(edit: &'a Edit) -> Self {
        Self {
            ops: &edit.ops,
            op_index: 0,
            consumed: 0,
        }
    }

    fn peek(&self) -> Option<&'a Op> {
        self.ops.get(self.op_index)
    }

    fn remaining(&self) -> usize {
        self.ops[self.op_index].len() - self.consumed
    }

    fn text(&self, n: usize) -> &'a str {
        match &self.ops[self.op_index] {
            Op::Retain(_) => unreachable!("text of a retain op"),
            Op::Delete(text) | Op::Insert(text) => &text[self.consumed..self.consumed + n],
        }
    }

    fn advance(&mut self, n: usize) {
        self.consumed += n;
        if self.consumed == self.ops[self.op_index].len() {
            self.op_index += 1;
            self.consumed = 0;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hegel::generators as gs;
    use std::iter::zip;

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
        edit.invert().apply(&mut rope)?;
        assert_eq!(rope, Rope::from("Hello, world!"));

        let mut rope = Rope::from("Hello, world!");
        edit.retain(1);
        assert!(edit.apply(&mut rope).is_err());

        Ok(())
    }

    #[test]
    fn test_map_positions() -> anyhow::Result<()> {
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
        edit.map_positions(&mut indexes, Bias::Forward);
        assert_eq!(indexes, [0, 0, 2]);

        // An index at an insertion point stays before or moves after the inserted text,
        // depending on bias.
        let mut edit = Edit::new();
        edit.retain(5);
        edit.insert("???");
        edit.retain_rest(&rope)?;

        let mut indexes = [5];
        edit.map_positions(&mut indexes, Bias::Backward);
        assert_eq!(indexes, [5]);

        let mut indexes = [5];
        edit.map_positions(&mut indexes, Bias::Forward);
        assert_eq!(indexes, [8]);

        Ok(())
    }

    #[test]
    fn test_rebase() -> anyhow::Result<()> {
        let rope = Rope::from("Hello, world!");

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

        // ...and inverting is an involution.
        assert_eq!(edit.invert().invert(), edit);
    }

    // See Note [Canonical form] in `edit.rs`.
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
        edit.map_positions(&mut backward, Bias::Backward);
        let mut forward = indexes;
        edit.map_positions(&mut forward, Bias::Forward);

        // Transformed indexes are valid, ordered positions in the new document, and backward bias
        // never lands after forward bias.
        assert!(backward.is_sorted());
        assert!(forward.is_sorted());
        for (backward, forward) in zip(&backward, &forward) {
            assert!(after.is_char_boundary(*backward));
            assert!(after.is_char_boundary(*forward));
            assert!(backward <= forward);
        }
    }
}
