//! Operational transformations.
//!
//! <https://en.wikipedia.org/wiki/Operational_transformation>

use ropey::Rope;
use std::{cmp::min, ops::Deref, rc::Rc};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Length mismatch: {left} != {right}")]
    LengthMismatch { left: usize, right: usize },

    #[error("Char offset {char_offset} exceeds rope length {len_chars}")]
    CharOffsetPastEnd {
        char_offset: usize,
        len_chars: usize,
    },

    #[error("Failed to compose: first sequence produces more chars than second expects")]
    ComposeFirstProducesMore,

    #[error("Failed to compose: second sequence expects more chars than first produces")]
    ComposeSecondExpectsMore,

    #[error("Error from rope")]
    Rope(#[from] ropey::Error),
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct EditSeq {
    edits: Vec<Edit>,
    source_chars: usize,
    target_chars: usize,
}

impl EditSeq {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            edits: Vec::with_capacity(capacity),
            source_chars: 0,
            target_chars: 0,
        }
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.edits.is_empty()
    }

    pub fn retain(&mut self, char_length: usize) {
        if char_length == 0 {
            return;
        }

        self.source_chars += char_length;
        self.target_chars += char_length;

        if let Some(Edit::Retain(n)) = self.edits.last_mut() {
            *n += char_length;
        } else {
            self.edits.push(Edit::Retain(char_length));
        }
    }

    pub fn retain_rest(&mut self, rope: &Rope) {
        // TODO: Return custom error if `source_chars` exceeds rope length.
        let char_length = rope.len_chars() - self.source_chars;

        self.retain(char_length);
    }

    pub fn delete(&mut self, char_length: usize) {
        if char_length == 0 {
            return;
        }

        self.source_chars += char_length;

        if let Some(Edit::Delete(n)) = self.edits.last_mut() {
            *n += char_length;
        } else {
            self.edits.push(Edit::Delete(char_length));
        }
    }

    pub fn insert(&mut self, text: &str) {
        if text.is_empty() {
            return;
        }

        self.target_chars += text.chars().count();

        if let Some(Edit::Insert(last)) = self.edits.last_mut() {
            let mut s = String::with_capacity(last.chars().count() + text.chars().count());
            s.push_str(last);
            s.push_str(text);
            *last = Rc::from(s);
        } else {
            self.edits.push(Edit::Insert(Rc::from(text)));
        }
    }

    pub fn push(&mut self, edit: Edit) {
        match edit {
            Edit::Retain(n) => self.retain(n),
            Edit::Delete(n) => self.delete(n),
            Edit::Insert(s) => self.insert(&s),
        }
    }

    pub fn compose(&self, other: &Self) -> anyhow::Result<Self> {
        if self.target_chars != other.source_chars {
            anyhow::bail!(Error::LengthMismatch {
                left: self.target_chars,
                right: other.source_chars,
            });
        }

        let mut result = Self::with_capacity(self.len() + other.len());

        let mut iter1 = self.edits.iter().cloned();
        let mut iter2 = other.edits.iter().cloned();

        let mut op1 = iter1.next();
        let mut op2 = iter2.next();

        while op1.is_some() || op2.is_some() {
            match (&mut op1, &mut op2) {
                // Delete from first: pass through, doesn't consume from second
                (Some(Edit::Delete(n)), _) => {
                    result.delete(*n);
                    op1 = iter1.next();
                }

                // Insert from second: pass through, doesn't consume from first
                (_, Some(Edit::Insert(s))) => {
                    result.insert(s);
                    op2 = iter2.next();
                }

                // Retain + Retain: both consume, output retain
                (Some(Edit::Retain(n1)), Some(Edit::Retain(n2))) => {
                    let n = min(*n1, *n2);
                    result.retain(n);
                    *n1 -= n;
                    *n2 -= n;
                    if *n1 == 0 {
                        op1 = iter1.next();
                    }
                    if *n2 == 0 {
                        op2 = iter2.next();
                    }
                }

                // Retain + Delete: both consume, output delete
                (Some(Edit::Retain(n1)), Some(Edit::Delete(n2))) => {
                    let n = min(*n1, *n2);
                    result.delete(n);
                    *n1 -= n;
                    *n2 -= n;
                    if *n1 == 0 {
                        op1 = iter1.next();
                    }
                    if *n2 == 0 {
                        op2 = iter2.next();
                    }
                }

                // Insert + Retain: second retains inserted text
                (Some(Edit::Insert(s1)), Some(Edit::Retain(n2))) => {
                    let len1 = s1.chars().count();
                    let n = len1.min(*n2);

                    if n == len1 {
                        // Second retains all of first's insert
                        result.insert(s1);
                        op1 = iter1.next();
                    } else {
                        // Second retains only part: split the insert
                        let byte_offset = s1.chars().take(n).map(char::len_utf8).sum();
                        let before = Rc::from(&s1[..byte_offset]);
                        let after = Rc::from(&s1[byte_offset..]);
                        result.insert(&before);
                        *s1 = after;
                    }

                    *n2 -= n;
                    if *n2 == 0 {
                        op2 = iter2.next();
                    }
                }

                // Insert + Delete: operations cancel out
                (Some(Edit::Insert(s1)), Some(Edit::Delete(n2))) => {
                    let len1 = s1.chars().count();
                    let n = len1.min(*n2);

                    if n == len1 {
                        // All of insert is deleted
                        op1 = iter1.next();
                    } else {
                        // Only part deleted: keep the rest
                        let byte_offset = s1.chars().take(n).map(char::len_utf8).sum();
                        let after = Rc::from(&s1[byte_offset..]);
                        *s1 = after;
                    }

                    *n2 -= n;
                    if *n2 == 0 {
                        op2 = iter2.next();
                    }
                }

                (None, Some(Edit::Retain(_) | Edit::Delete(_))) => {
                    anyhow::bail!(Error::ComposeSecondExpectsMore);
                }

                (Some(Edit::Retain(_) | Edit::Insert(_)), None) => {
                    anyhow::bail!(Error::ComposeFirstProducesMore);
                }

                (None, None) => break,
            }
        }

        Ok(result)
    }

    // TODO
    #[doc(hidden)]
    pub fn transform(&self, other: &Self) -> anyhow::Result<(Self, Self)> {
        if self.source_chars != other.source_chars {
            anyhow::bail!(Error::LengthMismatch {
                left: self.source_chars,
                right: other.source_chars,
            });
        }

        // https://github.com/spebern/operational-transform-rs/blob/9faa17f0a2b282ac2e09dbb2d29fdaf2ae0bbb4a/operational-transform/src/lib.rs#L344

        todo!()
    }

    #[must_use]
    pub fn transform_char_offset(&self, mut char_offset: usize) -> usize {
        let mut position = 0;

        for edit in &self.edits {
            match edit {
                Edit::Retain(n) => {
                    position += n;
                }
                Edit::Delete(n) => {
                    if position < char_offset {
                        char_offset -= n;
                    }
                }
                Edit::Insert(s) => {
                    if position <= char_offset {
                        char_offset += s.chars().count();
                    }
                    position += s.chars().count();
                }
            }
        }

        char_offset
    }

    pub fn invert(&self, rope: &Rope) -> anyhow::Result<Self> {
        let length_mismatch = || Error::LengthMismatch {
            left: self.source_chars,
            right: rope.len_chars(),
        };

        if self.source_chars != rope.len_chars() {
            anyhow::bail!(length_mismatch());
        }

        let mut inverted = Self::with_capacity(self.len());

        for edit in &self.edits {
            match edit {
                Edit::Retain(n) => inverted.retain(*n),
                Edit::Delete(n) => {
                    let start = inverted.target_chars;
                    let end = start + n;
                    let s = rope
                        .get_slice(start..end)
                        .ok_or_else(length_mismatch)?
                        .to_string();
                    inverted.insert(&s);
                }
                Edit::Insert(s) => inverted.delete(s.chars().count()),
            }
        }

        Ok(inverted)
    }

    pub fn apply(&self, rope: &mut Rope) -> anyhow::Result<()> {
        if self.source_chars != rope.len_chars() {
            anyhow::bail!(Error::LengthMismatch {
                left: self.source_chars,
                right: rope.len_chars(),
            });
        }

        let mut char_offset = 0;

        for edit in &self.edits {
            char_offset = edit.apply(char_offset, rope)?;
        }

        Ok(())
    }
}

impl Deref for EditSeq {
    type Target = [Edit];

    fn deref(&self) -> &Self::Target {
        &self.edits
    }
}

impl IntoIterator for EditSeq {
    type Item = Edit;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.edits.into_iter()
    }
}

impl<'a> IntoIterator for &'a EditSeq {
    type Item = &'a Edit;

    type IntoIter = std::slice::Iter<'a, Edit>;

    fn into_iter(self) -> Self::IntoIter {
        self.edits.iter()
    }
}

impl Extend<Edit> for EditSeq {
    fn extend<T>(&mut self, edits: T)
    where
        T: IntoIterator<Item = Edit>,
    {
        for edit in edits {
            self.push(edit);
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Edit {
    Retain(usize),
    Delete(usize),
    Insert(Rc<str>),
}

impl Edit {
    pub fn apply(&self, char_offset: usize, rope: &mut Rope) -> anyhow::Result<usize> {
        let char_offset = match self {
            Self::Retain(n) => char_offset + n,
            Self::Delete(n) => {
                rope.try_remove(char_offset..char_offset + n)?;
                char_offset
            }
            Self::Insert(s) => {
                rope.try_insert(char_offset, s)?;
                char_offset + s.chars().count()
            }
        };

        if char_offset > rope.len_chars() {
            anyhow::bail!(Error::CharOffsetPastEnd {
                char_offset,
                len_chars: rope.len_chars(),
            });
        }

        Ok(char_offset)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn edit_seq_push() {
        let mut edits = EditSeq::new();
        edits.extend([
            Edit::Insert(Rc::from("Hello,")),
            Edit::Insert(Rc::from(" world!")),
            Edit::Retain(42),
            Edit::Retain(58),
            Edit::Delete(4),
            Edit::Delete(4),
            Edit::Delete(4),
        ]);
        assert_eq!(
            *edits,
            [
                Edit::Insert(Rc::from("Hello, world!")),
                Edit::Retain(100),
                Edit::Delete(12),
            ]
        );
    }

    #[test]
    fn edits_transform_offset() {
        let mut rope = Rope::from("Hello, world!");

        let mut char_offset = 9;
        assert_eq!(rope.char(char_offset), 'r');

        let mut edits = EditSeq::new();
        edits.delete("Hello, ".chars().count());
        edits.retain("world".chars().count());
        edits.insert("!!!");
        edits.retain("!".chars().count());

        edits.apply(&mut rope).unwrap();
        assert_eq!(rope, Rope::from("world!!!!"));

        assert_eq!(rope.get_char(char_offset), None);
        char_offset = edits.transform_char_offset(char_offset);
        assert_eq!(char_offset, 2);
        assert_eq!(rope.char(char_offset), 'r');

        let mut edits = EditSeq::new();
        edits.delete("w".chars().count());
        edits.insert("the whole w");
        edits.retain_rest(&rope);
        edits.apply(&mut rope).unwrap();
        assert_eq!(rope, Rope::from("the whole world!!!!"));

        assert_eq!(rope.char(char_offset), 'e');
        char_offset = edits.transform_char_offset(char_offset);
        assert_eq!(char_offset, 12);
        assert_eq!(rope.char(char_offset), 'r');
    }

    #[test]
    fn edits_invert() {
        let rope1 = Rope::from("Hello, world!");

        let mut edits = EditSeq::new();
        edits.delete("H".chars().count());
        edits.insert("Y");
        edits.retain("ello".chars().count());
        edits.insert("w");
        edits.delete(", world!".chars().count());
        edits.insert(" and pink");

        let mut rope2 = rope1.clone();
        edits.apply(&mut rope2).unwrap();
        assert_eq!(rope2, Rope::from("Yellow and pink"));

        assert_eq!(edits.invert(&rope1).unwrap().invert(&rope2).unwrap(), edits);

        let mut rope3 = rope2.clone();
        edits.invert(&rope1).unwrap().apply(&mut rope3).unwrap();
        assert_eq!(rope3, Rope::from("Hello, world!"));
    }

    #[test]
    fn edits_apply() {
        let mut edits = EditSeq::new();
        edits.retain(7);
        edits.delete("world".chars().count());
        edits.insert("Evan");
        edits.delete("!".chars().count());
        edits.insert("...");
        edits.insert("?");

        let mut rope = Rope::from("Hello, world!");
        edits.apply(&mut rope).unwrap();
        assert_eq!(rope, Rope::from("Hello, Evan...?"));

        let mut rope = Rope::from("Hello, world!");
        edits.retain(1);
        assert!(edits.apply(&mut rope).is_err());
    }

    #[test]
    fn edit_apply() {
        let mut rope = Rope::from("Hello, world!");
        let char_offset = 0;
        let char_offset = Edit::Retain(7).apply(char_offset, &mut rope).unwrap();
        let char_offset = Edit::Delete("world".chars().count())
            .apply(char_offset, &mut rope)
            .unwrap();
        let char_offset = Edit::Insert(Rc::from("Evan"))
            .apply(char_offset, &mut rope)
            .unwrap();
        let char_offset = Edit::Delete("!".chars().count())
            .apply(char_offset, &mut rope)
            .unwrap();
        let char_offset = Edit::Insert(Rc::from("..."))
            .apply(char_offset, &mut rope)
            .unwrap();
        let char_offset = Edit::Insert(Rc::from("?"))
            .apply(char_offset, &mut rope)
            .unwrap();
        assert_eq!(rope, Rope::from("Hello, Evan...?"));
        assert!(Edit::Retain(1).apply(char_offset, &mut rope).is_err());
    }

    #[test]
    fn edits_compose_basic() {
        // S0 = "abcdef"
        // A: Delete "ab", retain "cdef" -> S1 = "cdef"
        // B: Retain "cd", delete "ef" -> S2 = "cd"
        // A ∘ B should transform "abcdef" -> "cd"

        let mut edit_a = EditSeq::new();
        edit_a.delete(2); // Delete "ab"
        edit_a.retain(4); // Retain "cdef"

        let mut edit_b = EditSeq::new();
        edit_b.retain(2); // Retain "cd"
        edit_b.delete(2); // Delete "ef"

        let composed = edit_a.compose(&edit_b).unwrap();

        // Test the composition directly
        let mut rope = Rope::from("abcdef");
        composed.apply(&mut rope).unwrap();
        assert_eq!(rope, Rope::from("cd"));

        // Verify it equals sequential application
        let mut rope2 = Rope::from("abcdef");
        edit_a.apply(&mut rope2).unwrap();
        edit_b.apply(&mut rope2).unwrap();
        assert_eq!(rope, rope2);
    }

    #[test]
    fn edits_compose_insert() {
        // S0 = "hello"
        // A: Insert "X", retain "hello" -> S1 = "Xhello"
        // B: Retain "X", insert "Y", retain "hello" -> S2 = "XYhello"

        let mut edit_a = EditSeq::new();
        edit_a.insert("X");
        edit_a.retain(5);

        let mut edit_b = EditSeq::new();
        edit_b.retain(1);
        edit_b.insert("Y");
        edit_b.retain(5);

        let composed = edit_a.compose(&edit_b).unwrap();

        let mut rope = Rope::from("hello");
        composed.apply(&mut rope).unwrap();
        assert_eq!(rope, Rope::from("XYhello"));
    }

    #[test]
    fn edits_compose_insert_delete() {
        // S0 = "abc"
        // A: Retain "a", insert "XYZ", retain "bc" -> S1 = "aXYZbc"
        // B: Retain "a", delete "XY", retain "Zbc" -> S2 = "aZbc"

        let mut edit_a = EditSeq::new();
        edit_a.retain(1);
        edit_a.insert("XYZ");
        edit_a.retain(2);

        let mut edit_b = EditSeq::new();
        edit_b.retain(1);
        edit_b.delete(2);
        edit_b.retain(3);

        let composed = edit_a.compose(&edit_b).unwrap();

        let mut rope = Rope::from("abc");
        composed.apply(&mut rope).unwrap();
        assert_eq!(rope, Rope::from("aZbc"));

        // Verify property: apply(apply(S, A), B) = apply(S, compose(A, B))
        let mut rope2 = Rope::from("abc");
        edit_a.apply(&mut rope2).unwrap();
        edit_b.apply(&mut rope2).unwrap();
        assert_eq!(rope, rope2);
    }

    #[test]
    fn edits_compose_complex() {
        // S0 = "Hello, world!"
        // A: Delete "H", insert "Y", retain "ello", insert "w", delete ", world!", insert " and pink"
        //    -> S1 = "Yellow and pink"
        // B: Retain "Yellow", delete " and", retain " pink"
        //    -> S2 = "Yellow pink"

        let mut edit_a = EditSeq::new();
        edit_a.delete(1);
        edit_a.insert("Y");
        edit_a.retain(4);
        edit_a.insert("w");
        edit_a.delete(8);
        edit_a.insert(" and pink");

        let mut edit_b = EditSeq::new();
        edit_b.retain(6);
        edit_b.delete(4);
        edit_b.retain(5);

        let composed = edit_a.compose(&edit_b).unwrap();

        let mut rope = Rope::from("Hello, world!");
        composed.apply(&mut rope).unwrap();
        assert_eq!(rope, Rope::from("Yellow pink"));

        // Verify property
        let mut rope2 = Rope::from("Hello, world!");
        edit_a.apply(&mut rope2).unwrap();
        edit_b.apply(&mut rope2).unwrap();
        assert_eq!(rope, rope2);
    }

    #[test]
    fn edits_compose_length_mismatch() {
        let mut edit_a = EditSeq::new();
        edit_a.retain(5);

        let mut edit_b = EditSeq::new();
        edit_b.retain(3);

        assert!(edit_a.compose(&edit_b).is_err());
    }

    fn gen_edit_seq(
        u: &mut arbitrary::Unstructured<'_>,
        source_chars: usize,
    ) -> arbitrary::Result<EditSeq> {
        let mut edits = EditSeq::new();
        let mut remaining = source_chars;

        while remaining > 0 || u.arbitrary::<bool>()? {
            let op_type = u.int_in_range(0..=2)?;

            match op_type {
                // Retain
                0 if remaining > 0 => {
                    let n = u.int_in_range(1..=remaining)?;
                    edits.retain(n);
                    remaining -= n;
                }
                // Delete
                1 if remaining > 0 => {
                    let n = u.int_in_range(1..=remaining)?;
                    edits.delete(n);
                    remaining -= n;
                }
                // Insert
                2 => {
                    let len = u.int_in_range(1..=10)?;
                    let s: String = (0..len)
                        .map(|_| u.int_in_range(b'a'..=b'z').map(|b| char::from(b)))
                        .collect::<arbitrary::Result<_>>()?;
                    edits.insert(&s);

                    // If we've consumed all source chars, we can stop
                    if remaining == 0 {
                        break;
                    }
                }
                _ => {
                    // If remaining > 0, we must consume it
                    if remaining > 0 {
                        continue;
                    }
                    break;
                }
            }

            // Sometimes stop early to avoid making sequences too long
            if remaining == 0 && u.int_in_range(0..=2)? == 0 {
                break;
            }
        }

        // Ensure we consumed all source chars
        if remaining > 0 {
            edits.retain(remaining);
        }

        Ok(edits)
    }

    #[test]
    fn prop_compose() {
        arbtest::arbtest(|u| {
            // Generate a random initial rope
            let s_len = u.int_in_range(0..=50)?;
            let s: String = (0..s_len)
                .map(|_| u.int_in_range(b'a'..=b'z').map(|b| char::from(b)))
                .collect::<arbitrary::Result<_>>()?;
            let rope = Rope::from(s.as_str());

            // Generate edit sequence A that's valid for the rope
            let edit_a = gen_edit_seq(u, rope.len_chars())?;

            // Apply A to get intermediate rope
            let mut rope1 = rope.clone();
            edit_a.apply(&mut rope1).expect("edit_a.apply failed");

            // Generate edit sequence B that's valid for the intermediate rope
            let edit_b = gen_edit_seq(u, rope1.len_chars())?;

            // Test property: apply(apply(S, A), B) = apply(S, compose(A, B))
            let mut rope_sequential = rope.clone();
            edit_a
                .apply(&mut rope_sequential)
                .expect("sequential: edit_a.apply failed");
            edit_b
                .apply(&mut rope_sequential)
                .expect("sequential: edit_b.apply failed");

            let composed = edit_a.compose(&edit_b).expect("compose failed");
            let mut rope_composed = rope.clone();
            composed
                .apply(&mut rope_composed)
                .expect("composed: apply failed");

            assert_eq!(
                rope_sequential,
                rope_composed,
                "Property violation: apply(apply(S, A), B) != apply(S, compose(A, B))\n\
                 S = {:?}\n\
                 A = source:{} target:{}\n\
                 B = source:{} target:{}",
                rope.to_string(),
                edit_a.source_chars,
                edit_a.target_chars,
                edit_b.source_chars,
                edit_b.target_chars,
            );

            Ok(())
        });
    }
}
