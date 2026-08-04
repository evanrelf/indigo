use crate::{history::History, ot::OperationSeq};
use ropey::Rope;
use std::ops::{Deref, Range};

#[derive(Clone, Debug)]
struct BidiOperationSeq {
    /// Inverted operations. Apply to undo the edit.
    undo: OperationSeq,
    /// Original operations. Apply to perform the edit.
    redo: OperationSeq,
}

impl Extend<Self> for BidiOperationSeq {
    fn extend<T>(&mut self, opss: T)
    where
        T: IntoIterator<Item = Self>,
    {
        for ops in opss {
            if self.redo.is_empty() {
                *self = ops;
            } else {
                self.undo = ops.undo.compose(&self.undo).unwrap();
                self.redo = self.redo.compose(&ops.redo).unwrap();
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Text {
    rope: Rope,
    history: History<BidiOperationSeq, BidiOperationSeq>,
    log: Vec<OperationSeq>,
    pub readonly: bool,
}

impl Default for Text {
    fn default() -> Self {
        Self {
            rope: Rope::from("\n"),
            history: History::default(),
            log: Vec::new(),
            readonly: false,
        }
    }
}

impl Text {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn rope(&self) -> &Rope {
        &self.rope
    }

    pub fn insert(&mut self, byte_offset: usize, text: &str) -> anyhow::Result<()> {
        let mut ops = OperationSeq::new();
        ops.retain(byte_offset);
        ops.insert(text);
        ops.retain_rest(&self.rope);
        self.apply(&ops)?;
        Ok(())
    }

    pub fn delete(&mut self, range: Range<usize>) -> anyhow::Result<()> {
        let mut ops = OperationSeq::new();
        ops.retain(range.start);
        ops.delete(range.end - range.start);
        if range.end == self.rope.len()
            && (range.start == 0 || self.rope.byte(range.start - 1) != b'\n')
        {
            ops.insert("\n");
        }
        ops.retain_rest(&self.rope);
        self.apply(&ops)?;
        Ok(())
    }

    pub fn apply(&mut self, ops: &OperationSeq) -> anyhow::Result<()> {
        anyhow::ensure!(!self.readonly, "Cannot modify readonly text");
        let undo = ops.invert(&self.rope)?;
        ops.apply(&mut self.rope)?;
        self.history.push(BidiOperationSeq {
            redo: ops.clone(),
            undo,
        });
        self.log.push(ops.clone());
        Ok(())
    }

    pub fn commit(&mut self) {
        self.history.commit();
    }

    pub fn undo(&mut self) -> anyhow::Result<bool> {
        anyhow::ensure!(!self.readonly, "Cannot modify readonly text");
        if let Some(ops) = self.history.undo() {
            ops.undo.apply(&mut self.rope)?;
            self.log.push(ops.undo.clone());
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub fn redo(&mut self) -> anyhow::Result<bool> {
        anyhow::ensure!(!self.readonly, "Cannot modify readonly text");
        if let Some(ops) = self.history.redo() {
            ops.redo.apply(&mut self.rope)?;
            self.log.push(ops.redo.clone());
            Ok(true)
        } else {
            Ok(false)
        }
    }

    #[must_use]
    pub fn create_anchor(&self, byte_offset: usize) -> Anchor {
        Anchor::new(self, byte_offset)
    }

    #[must_use]
    pub fn resolve_anchor(&self, anchor: &Anchor) -> Option<usize> {
        anchor.resolve(self)
    }

    #[must_use]
    pub fn version(&self) -> usize {
        self.log.len()
    }

    #[must_use]
    pub fn ops_since(&self, version: usize) -> Option<&[OperationSeq]> {
        self.log.get(version..)
    }
}

impl Deref for Text {
    type Target = Rope;

    fn deref(&self) -> &Self::Target {
        &self.rope
    }
}

impl<'a> From<&'a str> for Text {
    fn from(str: &'a str) -> Self {
        Self::from(Rope::from(str))
    }
}

impl From<Rope> for Text {
    fn from(mut rope: Rope) -> Self {
        if rope.len() == 0 || rope.byte(rope.len() - 1) != b'\n' {
            rope.insert(rope.len(), "\n");
        }
        Self {
            rope,
            ..Self::default()
        }
    }
}

pub struct Anchor {
    byte_offset: usize,
    version: usize,
}

impl Anchor {
    #[must_use]
    pub fn new(text: &Text, byte_offset: usize) -> Self {
        Self {
            byte_offset,
            version: text.version(),
        }
    }

    #[must_use]
    pub fn resolve(&self, text: &Text) -> Option<usize> {
        let mut byte_offset = self.byte_offset;
        for ops in text.ops_since(self.version)? {
            byte_offset = ops.transform_byte_offset(byte_offset);
        }
        Some(byte_offset)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cmp::{max, min};

    fn has_trailing_newline(text: &Text) -> bool {
        let rope = text.rope();
        rope.len() > 0 && rope.byte(rope.len() - 1) == b'\n'
    }

    #[test]
    fn constructors_normalize() {
        assert_eq!(Text::new().to_string(), "\n");
        assert_eq!(Text::default().to_string(), "\n");
        assert_eq!(Text::from("").to_string(), "\n");
        assert_eq!(Text::from("x").to_string(), "x\n");
        assert_eq!(Text::from("x\n").to_string(), "x\n");
        assert_eq!(Text::from(Rope::from("x")).to_string(), "x\n");
        // A bare final `'\r'` gains a `'\n'`, forming a single CRLF grapheme.
        assert_eq!(Text::from("x\r").to_string(), "x\r\n");
    }

    #[test]
    fn delete_to_end_keeps_trailing_newline() {
        let mut text = Text::from("ab\n");
        text.delete(1..3).unwrap();
        assert_eq!(text.to_string(), "a\n");

        // Delete everything.
        let mut text = Text::from("ab\n");
        text.delete(0..3).unwrap();
        assert_eq!(text.to_string(), "\n");

        // Deleting a final line whose predecessor is already terminated adds nothing.
        let mut text = Text::from("a\nb\n");
        text.delete(2..4).unwrap();
        assert_eq!(text.to_string(), "a\n");
    }

    #[test]
    fn undo_redo_keep_trailing_newline() {
        let mut text = Text::from("ab\n");
        text.delete(0..3).unwrap();
        text.commit();
        assert_eq!(text.to_string(), "\n");
        assert!(text.undo().unwrap());
        assert_eq!(text.to_string(), "ab\n");
        assert!(has_trailing_newline(&text));
        assert!(text.redo().unwrap());
        assert_eq!(text.to_string(), "\n");
        assert!(has_trailing_newline(&text));
    }

    #[hegel::test(test_cases = 500)]
    fn fuzz(tc: hegel::TestCase) {
        use crate::rope::RopeExt as _;
        use hegel::generators as gs;

        struct StateMachine {
            text: Text,
        }
        #[hegel::state_machine]
        #[expect(clippy::needless_pass_by_value)]
        impl StateMachine {
            #[rule]
            fn insert(&mut self, tc: hegel::TestCase) {
                // Inserting at the very end is only invariant-safe when the inserted text ends
                // with a newline (e.g. `prepare_append`), so draw positions before the final
                // grapheme.
                let byte_offset = tc.draw(gs::integers::<usize>().max_value(self.text.len()));
                let byte_offset = self
                    .text
                    .snap_to_grapheme_start(byte_offset)
                    .expect("Text is never empty");
                let string = tc.draw(gs::text());
                self.text.insert(byte_offset, &string).unwrap();
            }
            #[rule]
            fn delete(&mut self, tc: hegel::TestCase) {
                let a = tc.draw(gs::integers::<usize>().max_value(self.text.len()));
                let b = tc.draw(gs::integers::<usize>().max_value(self.text.len()));
                let start = self.text.floor_grapheme_boundary(min(a, b));
                let end = self.text.floor_grapheme_boundary(max(a, b));
                self.text.delete(start..end).unwrap();
            }
            #[rule]
            fn commit(&mut self, _: hegel::TestCase) {
                self.text.commit();
            }
            #[rule]
            fn undo(&mut self, _: hegel::TestCase) {
                self.text.undo().unwrap();
            }
            #[rule]
            fn redo(&mut self, _: hegel::TestCase) {
                self.text.redo().unwrap();
            }
            #[invariant]
            fn invariants(&self, _: hegel::TestCase) {
                assert!(
                    has_trailing_newline(&self.text),
                    "Text keeps its trailing newline"
                );
            }
        }
        hegel::stateful::run(StateMachine { text: Text::new() }, tc);
    }
}
