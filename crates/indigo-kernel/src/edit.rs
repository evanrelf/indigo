use ropey::Rope;
use std::{borrow::Cow, rc::Rc};

enum Operation {
    Retain(usize),
    Delete(Rc<str>),
    Insert(Rc<str>),
}

#[derive(Default)]
pub struct Edits {
    ops: Vec<Operation>,
    len_before: usize,
    len_after: usize,
}

impl Edits {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn retain(&mut self, byte_length: usize) {
        if byte_length == 0 {
            return;
        }

        self.len_before += byte_length;
        self.len_after += byte_length;

        if let Some(Operation::Retain(n)) = self.ops.last_mut() {
            *n += byte_length;
        } else {
            self.ops.push(Operation::Retain(byte_length));
        }
    }

    pub fn retain_rest(&mut self, rope: &Rope) -> anyhow::Result<()> {
        anyhow::ensure!(
            self.len_before <= rope.len(),
            "edit input length {} <= rope length {}",
            self.len_before,
            rope.len()
        );

        self.retain(rope.len() - self.len_before);

        Ok(())
    }

    pub fn delete(&mut self, text: &str) {
        if text.is_empty() {
            return;
        }

        self.len_before += text.len();

        if let Some(Operation::Delete(last)) = self.ops.last_mut() {
            let mut s = String::with_capacity(last.len() + text.len());
            s.push_str(last);
            s.push_str(text);
            *last = Rc::from(s);
        } else {
            self.ops.push(Operation::Delete(Rc::from(text)));
        }
    }

    pub fn insert(&mut self, text: &str) {
        if text.is_empty() {
            return;
        }

        self.len_after += text.len();

        if let Some(Operation::Insert(last)) = self.ops.last_mut() {
            let mut s = String::with_capacity(last.len() + text.len());
            s.push_str(last);
            s.push_str(text);
            *last = Rc::from(s);
        } else {
            self.ops.push(Operation::Insert(Rc::from(text)));
        }
    }

    #[must_use]
    pub fn compose(&self, _other: &Self) -> Self {
        todo!()
    }

    #[must_use]
    pub fn invert(&self) -> Self {
        let mut inverted = Self::new();

        for op in &self.ops {
            match op {
                Operation::Retain(n) => inverted.retain(*n),
                Operation::Delete(s) => inverted.insert(s),
                Operation::Insert(s) => inverted.delete(s),
            }
        }

        inverted
    }

    #[must_use]
    pub fn rebase(&self, _over: &Self, _bias: Bias) -> Self {
        // Requires OT TP1 but not TP2
        // <https://en.wikipedia.org/wiki/Operational_transformation#Transformation_properties>
        todo!()
    }

    // TODO: Convert panics to errors
    pub fn apply(&self, rope: &mut Rope) -> anyhow::Result<()> {
        anyhow::ensure!(
            self.len_before == rope.len(),
            "edit input length {} != rope length {}",
            self.len_before,
            rope.len()
        );

        let mut byte_index = 0;

        for op in &self.ops {
            match op {
                Operation::Retain(n) => {
                    byte_index += n;
                }
                Operation::Delete(s) => {
                    let range = byte_index..byte_index + s.len();
                    let slice = Cow::<str>::from(rope.slice(range.clone()));
                    anyhow::ensure!(slice == s.as_ref(), "deleted text `{slice}` not in rope");
                    rope.try_remove(range)?;
                }
                Operation::Insert(s) => {
                    rope.try_insert(byte_index, s)?;
                    byte_index += s.len();
                }
            }
        }

        anyhow::ensure!(
            self.len_after == rope.len() && byte_index == rope.len(),
            "edit output length {} != rope length {}",
            self.len_after,
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

        let mut rope = Rope::from("Hello, world!");
        edits.retain(1);
        assert!(edits.apply(&mut rope).is_err());

        Ok(())
    }
}
