use crate::{ot::EditSeq, RopeExt as _};
use ropey::{Rope, RopeSlice};

// I want cursors to be correct by construction; cursor code outside of this module should be safe
// by definition. However currently that is not the case, because ranges are built on cursor
// internals. Specifically, any code in this module that is `pub(crate)` is where I'm compromising
// on this ideal, and should be scrutinized.

#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct CursorState {
    // Char gap index
    pub(crate) char_index: usize,
}

#[derive(Clone, Copy, Debug)]
pub struct Cursor<'a> {
    rope: &'a Rope,
    state: CursorState,
}

impl<'a> Cursor<'a> {
    #[must_use]
    pub fn new(rope: &'a Rope) -> Self {
        Self {
            rope,
            state: CursorState::default(),
        }
    }

    // `Ok` means the char index was valid in the rope. `Err` means the char index was not valid in
    // the rope, but we we're able to correct it (i.e. snap to the end of the rope).
    pub fn from_char_index(rope: &'a Rope, char_index: usize) -> Result<Self, Self> {
        let state = CursorState { char_index };
        Self::from_state(rope, state)
    }

    pub(crate) fn from_state(rope: &'a Rope, state: CursorState) -> Result<Self, Self> {
        // Gap at end of file counts as grapheme boundary
        if let Ok(true) = rope.try_is_grapheme_boundary(state.char_index) {
            Ok(Self { rope, state })
        } else {
            let state = CursorState {
                char_index: rope.len_chars(),
            };
            Err(Self { rope, state })
        }
    }

    pub(crate) fn into_state(self) -> CursorState {
        self.state
    }
}

#[derive(Debug)]
pub struct CursorMut<'a> {
    rope: &'a mut Rope,
    state: CursorState,
}

impl<'a> CursorMut<'a> {
    #[must_use]
    pub fn new(rope: &'a mut Rope) -> Self {
        Self {
            rope,
            state: CursorState::default(),
        }
    }

    pub fn from_char_index(rope: &'a mut Rope, char_index: usize) -> Result<Self, Self> {
        let state = CursorState { char_index };
        Self::from_state(rope, state)
    }

    pub(crate) fn from_state(rope: &'a mut Rope, state: CursorState) -> Result<Self, Self> {
        // Gap at end of file counts as grapheme boundary
        if let Ok(true) = rope.try_is_grapheme_boundary(state.char_index) {
            Ok(Self { rope, state })
        } else {
            let state = CursorState {
                char_index: rope.len_chars(),
            };
            Err(Self { rope, state })
        }
    }

    pub(crate) fn into_state(self) -> CursorState {
        self.state
    }

    pub fn insert_char(&mut self, char: char) {
        self.insert(&char.to_string());
    }

    pub fn insert(&mut self, string: &str) {
        let mut edits = EditSeq::new();
        edits.retain(self.state.char_index);
        edits.insert(string);
        edits.retain(self.rope.len_chars() - self.state.char_index);
        edits.apply(self.rope).unwrap();
        self.state.char_index = edits.transform_index(self.state.char_index);
    }

    pub fn backspace(&mut self, count: usize) {
        let mut start_cursor = Cursor {
            rope: self.rope,
            state: self.state,
        };
        start_cursor.move_left(count);
        let start = start_cursor.state.char_index;
        let end = self.state.char_index;
        self.rope.remove(start..end);
        self.state.char_index = start;
    }

    pub fn delete(&mut self, count: usize) {
        let start = self.state.char_index;
        let mut end_cursor = Cursor {
            rope: self.rope,
            state: self.state,
        };
        end_cursor.move_right(count);
        let end = end_cursor.state.char_index;
        self.rope.remove(start..end);
    }
}

trait CursorParts {
    fn cursor_parts(&self) -> (&Rope, &CursorState);

    fn cursor_parts_mut(&mut self) -> (&Rope, &mut CursorState);
}

impl CursorParts for Cursor<'_> {
    fn cursor_parts(&self) -> (&Rope, &CursorState) {
        (self.rope, &self.state)
    }

    fn cursor_parts_mut(&mut self) -> (&Rope, &mut CursorState) {
        (self.rope, &mut self.state)
    }
}

impl CursorParts for CursorMut<'_> {
    fn cursor_parts(&self) -> (&Rope, &CursorState) {
        (self.rope, &self.state)
    }

    fn cursor_parts_mut(&mut self) -> (&Rope, &mut CursorState) {
        (self.rope, &mut self.state)
    }
}

#[allow(private_bounds)]
pub trait CursorExt: CursorParts {
    #[must_use]
    fn rope(&self) -> &Rope {
        let (rope, _state) = self.cursor_parts();

        rope
    }

    /// Char gap index
    #[must_use]
    fn char_index(&self) -> usize {
        let (_rope, state) = self.cursor_parts();

        state.char_index
    }

    /// Grapheme gap index
    #[must_use]
    fn grapheme_index(&self) -> usize {
        let (rope, state) = self.cursor_parts();

        rope.char_to_grapheme(state.char_index)
    }

    fn char(&self) -> Option<char> {
        let (rope, state) = self.cursor_parts();

        rope.get_char(state.char_index)
    }

    fn grapheme(&self) -> Option<RopeSlice> {
        let (rope, state) = self.cursor_parts();

        if state.char_index < rope.len_chars() {
            let start = state.char_index;
            let end = rope.next_grapheme_boundary(start);
            Some(rope.slice(start..end))
        } else {
            None
        }
    }

    fn move_left(&mut self, distance: usize) -> bool {
        let (rope, state) = self.cursor_parts_mut();

        let mut moved = false;

        for _ in 1..=distance {
            match rope.get_prev_grapheme_boundary(state.char_index) {
                Ok(char_index) if state.char_index != char_index => {
                    state.char_index = char_index;
                    moved = true;
                }
                _ => break,
            }
        }

        moved
    }

    fn move_right(&mut self, distance: usize) -> bool {
        let (rope, state) = self.cursor_parts_mut();

        let mut moved = false;

        for _ in 1..=distance {
            match rope.get_next_grapheme_boundary(state.char_index) {
                Ok(char_index) if state.char_index != char_index => {
                    state.char_index = char_index;
                    moved = true;
                }
                _ => break,
            }
        }

        moved
    }
}

impl<T> CursorExt for T where T: CursorParts {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn construction() {
        let rope = Rope::default();
        assert!(Cursor::from_char_index(&rope, 0).is_ok());
        assert!(Cursor::from_char_index(&rope, 1).is_err());

        let rope = Rope::from("xyz");
        assert!(Cursor::from_char_index(&rope, 3).is_ok());
        assert!(Cursor::from_char_index(&rope, 4).is_err());
    }

    #[test]
    fn movement() {
        let rope = Rope::from("å𝒷cḋ🇯🇵");
        assert_eq!(rope.chars().count(), 6);
        let mut cursor = Cursor::new(&rope);

        assert_eq!(cursor.char_index(), 0);

        cursor.move_right(1);
        assert_eq!(cursor.char_index(), "å".chars().count());

        cursor.move_right(1);
        assert_eq!(cursor.char_index(), "å𝒷".chars().count());

        cursor.move_right(3);
        assert_eq!(cursor.char_index(), "å𝒷cḋ🇯🇵".chars().count());

        let moved = cursor.move_right(99);
        assert_eq!(cursor.char_index(), "å𝒷cḋ🇯🇵".chars().count());
        assert!(!moved);

        let moved = cursor.move_left(99);
        assert_eq!(cursor.char_index(), 0);
        assert!(moved);
    }

    #[test]
    fn insertion() {
        let mut rope = Rope::new();
        let mut cursor = CursorMut::new(&mut rope);
        assert_eq!(cursor.rope.chars().count(), 0);
        assert_eq!(cursor.rope.chars().count(), cursor.char_index());

        cursor.insert_char('x');
        assert_eq!(cursor.rope.chars().count(), 1);
        assert_eq!(cursor.rope.chars().count(), cursor.char_index());

        cursor.insert("yz");
        assert_eq!(cursor.rope.chars().count(), 3);
        assert_eq!(cursor.rope.chars().count(), cursor.char_index());

        cursor.move_left(99);

        cursor.insert("");
        assert_eq!(cursor.rope.chars().count(), 3);
        assert_eq!(cursor.char_index(), 0);

        cursor.insert("🇯🇵");
        assert_eq!(cursor.rope.chars().count(), 5);
        assert_eq!(cursor.char_index(), 2);

        assert_eq!(cursor.rope.to_string(), "🇯🇵xyz");
    }

    #[test]
    fn deletion() {
        let mut rope = Rope::new();
        let mut cursor = CursorMut::new(&mut rope);
        cursor.backspace(99);
        cursor.delete(99);

        cursor.insert("Hello, world!");
        assert_eq!(cursor.char_index(), 13);

        cursor.backspace(1);
        assert_eq!(cursor.rope.to_string(), "Hello, world");
        assert_eq!(cursor.char_index(), 12);

        cursor.move_left(7);
        cursor.delete(99);
        assert_eq!(cursor.rope.to_string(), "Hello");
        assert_eq!(cursor.char_index(), 5);

        cursor.backspace(99);
        assert_eq!(cursor.rope.to_string(), "");
        assert_eq!(cursor.char_index(), 0);

        cursor.insert("कि मपि   नमस्ते");
        assert_eq!(cursor.char_index(), 15);
        cursor.backspace(1);
        assert_eq!(cursor.rope.to_string(), "कि मपि   ");
        assert_eq!(cursor.char_index(), 9);
        cursor.backspace(1);
        assert_eq!(cursor.rope.to_string(), "कि मपि  ");
        assert_eq!(cursor.char_index(), 8);
    }

    #[test]
    fn querying() {
        let rope = Rope::from("कि मपि   नमस्ते🇯🇵x");
        let mut cursor = Cursor::new(&rope);
        assert_eq!(cursor.char(), Some('क'));
        assert_eq!(
            cursor.grapheme().map(|slice| slice.to_string()),
            Some(String::from("कि"))
        );
        assert_eq!(cursor.char_index(), 0);
        assert_eq!(cursor.grapheme_index(), 0);
        cursor.move_right(99);
        assert_eq!(cursor.char(), None);
        assert_eq!(cursor.grapheme(), None);
        assert_eq!(cursor.char_index(), 18);
        assert_eq!(cursor.grapheme_index(), 12);
        cursor.move_left(1);
        assert_eq!(cursor.char(), Some('x'));
        assert_eq!(
            cursor.grapheme().map(|slice| slice.to_string()),
            Some(String::from("x"))
        );
        assert_eq!(cursor.char_index(), 17);
        assert_eq!(cursor.grapheme_index(), 11);
        cursor.move_left(1);
        assert_eq!(cursor.char(), Some('🇯'));
        assert_eq!(
            cursor.grapheme().map(|slice| slice.to_string()),
            Some(String::from("🇯🇵"))
        );
        assert_eq!(cursor.char_index(), 15);
        assert_eq!(cursor.grapheme_index(), 10);
    }
}
