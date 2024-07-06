use crate::RopeExt as _;
use ropey::{Rope, RopeSlice};

#[derive(Clone, Copy, Default)]
pub struct CursorState {
    // Char gap index
    char_index: usize,
}

#[derive(Clone, Copy)]
pub struct Cursor<'a> {
    pub(crate) rope: &'a Rope,
    pub(crate) state: CursorState,
}

impl<'a> Cursor<'a> {
    #[must_use]
    pub fn new(rope: &'a Rope) -> Self {
        Self {
            rope,
            state: CursorState::default(),
        }
    }

    #[must_use]
    pub fn from_char_index(rope: &'a Rope, char_index: usize) -> Option<Self> {
        // Gap at end of file counts as grapheme boundary
        if let Ok(true) = rope.try_is_grapheme_boundary(char_index) {
            let state = CursorState { char_index };
            Some(Self { rope, state })
        } else {
            None
        }
    }
}

pub struct CursorMut<'a> {
    pub(crate) rope: &'a mut Rope,
    pub(crate) state: CursorState,
}

impl<'a> CursorMut<'a> {
    #[must_use]
    pub fn new(rope: &'a mut Rope) -> Self {
        Self {
            rope,
            state: CursorState::default(),
        }
    }

    #[must_use]
    pub fn from_char_index(rope: &'a mut Rope, char_index: usize) -> Option<Self> {
        // Gap at end of file counts as grapheme boundary
        if let Ok(true) = rope.try_is_grapheme_boundary(char_index) {
            let state = CursorState { char_index };
            Some(Self { rope, state })
        } else {
            None
        }
    }

    #[must_use]
    pub fn downgrade(self) -> Cursor<'a> {
        Cursor {
            rope: self.rope,
            state: self.state,
        }
    }

    #[must_use]
    pub fn downgraded(&self) -> Cursor {
        Cursor {
            rope: self.rope,
            state: self.state,
        }
    }

    pub fn insert_char(&mut self, char: char) {
        self.rope.insert_char(self.state.char_index, char);
        self.state.char_index += 1;
    }

    pub fn insert(&mut self, string: &str) {
        self.rope.insert(self.state.char_index, string);
        self.state.char_index += string.chars().count();
    }

    pub fn backspace(&mut self, count: usize) {
        let mut start_cursor = self.downgraded();
        start_cursor.move_left(count);
        let start = start_cursor.state.char_index;
        let end = self.state.char_index;
        self.rope.remove(start..end);
        self.state.char_index = start;
    }

    pub fn delete(&mut self, count: usize) {
        let start = self.state.char_index;
        let mut end_cursor = self.downgraded();
        end_cursor.move_right(count);
        let end = end_cursor.state.char_index;
        self.rope.remove(start..end);
    }
}

pub trait CursorExt {
    fn cursor_parts(&self) -> (&Rope, &CursorState);

    fn cursor_parts_mut(&mut self) -> (&Rope, &mut CursorState);

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

        char_to_grapheme(rope, state.char_index)
    }

    fn char(&self) -> Option<char> {
        let (rope, state) = self.cursor_parts();

        if state.char_index < rope.len_chars() {
            Some(rope.char(state.char_index))
        } else {
            None
        }
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

impl CursorExt for Cursor<'_> {
    fn cursor_parts(&self) -> (&Rope, &CursorState) {
        (self.rope, &self.state)
    }

    fn cursor_parts_mut(&mut self) -> (&Rope, &mut CursorState) {
        (self.rope, &mut self.state)
    }
}

impl CursorExt for CursorMut<'_> {
    fn cursor_parts(&self) -> (&Rope, &CursorState) {
        (self.rope, &self.state)
    }

    fn cursor_parts_mut(&mut self) -> (&Rope, &mut CursorState) {
        (self.rope, &mut self.state)
    }
}

fn char_to_grapheme(rope: &Rope, char_index: usize) -> usize {
    rope.slice(..char_index).graphemes().count()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn construction() {
        let rope = Rope::default();
        assert!(Cursor::from_char_index(&rope, 0).is_some());
        assert!(Cursor::from_char_index(&rope, 1).is_none());

        let rope = Rope::from("xyz");
        assert!(Cursor::from_char_index(&rope, 3).is_some());
        assert!(Cursor::from_char_index(&rope, 4).is_none());
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
        assert_eq!(cursor.rope.to_string(), "कि मपि   नमस्");
        assert_eq!(cursor.char_index(), 13);
        cursor.backspace(1);
        assert_eq!(cursor.rope.to_string(), "कि मपि   नम");
        assert_eq!(cursor.char_index(), 11);
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
        assert_eq!(cursor.grapheme_index(), 13);
        cursor.move_left(1);
        assert_eq!(cursor.char(), Some('x'));
        assert_eq!(
            cursor.grapheme().map(|slice| slice.to_string()),
            Some(String::from("x"))
        );
        assert_eq!(cursor.char_index(), 17);
        assert_eq!(cursor.grapheme_index(), 12);
        cursor.move_left(1);
        assert_eq!(cursor.char(), Some('🇯'));
        assert_eq!(
            cursor.grapheme().map(|slice| slice.to_string()),
            Some(String::from("🇯🇵"))
        );
        assert_eq!(cursor.char_index(), 15);
        assert_eq!(cursor.grapheme_index(), 11);
    }
}
