use crate::RopeExt as _;
use ropey::Rope;

#[derive(Default)]
pub struct CursorState {
    // Char gap index
    char_index: usize,
}

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

    fn move_left(&mut self, grapheme_distance: usize) -> bool {
        let (rope, state) = self.cursor_parts_mut();

        let mut moved = false;

        for _ in 1..=grapheme_distance {
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

    fn move_right(&mut self, grapheme_distance: usize) -> bool {
        let (rope, state) = self.cursor_parts_mut();

        let mut moved = false;

        for _ in 1..=grapheme_distance {
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

impl<'a> CursorExt for Cursor<'a> {
    fn cursor_parts(&self) -> (&'a Rope, &CursorState) {
        (self.rope, &self.state)
    }

    fn cursor_parts_mut(&mut self) -> (&'a Rope, &mut CursorState) {
        (self.rope, &mut self.state)
    }
}

impl<'a> CursorExt for CursorMut<'a> {
    fn cursor_parts(&self) -> (&Rope, &CursorState) {
        (self.rope, &self.state)
    }

    fn cursor_parts_mut(&mut self) -> (&Rope, &mut CursorState) {
        (self.rope, &mut self.state)
    }
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
}
