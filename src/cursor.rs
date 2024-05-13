use crate::RopeExt as _;
use ropey::Rope;

pub struct Cursor<'a> {
    rope: &'a Rope,
    // Char gap index
    char_index: usize,
}

impl<'a> Cursor<'a> {
    #[must_use]
    pub fn new(rope: &'a Rope) -> Self {
        Self {
            rope,
            char_index: 0,
        }
    }

    #[must_use]
    pub fn from_char_index(rope: &'a Rope, char_index: usize) -> Option<Self> {
        // Gap at end of file counts as grapheme boundary
        if let Ok(true) = rope.try_is_grapheme_boundary(char_index) {
            Some(Self { rope, char_index })
        } else {
            None
        }
    }

    /// Char gap index
    #[must_use]
    pub fn char_index(&self) -> usize {
        self.char_index
    }

    pub fn move_left(&mut self, grapheme_distance: usize) -> bool {
        let mut moved = false;

        for _ in 1..=grapheme_distance {
            match self.rope.get_prev_grapheme_boundary(self.char_index) {
                Ok(char_index) if self.char_index != char_index => {
                    self.char_index = char_index;
                    moved = true;
                }
                _ => break,
            }
        }

        moved
    }

    pub fn move_right(&mut self, grapheme_distance: usize) -> bool {
        let mut moved = false;

        for _ in 1..=grapheme_distance {
            match self.rope.get_next_grapheme_boundary(self.char_index) {
                Ok(char_index) if self.char_index != char_index => {
                    self.char_index = char_index;
                    moved = true;
                }
                _ => break,
            }
        }

        moved
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
