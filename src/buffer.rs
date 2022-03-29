use crate::{cursor::Cursor, rope::Rope, selection::Selection};
use std::{
    fs::File,
    io::BufReader,
    path::{Path, PathBuf},
    str::FromStr,
};

#[derive(Default)]
pub struct Buffer {
    path: Option<PathBuf>,
    rope: Rope,
    pub selection: Selection,
    view_lines_offset: usize,
    view_columns_offset: usize,
}

impl Buffer {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn open<P>(path: P) -> Self
    where
        P: AsRef<Path>,
    {
        let path_buf = path.as_ref().to_path_buf();
        let file = File::open(path).unwrap();
        let reader = BufReader::new(file);

        Self {
            path: Some(path_buf),
            rope: ropey::Rope::from_reader(reader).unwrap().into(),
            selection: Selection::default(),
            view_lines_offset: 0,
            view_columns_offset: 0,
        }
    }

    pub fn path(&self) -> &Option<PathBuf> {
        &self.path
    }

    pub fn rope(&self) -> &Rope {
        &self.rope
    }

    pub fn selection(&self) -> &Selection {
        &self.selection
    }

    pub fn view_lines_offset(&self) -> usize {
        self.view_lines_offset
    }

    pub fn view_columns_offset(&self) -> usize {
        self.view_columns_offset
    }

    pub fn scroll_up(&mut self, distance: usize) {
        self.view_lines_offset = self.view_lines_offset.saturating_sub(distance);
    }

    pub fn scroll_down(&mut self, distance: usize) {
        let new_view_lines_offset = self.view_lines_offset + distance;
        if new_view_lines_offset <= self.rope.len_lines() {
            self.view_lines_offset = new_view_lines_offset;
        }
    }

    pub fn scroll_left(&mut self, distance: usize) {
        self.view_columns_offset = self.view_columns_offset.saturating_sub(distance);
    }

    pub fn scroll_right(&mut self, distance: usize) {
        self.view_columns_offset += distance;
    }

    pub fn move_up(&mut self, distance: usize) {
        for range in &mut self.selection.ranges {
            let head = self.rope.corrected_cursor(&Cursor {
                line: range.head.line.saturating_sub(distance),
                ..range.head
            });
            match head {
                Some(head) if self.rope.is_valid_cursor(&head) => range.head = head,
                _ => {}
            }
        }
    }

    pub fn move_down(&mut self, distance: usize) {
        for range in &mut self.selection.ranges {
            let head = self.rope.corrected_cursor(&Cursor {
                line: range.head.line + distance,
                ..range.head
            });
            match head {
                Some(head) if self.rope.is_valid_cursor(&head) => range.head = head,
                _ => {}
            }
        }
    }

    pub fn move_left(&mut self, distance: usize) {
        for range in &mut self.selection.ranges {
            let old_index = self.rope.cursor_to_index(&range.head).unwrap();
            let new_index = old_index.saturating_sub(distance);
            range.head = self.rope.index_to_cursor(new_index).unwrap();
        }
    }

    pub fn move_right(&mut self, distance: usize) {
        for range in &mut self.selection.ranges {
            let old_index = self.rope.cursor_to_index(&range.head).unwrap();
            let new_index = old_index + distance;
            if new_index < self.rope.len_chars() {
                range.head = self.rope.index_to_cursor(new_index).unwrap();
            } else {
                range.head = self
                    .rope
                    .index_to_cursor(self.rope.len_chars() - 1)
                    .unwrap();
            }
        }
    }

    pub fn insert(&mut self, c: char) {
        for range in &mut self.selection.ranges {
            // Insert character
            let anchor_index = self.rope.cursor_to_index(&range.anchor).unwrap();
            let head_index = self.rope.cursor_to_index(&range.head).unwrap();

            if range.is_forwards() {
                self.rope.insert_char(anchor_index, c);
            } else {
                self.rope.insert_char(head_index, c);
            }

            // Shift selection
            range.anchor = self.rope.index_to_cursor(anchor_index + 1).unwrap();
            range.head = self.rope.index_to_cursor(head_index + 1).unwrap();
        }
    }

    pub fn backspace(&mut self) {
        for range in &mut self.selection.ranges {
            let anchor_index = self.rope.cursor_to_index(&range.head).unwrap();
            let head_index = self.rope.cursor_to_index(&range.head).unwrap();
            if head_index > 0 {
                // Delete character
                self.rope.remove(head_index - 1..head_index);

                // Shift selection
                range.anchor = self.rope.index_to_cursor(anchor_index - 1).unwrap();
                range.head = self.rope.index_to_cursor(head_index - 1).unwrap();
            }
        }
    }

    pub fn delete(&mut self) {
        for range in &mut self.selection.ranges {
            range.flip_backwards();
            let anchor_index = self.rope.cursor_to_index(&range.anchor).unwrap();
            let head_index = self.rope.cursor_to_index(&range.head).unwrap();
            self.rope.remove(head_index..=anchor_index);
            range.reduce();
            if let Some(s) = self.rope.corrected_range(range) {
                *range = s;
            };
        }
    }
}

impl FromStr for Buffer {
    type Err = std::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self {
            path: None,
            rope: ropey::Rope::from_str(s).into(),
            selection: Selection::default(),
            view_lines_offset: 0,
            view_columns_offset: 0,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_index_cursor() {
        fn case(s: &str, tuple: (usize, usize), expected: char) {
            let buffer = Buffer::from_str(s).unwrap();
            let cursor = Cursor::from(tuple);
            let index = buffer.rope.cursor_to_index(&cursor).unwrap();
            let actual = buffer.rope.char(index);
            assert!(
                expected == actual,
                "\nexpected = {:?}\nactual = {:?}\n",
                expected,
                actual
            );
        }

        case("abc\nxyz\n", (0, 0), 'a');
        case("abc\nxyz\n", (0, 3), '\n');
        case("abc\nxyz\n", (1, 0), 'x');
        case("abc\nxyz\n", (1, 1), 'y');
    }

    #[test]
    fn test_index_cursor_roundtrip() {
        enum CaseResult {
            Pass,
            Fail,
        }
        use CaseResult::*;

        fn case(result: CaseResult, s: &str, tuple: (usize, usize)) {
            let buffer = Buffer::from_str(s).unwrap();
            let cursor = Cursor::from(tuple);
            let actual = buffer
                .rope
                .cursor_to_index(&cursor)
                .and_then(|i| buffer.rope.index_to_cursor(i));
            let expected = Some(cursor);
            assert!(
                match result {
                    Pass => expected == actual,
                    Fail => None == actual,
                },
                "\nexpected = {:?}\nactual = {:?}\n",
                expected,
                actual
            );
        }

        case(Pass, "abc\nxyz\n", (0, 0));
        case(Pass, "abc\nxyz\n", (1, 0));
        case(Pass, "abc\nxyz\n", (1, 1));
        case(Fail, "abc\nxyz\n", (0, 4));
        case(Fail, "abc\nxyz\n", (0, 20));
        case(Fail, "abc\nxyz\n", (2, 2));
    }

    #[test]
    fn test_corrected_cursor() {
        fn case(
            s: &str,
            original: (usize, usize, Option<usize>),
            corrected: (usize, usize, Option<usize>),
        ) {
            let buffer = Buffer::from_str(s).unwrap();
            let expected = Some(Cursor {
                line: corrected.0,
                column: corrected.1,
                target_column: corrected.2,
            });
            let actual = buffer.rope.corrected_cursor(&Cursor {
                line: original.0,
                column: original.1,
                target_column: original.2,
            });
            assert!(
                expected == actual,
                "\nexpected = {:?}\nactual = {:?}\n",
                expected,
                actual
            );
        }

        case("abc\nx\n", (0, 0, None), (0, 0, None));
        case("abc\nx\n", (0, 99, None), (0, 3, Some(99)));
        case("abc\nx\n", (1, 0, Some(1)), (1, 1, None));
        case("abc\nx\n", (1, 99, None), (1, 1, Some(99)));
    }
}
