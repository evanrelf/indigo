use crate::{movement, position::Position, selection::Selection};
use ropey::Rope;
use std::{
    cmp::min,
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
            rope: Rope::from_reader(reader).unwrap(),
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
        self.view_lines_offset = min(
            // Subtracting 1 to convert to zero-based index, subtracting another 1 to account for
            // ropey's mysterious empty final line
            self.rope.len_lines().saturating_sub(2),
            self.view_lines_offset + distance,
        );
    }

    pub fn scroll_left(&mut self, distance: usize) {
        self.view_columns_offset = self.view_columns_offset.saturating_sub(distance);
    }

    pub fn scroll_right(&mut self, distance: usize) {
        self.view_columns_offset += distance;
    }

    pub fn extend_up(&mut self, distance: usize) {
        for range in &mut self.selection.ranges {
            *range = movement::extend_up(&self.rope, range, distance);
        }
    }

    pub fn extend_down(&mut self, distance: usize) {
        for range in &mut self.selection.ranges {
            *range = movement::extend_down(&self.rope, range, distance);
        }
    }

    pub fn extend_left(&mut self, distance: usize) {
        for range in &mut self.selection.ranges {
            *range = movement::extend_left(&self.rope, range, distance);
        }
    }

    pub fn extend_right(&mut self, distance: usize) {
        for range in &mut self.selection.ranges {
            *range = movement::extend_right(&self.rope, range, distance);
        }
    }

    pub fn move_up(&mut self, distance: usize) {
        for range in &mut self.selection.ranges {
            *range = movement::move_up(&self.rope, range, distance);
        }
    }

    pub fn move_down(&mut self, distance: usize) {
        for range in &mut self.selection.ranges {
            *range = movement::move_down(&self.rope, range, distance);
        }
    }

    pub fn move_left(&mut self, distance: usize) {
        for range in &mut self.selection.ranges {
            *range = movement::move_left(&self.rope, range, distance);
        }
    }

    pub fn move_right(&mut self, distance: usize) {
        for range in &mut self.selection.ranges {
            *range = movement::move_right(&self.rope, range, distance);
        }
    }

    pub fn extend_buffer_top(&mut self) {
        for range in &mut self.selection.ranges {
            *range = movement::extend_buffer_top(range);
        }
    }

    pub fn extend_buffer_bottom(&mut self) {
        for range in &mut self.selection.ranges {
            *range = movement::extend_buffer_bottom(&self.rope, range);
        }
    }

    pub fn extend_buffer_end(&mut self) {
        for range in &mut self.selection.ranges {
            *range = movement::extend_buffer_end(&self.rope, range);
        }
    }

    pub fn move_buffer_top(&mut self) {
        for range in &mut self.selection.ranges {
            *range = movement::move_buffer_top(range);
        }
    }

    pub fn move_buffer_bottom(&mut self) {
        for range in &mut self.selection.ranges {
            *range = movement::move_buffer_bottom(&self.rope, range);
        }
    }

    pub fn move_buffer_end(&mut self) {
        for range in &mut self.selection.ranges {
            *range = movement::move_buffer_end(&self.rope, range);
        }
    }

    pub fn extend_line_begin(&mut self) {
        for range in &mut self.selection.ranges {
            *range = movement::extend_line_begin(range);
        }
    }

    pub fn extend_line_first_non_blank(&mut self) {
        for range in &mut self.selection.ranges {
            *range = movement::extend_line_first_non_blank(&self.rope, range);
        }
    }

    pub fn extend_line_end(&mut self) {
        for range in &mut self.selection.ranges {
            *range = movement::extend_line_end(&self.rope, range);
        }
    }

    pub fn move_line_begin(&mut self) {
        for range in &mut self.selection.ranges {
            *range = movement::move_line_begin(range);
        }
    }

    pub fn move_line_first_non_blank(&mut self) {
        for range in &mut self.selection.ranges {
            *range = movement::move_line_first_non_blank(&self.rope, range);
        }
    }

    pub fn move_line_end(&mut self) {
        for range in &mut self.selection.ranges {
            *range = movement::move_line_end(&self.rope, range);
        }
    }

    pub fn insert(&mut self, c: char) {
        for range in &mut self.selection.ranges {
            // Insert character
            let anchor_index = range.anchor.to_rope_index(&self.rope).unwrap();
            let head_index = range.head.to_rope_index(&self.rope).unwrap();

            if range.is_forwards() {
                self.rope.insert_char(anchor_index, c);
            } else {
                self.rope.insert_char(head_index, c);
            }

            // Shift selection
            range.anchor = Position::from_rope_index(&self.rope, anchor_index + 1).unwrap();
            range.head = Position::from_rope_index(&self.rope, head_index + 1).unwrap();
        }
    }

    pub fn backspace(&mut self) {
        for range in &mut self.selection.ranges {
            let anchor_index = range.anchor.to_rope_index(&self.rope).unwrap();
            let head_index = range.head.to_rope_index(&self.rope).unwrap();
            if head_index > 0 {
                // Delete character
                self.rope.remove(head_index - 1..head_index);

                // Shift selection
                range.anchor = Position::from_rope_index(&self.rope, anchor_index - 1).unwrap();
                range.head = Position::from_rope_index(&self.rope, head_index - 1).unwrap();
            }
        }
    }

    pub fn delete(&mut self) {
        for range in &mut self.selection.ranges {
            range.flip_backwards();
            let anchor_index = range.anchor.to_rope_index(&self.rope).unwrap();
            let head_index = range.head.to_rope_index(&self.rope).unwrap();
            self.rope.remove(head_index..=anchor_index);
            range.reduce();
            *range = range.corrected(&self.rope).0;
        }
    }
}

impl FromStr for Buffer {
    type Err = std::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self {
            path: None,
            rope: Rope::from_str(s),
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
    fn test_index_position() {
        fn case(s: &str, tuple: (usize, usize), expected: char) {
            let buffer = Buffer::from_str(s).unwrap();
            let position = Position::from(tuple);
            let index = position.to_rope_index(&buffer.rope).unwrap();
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
    fn test_index_position_roundtrip() {
        enum CaseResult {
            Pass,
            Fail,
        }
        use CaseResult::*;

        fn case(result: CaseResult, s: &str, tuple: (usize, usize)) {
            let buffer = Buffer::from_str(s).unwrap();
            let position = Position::from(tuple);
            let actual = position
                .to_rope_index(&buffer.rope)
                .and_then(|i| Position::from_rope_index(&buffer.rope, i));
            let expected = Some(position);
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
}
