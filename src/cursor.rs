use ropey::Rope;
use std::fmt::Display;

#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
pub struct Cursor {
    pub line: usize,
    pub column: usize,
    pub target_column: Option<usize>,
}

impl Cursor {
    pub fn new(line: usize, column: usize) -> Self {
        Self {
            line,
            column,
            target_column: None,
        }
    }

    pub fn to_rope_index(&self, rope: &Rope) -> Option<usize> {
        let bol_char_index = rope.try_line_to_char(self.line).ok()?;
        let line_length = rope.get_line(self.line)?.len_chars();
        if line_length < self.column {
            return None;
        }
        Some(bol_char_index + self.column)
    }

    #[allow(unused_variables)]
    pub fn to_rope_index_lossy(&self, rope: &Rope) -> usize {
        todo!()
    }

    pub fn from_rope_index(rope: &Rope, index: usize) -> Option<Self> {
        let line = rope.try_char_to_line(index).ok()?;
        let column = index - rope.try_line_to_char(line).ok()?;
        Some(Self {
            line,
            column,
            target_column: None,
        })
    }

    #[allow(unused_variables)]
    pub fn from_rope_index_lossy(rope: &Rope, index: usize) -> Self {
        todo!()
    }
}

impl Display for Cursor {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "{}:{}", self.line + 1, self.column + 1)
    }
}

impl From<(usize, usize)> for Cursor {
    fn from((line, column): (usize, usize)) -> Self {
        Self {
            line,
            column,
            target_column: None,
        }
    }
}

// TODO: Delete these functions after replacing their uses with equivalent `*_lossy` functions

pub fn is_valid_cursor(rope: &Rope, cursor: &Cursor) -> bool {
    cursor.to_rope_index(rope).is_some()
}

pub fn corrected_cursor(rope: &Rope, cursor: &Cursor) -> Option<Cursor> {
    let line_length = rope.get_line(cursor.line)?.len_chars();
    if line_length == 0 {
        return None;
    }

    #[allow(clippy::collapsible_else_if)]
    if let Some(target_column) = cursor.target_column {
        if line_length > target_column {
            Some(Cursor {
                column: target_column,
                target_column: None,
                ..cursor.clone()
            })
        } else {
            Some(Cursor {
                column: line_length - 1,
                ..cursor.clone()
            })
        }
    } else {
        if line_length > cursor.column {
            Some(cursor.clone())
        } else {
            Some(Cursor {
                column: line_length - 1,
                target_column: Some(cursor.column),
                ..cursor.clone()
            })
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_corrected_cursor() {
        fn case(
            s: &str,
            original: (usize, usize, Option<usize>),
            corrected: (usize, usize, Option<usize>),
        ) {
            let rope = Rope::from_str(s);
            let expected = Some(Cursor {
                position: Position {
                    line: corrected.0,
                    column: corrected.1,
                },
                target_column: corrected.2,
            });
            let actual = corrected_cursor(
                &rope,
                &Cursor {
                    position: Position {
                        line: original.0,
                        column: original.1,
                    },
                    target_column: original.2,
                },
            );
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
