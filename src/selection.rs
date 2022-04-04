use crate::range::Range;
use ropey::Rope;

pub struct Selection {
    pub ranges: Vec<Range>,
    pub primary_range_index: usize,
}

impl Selection {
    pub fn new(ranges: &[Range]) -> Self {
        debug_assert!(!ranges.is_empty());

        Self {
            ranges: ranges.to_vec(),
            primary_range_index: 0,
        }
    }

    pub fn is_overlapping(&self, other: &Self) -> bool {
        for self_range in &self.ranges {
            for other_range in &other.ranges {
                if self_range.is_overlapping(other_range) {
                    return true;
                }
            }
        }

        false
    }

    pub fn next_range(&mut self, count: usize) {
        self.primary_range_index =
            wrap_around(self.ranges.len(), self.primary_range_index, count as isize);
    }

    pub fn previous_range(&mut self, count: usize) {
        self.primary_range_index = wrap_around(
            self.ranges.len(),
            self.primary_range_index,
            -(count as isize),
        );
    }

    pub fn filter_ranges(&mut self, filter: fn(usize, &Range) -> bool) {
        self.ranges = self
            .ranges
            .iter()
            .enumerate()
            .filter(|(i, r)| filter(*i, r))
            .map(|(_, r)| r)
            .cloned()
            .collect();
    }

    pub fn primary_range(&mut self) -> &mut Range {
        &mut self.ranges[self.primary_range_index]
    }

    pub fn in_all_ranges(&mut self, f: fn(&mut Range)) {
        for range in &mut self.ranges {
            f(range);
        }
    }

    #[allow(unused_variables)]
    pub fn union(self, other: Self) -> Self {
        todo!()
    }

    #[allow(unused_variables)]
    pub fn subtract(self, other: Self) -> Self {
        todo!()
    }

    #[allow(unused_variables)]
    pub fn intersect(self, other: Self) -> Self {
        todo!()
    }

    #[allow(unused_variables)]
    pub fn exclude(self, other: Self) -> Self {
        todo!()
    }

    pub fn move_up(&mut self, rope: &Rope, distance: usize) {
        for range in &mut self.ranges {
            range.move_up(rope, distance);
        }
    }

    pub fn move_down(&mut self, rope: &Rope, distance: usize) {
        for range in &mut self.ranges {
            range.move_down(rope, distance);
        }
    }

    pub fn move_left(&mut self, rope: &Rope, distance: usize) {
        for range in &mut self.ranges {
            range.move_left(rope, distance);
        }
    }

    pub fn move_right(&mut self, rope: &Rope, distance: usize) {
        for range in &mut self.ranges {
            range.move_right(rope, distance);
        }
    }
}

impl Default for Selection {
    fn default() -> Self {
        Self {
            ranges: vec![Range::default()],
            primary_range_index: 0,
        }
    }
}

// Modified version of https://stackoverflow.com/a/39740009
fn wrap_around(length: usize, value: usize, delta: isize) -> usize {
    let length = length as isize;
    if length == 0 {
        0
    } else {
        let value = value as isize;
        let result = if delta >= 0 {
            (value + delta) % length
        } else {
            ((value + delta) - (delta * length)) % length
        };
        result as usize
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::cursor::Cursor;

    #[test]
    fn test_cursor_partialord() {
        assert!(Cursor::from((0, 0)) < Cursor::from((1, 0)));
        assert!(Cursor::from((0, 99)) < Cursor::from((1, 0)));
        assert!(Cursor::from((1, 0)) < Cursor::from((1, 1)));
    }

    #[test]
    fn test_range_is_overlapping() {
        {
            let x: Range = ((0, 0), (0, 0)).into();
            let y: Range = ((0, 0), (0, 0)).into();
            assert!(x.is_overlapping(&y))
        }
        {
            let x: Range = ((0, 0), (10, 10)).into();
            let y: Range = ((5, 5), (15, 15)).into();
            assert!(x.is_overlapping(&y))
        }
        {
            let x: Range = ((10, 10), (0, 0)).into();
            let y: Range = ((5, 5), (15, 15)).into();
            assert!(x.is_overlapping(&y))
        }
        {
            let x: Range = ((10, 10), (0, 0)).into();
            let y: Range = ((10, 10), (20, 20)).into();
            assert!(x.is_overlapping(&y))
        }
        {
            let x: Range = ((10, 10), (0, 0)).into();
            let y: Range = ((10, 11), (20, 20)).into();
            assert!(!x.is_overlapping(&y))
        }
    }

    #[test]
    fn test_wrap_around() {
        assert_eq!(wrap_around(0, 0, 0), 0);
        assert_eq!(wrap_around(0, 0, 4), 0);
        assert_eq!(wrap_around(2, 0, 0), 0);
        assert_eq!(wrap_around(2, 0, 3), 1);
        assert_eq!(wrap_around(2, 0, 4), 0);
        assert_eq!(wrap_around(2, 0, -3), 1);
        assert_eq!(wrap_around(2, 0, -4), 0);
    }
}
