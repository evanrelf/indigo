use crate::position::Position;

pub struct Selection {
    anchor: Position,
    cursor: Position,
}

impl From<(usize, usize)> for Selection {
    fn from(tuple: (usize, usize)) -> Selection {
        Selection {
            anchor: Position::from(tuple),
            cursor: Position::from(tuple),
        }
    }
}

impl From<Position> for Selection {
    fn from(position: Position) -> Selection {
        Selection {
            anchor: position.clone(),
            cursor: position.clone(),
        }
    }
}

impl Default for Selection {
    fn default() -> Selection {
        Selection {
            anchor: Position::default(),
            cursor: Position::default(),
        }
    }
}
