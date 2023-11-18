#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Conversion<T> {
    Corrected(T),
    Valid(T),
}

impl<T> Conversion<T> {
    pub fn into_inner(self) -> T {
        match self {
            Self::Corrected(t) | Self::Valid(t) => t,
        }
    }

    pub fn is_corrected(&self) -> bool {
        matches!(self, Self::Corrected(_))
    }

    pub fn is_valid(&self) -> bool {
        matches!(self, Self::Valid(_))
    }

    pub fn map<U, F>(self, op: F) -> Conversion<U>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Self::Corrected(t) => Conversion::Corrected(op(t)),
            Self::Valid(t) => Conversion::Valid(op(t)),
        }
    }
}
