#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Conversion<T> {
    Invalid,
    Corrected(T),
    Valid(T),
}
