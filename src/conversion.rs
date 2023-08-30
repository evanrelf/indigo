#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Conversion<T> {
    Invalid,
    Corrected(T),
    Valid(T),
}
