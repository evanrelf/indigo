pub trait Field<T> {
    fn field(&self) -> Option<&T>;

    fn field_mut(&mut self) -> Option<&mut T>;
}
