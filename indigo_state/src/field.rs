pub trait Field<T> {
    type Error;

    fn field(&self) -> Result<&T, Self::Error>;

    fn field_mut(&mut self) -> Result<&mut T, Self::Error>;
}
