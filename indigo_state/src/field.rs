pub trait Field<T> {
    fn get(&self) -> Option<&T>;

    fn get_mut(&mut self) -> Option<&mut T>;
}
