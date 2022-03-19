pub(crate) trait Operand {
    type Operation;
    fn apply(&mut self, operation: Self::Operation);
}
