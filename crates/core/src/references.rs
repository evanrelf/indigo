use std::ops::Deref;

pub trait Ref {
    type Ref<'a, T: ?Sized + 'a>: Deref<Target = T>;
}

pub struct Immutable;

impl Ref for Immutable {
    type Ref<'a, T: ?Sized + 'a> = &'a T;
}

pub struct Mutable;

impl Ref for Mutable {
    type Ref<'a, T: ?Sized + 'a> = &'a mut T;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_immutable() {
        let xs: [usize; 3] = [1, 2, 3];
        let _: <Immutable as Ref>::Ref<'_, [usize]> = &xs;
    }

    #[test]
    fn test_mutable() {
        let mut xs: [usize; 3] = [1, 2, 3];
        let _: <Mutable as Ref>::Ref<'_, [usize]> = &mut xs;
    }
}
