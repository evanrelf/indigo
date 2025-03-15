use std::{marker::PhantomData, ops::Deref};

/// Generic type wrappers. A limited form of higher-kinded types in Rust.
///
/// This is a generalized version of `WrapRef` that doesn't require `Deref`, so you can use wrappers
/// that may not have the type they're wrapping (e.g. `PhantomData`, `Option`, etc).
///
/// If your wrapper type implements `Deref`, you should implement the `WrapRef` trait instead, and
/// you'll get a `Wrap` impl for free.
pub trait Wrap {
    type Wrap<'a, T: ?Sized + 'a>;
}

pub struct Phantom;

impl Wrap for Phantom {
    type Wrap<'a, T: ?Sized + 'a> = PhantomData<T>;
}

/// Anything implementing `WrapRef` trivially implements `Wrap`.
impl<R: WrapRef> Wrap for R {
    type Wrap<'a, T: ?Sized + 'a> = R::WrapRef<'a, T>;
}

/// Generic reference type wrappers. A limited form of higher-kinded types in Rust.
///
/// Specialized version of `Wrap` that requires `Deref`, so you can access the underlying `T` even
/// if the wrapper type is polymorphic.
///
/// If your wrapper type doesn't implement `Deref`, you should implement the `Wrap` trait instead.
pub trait WrapRef {
    type WrapRef<'a, T: ?Sized + 'a>: Deref<Target = T>;
}

pub struct Immutable;

impl WrapRef for Immutable {
    type WrapRef<'a, T: ?Sized + 'a> = &'a T;
}

pub struct Mutable;

impl WrapRef for Mutable {
    type WrapRef<'a, T: ?Sized + 'a> = &'a mut T;
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::convert::Infallible;

    struct TestDeref<T> {
        value: T,
    }

    #[test]
    fn test_wrap_phantom() {
        let _: <Phantom as Wrap>::Wrap<'_, Infallible> = PhantomData;
    }

    fn test_wrap_immutable() {
        let xs: [usize; 3] = [1, 2, 3];
        let _: <Immutable as Wrap>::Wrap<'_, [usize]> = &xs;
    }

    fn test_wrap_mutable() {
        let mut xs: [usize; 3] = [1, 2, 3];
        let _: <Mutable as Wrap>::Wrap<'_, [usize]> = &mut xs;
    }

    #[test]
    fn test_wrap_deref() {
        let mut x: TestDeref<Vec<usize>> = TestDeref { value: vec![42] };
        let r: <Mutable as Wrap>::Wrap<'_, TestDeref<Vec<usize>>> = &mut x;
        assert_eq!(r.value[0], 42);
    }

    #[test]
    fn test_ref_immutable() {
        let xs: [usize; 3] = [1, 2, 3];
        let _: <Immutable as WrapRef>::WrapRef<'_, [usize]> = &xs;
    }

    #[test]
    fn test_ref_mutable() {
        let mut xs: [usize; 3] = [1, 2, 3];
        let _: <Mutable as WrapRef>::WrapRef<'_, [usize]> = &mut xs;
    }

    #[test]
    fn test_ref_deref() {
        let mut x: TestDeref<Vec<usize>> = TestDeref { value: vec![42] };
        let r: <Mutable as WrapRef>::WrapRef<'_, TestDeref<Vec<usize>>> = &mut x;
        assert_eq!(r.value[0], 42);
    }
}
