// TODO: Support calling `.matches(..)` on types implementing `IntoPattern` but not `Pattern`? In
// other words, move `matches` to `PatternExt` somehow.

// TODO: Test Boolean algebra, make sure methods have correct precedence, compare with normal
// Boolean operators (e.g. assert `true.and(false.or(true)) == true && (false || true) == true`).

// TODO: Possible to drop the `'static` lifetime requirements on boxed functions? Maybe `Matcher`
// could have a lifetime parameter, and then `Matcher`s could be made static by boxing them?

pub enum Either<L, R> {
    Left(L),
    Right(R),
}

pub trait Pattern {
    type Value;
    type Params;
    fn matches(&self, value: &Self::Value) -> Option<Self::Params>;
}

impl<P> Pattern for &P
where
    P: Pattern,
{
    type Value = P::Value;
    type Params = P::Params;
    fn matches(&self, value: &Self::Value) -> Option<Self::Params> {
        P::matches(self, value)
    }
}

impl<P> Pattern for &mut P
where
    P: Pattern,
{
    type Value = P::Value;
    type Params = P::Params;
    fn matches(&self, value: &Self::Value) -> Option<Self::Params> {
        P::matches(self, value)
    }
}

impl<P> Pattern for Box<P>
where
    P: Pattern,
{
    type Value = P::Value;
    type Params = P::Params;
    fn matches(&self, value: &Self::Value) -> Option<Self::Params> {
        P::matches(self, value)
    }
}

pub trait IntoPattern<M, V> {
    type Params;
    type Pattern: Pattern<Value = V, Params = Self::Params>;
    fn into_pattern(self) -> Self::Pattern;
}

impl<V> IntoPattern<(), V> for V
where
    V: PartialEq,
{
    type Params = ();
    type Pattern = Value<V>;
    fn into_pattern(self) -> Self::Pattern {
        Value(self)
    }
}

impl<F, V, P> IntoPattern<(&V, Option<P>), V> for F
where
    F: Fn(&V) -> Option<P> + 'static,
{
    type Params = P;
    type Pattern = Matcher<V, P>;
    fn into_pattern(self) -> Self::Pattern {
        Box::new(self)
    }
}

impl<F, V, P> IntoPattern<(V, Option<P>), V> for F
where
    F: Fn(V) -> Option<P> + 'static,
    V: Copy,
{
    type Params = P;
    type Pattern = Matcher<V, P>;
    fn into_pattern(self) -> Self::Pattern {
        Box::new(move |v| self(*v))
    }
}

impl<F, V> IntoPattern<(&V, bool), V> for F
where
    F: Fn(&V) -> bool + 'static,
{
    type Params = ();
    type Pattern = Predicate<V>;
    fn into_pattern(self) -> Self::Pattern {
        Box::new(move |v| self(v).then_some(()))
    }
}

impl<F, V> IntoPattern<(V, bool), V> for F
where
    F: Fn(V) -> bool + 'static,
    V: Copy,
{
    type Params = ();
    type Pattern = Predicate<V>;
    fn into_pattern(self) -> Self::Pattern {
        Box::new(move |v| self(*v).then_some(()))
    }
}

impl<P, V> IntoPattern<P, V> for P
where
    P: Pattern<Value = V>,
{
    type Params = P::Params;
    type Pattern = P;
    fn into_pattern(self) -> Self::Pattern {
        self
    }
}

pub trait PatternExt: Sized {
    fn and<Other, V, M1, M2>(self, other: Other) -> And<Self::Pattern, Other::Pattern>
    where
        Self: IntoPattern<M1, V>,
        Other: IntoPattern<M2, V>,
    {
        And(self.into_pattern(), other.into_pattern())
    }

    fn or<Other, V, M1, M2>(self, other: Other) -> Or<Self::Pattern, Other::Pattern>
    where
        Self: IntoPattern<M1, V>,
        Other: IntoPattern<M2, V>,
    {
        Or(self.into_pattern(), other.into_pattern())
    }

    fn not<V, M>(self) -> Not<Self::Pattern>
    where
        Self: IntoPattern<M, V>,
    {
        Not(self.into_pattern())
    }
}

fn and<T, U, V, TM, UM>(this: T, other: U) -> And<T::Pattern, U::Pattern>
where
    T: IntoPattern<TM, V>,
    U: IntoPattern<UM, V>,
{
    And(this.into_pattern(), other.into_pattern())
}

fn or<T, U, V, TM, UM>(this: T, other: U) -> Or<T::Pattern, U::Pattern>
where
    T: IntoPattern<TM, V>,
    U: IntoPattern<UM, V>,
{
    Or(this.into_pattern(), other.into_pattern())
}

fn not<T, V, M>(this: T) -> Not<T::Pattern>
where
    T: IntoPattern<M, V>,
{
    Not(this.into_pattern())
}

impl<V> PatternExt for V {}

pub struct Value<V>(pub V);

impl<V> Pattern for Value<V>
where
    V: PartialEq,
{
    type Value = V;
    type Params = ();
    fn matches(&self, value: &Self::Value) -> Option<Self::Params> {
        if self.0 == *value { Some(()) } else { None }
    }
}

pub type Predicate<V> = Matcher<V, ()>;

pub type Matcher<V, P> = Box<dyn Fn(&V) -> Option<P>>;

impl<V, P> Pattern for Matcher<V, P> {
    type Value = V;
    type Params = P;
    fn matches(&self, value: &Self::Value) -> Option<Self::Params> {
        self(value)
    }
}

pub struct And<P1, P2>(pub P1, pub P2);

impl<P1, P2> Pattern for And<P1, P2>
where
    P1: Pattern,
    P2: Pattern<Value = P1::Value>,
{
    type Value = P1::Value;
    type Params = (P1::Params, P2::Params);
    fn matches(&self, value: &Self::Value) -> Option<Self::Params> {
        let p1 = self.0.matches(value)?;
        let p2 = self.1.matches(value)?;
        Some((p1, p2))
    }
}

pub struct Or<P1, P2>(pub P1, pub P2);

impl<P1, P2> Pattern for Or<P1, P2>
where
    P1: Pattern,
    P2: Pattern<Value = P1::Value>,
{
    type Value = P1::Value;
    type Params = Either<P1::Params, P2::Params>;
    fn matches(&self, value: &Self::Value) -> Option<Self::Params> {
        let p1 = self.0.matches(value).map(Either::Left);
        let p2 = self.1.matches(value).map(Either::Right);
        p1.or(p2)
    }
}

pub struct Not<P>(pub P);

impl<P> Pattern for Not<P>
where
    P: Pattern,
{
    type Value = P::Value;
    type Params = ();
    fn matches(&self, value: &Self::Value) -> Option<Self::Params> {
        match self.0.matches(value) {
            Some(_) => None,
            None => Some(()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        {
            let pattern = not(42.or(69).or(420));
            assert!(pattern.matches(&42).is_none());
            assert!(pattern.matches(&69).is_none());
            assert!(pattern.matches(&420).is_none());
            assert!(pattern.matches(&1).is_some());
            assert!(pattern.matches(&2).is_some());
        }
        {
            fn is_even(n: u8) -> bool {
                n % 2 == 0
            }
            fn is_double_digit(n: u8) -> bool {
                (10..=99).contains(&n)
            }
            let pattern = is_even.and(is_double_digit);
            assert!(pattern.matches(&42).is_some());
            assert!(pattern.matches(&69).is_none());
        }
        {
            #[expect(clippy::useless_vec)]
            let magic_numbers = vec![1, 2, 3];
            let bad_number = Box::new(3);
            let is_magic = move |n: u8| magic_numbers.contains(&n);
            let is_bad = move |n: u8| n == *bad_number;
            let pattern = is_magic.and(not(is_bad));
            assert!(pattern.matches(&2).is_some());
            assert!(pattern.matches(&3).is_none());
            assert!(pattern.matches(&69).is_none());
            // let pattern = and(is_magic, is_bad.not());
            // assert!(pattern.matches(&2).is_some());
            // assert!(pattern.matches(&3).is_none());
            // assert!(pattern.matches(&69).is_none());
        }
    }
}
