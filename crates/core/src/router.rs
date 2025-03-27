// TODO
#![allow(clippy::needless_pass_by_value)]
#![allow(clippy::use_self)]
#![allow(unused_variables)]

// Like `axum::Router`
// https://docs.rs/axum/latest/axum/struct.Router.html
pub mod high {
    pub struct Pattern;

    pub struct Handler;

    pub struct Layer;

    #[derive(Default)]
    pub struct Router {}

    impl Router {
        #[must_use]
        pub fn new() -> Self {
            Self::default()
        }

        #[must_use]
        pub fn route(self, pattern: Pattern, handler: Handler) -> Self {
            self
        }

        #[must_use]
        pub fn nest(self, pattern: Pattern, router: Router) -> Self {
            self
        }

        #[must_use]
        pub fn merge(self, router: Router) -> Self {
            self
        }

        #[must_use]
        pub fn layer(self, layer: Layer) -> Self {
            self
        }

        #[must_use]
        pub fn fallback(self, handler: Handler) -> Self {
            self
        }
    }
}

// Like `matchit::Router`
// https://docs.rs/matchit/latest/matchit/struct.Router.html
pub mod low {
    use std::marker::PhantomData;

    pub struct Pattern;

    pub struct Value;

    pub struct Match<T> {
        value: T,
        params: Params,
    }

    pub struct Params;

    pub struct InsertError;

    pub struct MergeError(Vec<InsertError>);

    pub struct MatchError;

    pub struct Router<T> {
        marker: PhantomData<T>,
    }

    impl<T> Router<T> {
        #[must_use]
        pub fn new() -> Self {
            Self::default()
        }

        pub fn insert(&mut self, pattern: Pattern, value: T) -> Result<(), InsertError> {
            todo!()
        }

        pub fn remove(&mut self, pattern: Pattern) -> Option<T> {
            todo!()
        }

        pub fn merge(&mut self, other: Router<T>) -> Result<(), MergeError> {
            let mut errors = Vec::new();
            for (pattern, value) in [/* TODO */] {
                if let Err(error) = self.insert(pattern, value) {
                    errors.push(error);
                }
            }
            if errors.is_empty() {
                Ok(())
            } else {
                Err(MergeError(errors))
            }
        }

        pub fn at(&self, pattern: Pattern) -> Result<Match<&T>, MatchError> {
            todo!()
        }

        pub fn at_mut(&mut self, pattern: Pattern) -> Result<Match<&mut T>, MatchError> {
            todo!()
        }
    }

    impl<T> Default for Router<T> {
        fn default() -> Self {
            Router {
                marker: PhantomData,
            }
        }
    }
}
