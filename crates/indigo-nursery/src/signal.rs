use std::sync::{Arc, RwLock};

// https://www.youtube.com/watch?v=cELFZQAMdhQ

pub fn signal<T>(value: T) -> (Getter<T>, Setter<T>) {
    let value = Arc::new(RwLock::new(value));

    let subscribers = Arc::new(RwLock::new(Vec::new()));

    let getter = Getter {
        value: Arc::clone(&value),
        subscribers: Arc::clone(&subscribers),
    };

    let setter = Setter {
        value: Arc::clone(&value),
        subscribers: Arc::clone(&subscribers),
    };

    (getter, setter)
}

pub struct Getter<T> {
    value: Arc<RwLock<T>>,
    #[allow(clippy::type_complexity)]
    subscribers: Arc<RwLock<Vec<Box<dyn Fn(&T)>>>>,
}

impl<T> Getter<T> {
    pub fn get(&self) -> T
    where
        T: Clone,
    {
        (*self.value.read().unwrap()).clone()
    }
}

pub struct Setter<T> {
    value: Arc<RwLock<T>>,
    #[allow(clippy::type_complexity)]
    subscribers: Arc<RwLock<Vec<Box<dyn Fn(&T)>>>>,
}

impl<T> Setter<T> {
    pub fn set(&self, value: T) {
        let mut value_guard = self.value.write().unwrap();

        *value_guard = value;

        for subscriber in &*self.subscribers.read().unwrap() {
            subscriber(&value_guard);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single() {
        let (count, set_count) = signal(0);
        set_count.set(count.get() + 1);
        assert_eq!(count.get(), 1);
    }
}
