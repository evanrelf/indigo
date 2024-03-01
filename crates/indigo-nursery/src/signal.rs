use std::sync::{Arc, Mutex};

// https://www.youtube.com/watch?v=cELFZQAMdhQ

pub fn signal<T>(value: T) -> (Reader<T>, Writer<T>)
where
    T: Clone + 'static,
{
    let value = Arc::new(Mutex::new(value));

    let reader_value = Arc::clone(&value);
    let reader = Reader {
        value: Box::new(move || (*reader_value.lock().unwrap()).clone()),
    };

    let writer_value = Arc::clone(&value);
    let writer = Writer {
        value: writer_value,
    };

    (reader, writer)
}

pub struct Writer<T> {
    value: Arc<Mutex<T>>,
}

impl<T> Writer<T> {
    pub fn set(&self, value: T) {
        *self.value.lock().unwrap() = value;
    }
}

pub struct Reader<T> {
    value: Box<dyn Fn() -> T>,
}

impl<T> Reader<T> {
    pub fn get(&self) -> T {
        (self.value)()
    }

    pub fn map<'me, C, F, U>(&'me self, context: &C, f: F) -> Reader<U>
    where
        C: Clone + 'me,
        F: Fn(T, C) -> U + 'me,
    {
        let context = context.clone();
        Reader {
            value: Box::new(move || f(self.get(), context)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // #[test]
    // fn test() {
    //     let (count, set_count) = signal(2);
    //     let (multipler, set_multiplier) = signal(2);
    //     let product = count.map(|x| x * multipler.get());
    //     set_count.set(count.get() + 1);
    //     set_multiplier.set(multipler.get() + 1);
    //     assert_eq!(
    //         format!("{} * {} = {}", count.get(), multipler.get(), product.get()),
    //         "3 * 3 = 9"
    //     );
    // }
}
