use async_broadcast::{broadcast, Receiver, RecvError, Sender};

pub fn signal<T>(value: T) -> (Writer<T>, Reader<T>) {
    let (mut sender, receiver) = broadcast(1);
    sender.set_overflow(true);
    sender.set_await_active(false);
    let writer = Writer { value, sender };
    let reader = Reader { receiver };
    (writer, reader)
}

pub struct Writer<T> {
    value: T,
    sender: Sender<T>,
}

impl<T> Writer<T> {
    pub fn get(&self) -> &T {
        &self.value
    }

    pub async fn set(&mut self, value: T)
    where
        T: Clone,
    {
        self.value = value;
        self.notify().await;
    }

    pub async fn update<F>(&mut self, update: F)
    where
        T: Clone,
        F: Fn(&mut T),
    {
        update(&mut self.value);
        self.notify().await;
    }

    async fn notify(&mut self)
    where
        T: Clone,
    {
        self.sender
            .broadcast_direct(self.value.clone())
            .await
            // TODO: Check if this is okay (will it error if nobody's listening?)
            .unwrap();
    }
}

#[derive(Clone)]
pub struct Reader<T> {
    receiver: Receiver<T>,
}

// TODO: "Get current value" vs "wait for next change"
impl<T> Reader<T> {
    pub async fn get(&mut self) -> Result<T, RecvError>
    where
        T: Clone,
    {
        // TODO: Could this loop forever, never going from "error overflowed" to "ok"?
        loop {
            match self.receiver.recv_direct().await {
                Err(RecvError::Overflowed(_)) => continue,
                Ok(value) => return Ok(value),
                Err(RecvError::Closed) => return Err(RecvError::Closed),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        smol::block_on(async {
            let (mut writer, mut reader) = signal(0);

            writer.update(|x| *x += 1).await;

            let x = writer.get();
            assert_eq!(*x, 1);

            let x = reader.get().await.unwrap();
            assert_eq!(x, 1);

            writer.set(42).await;

            let x = writer.get();
            assert_eq!(*x, 42);

            let x = reader.get().await.unwrap();
            assert_eq!(x, 42);
        });
    }
}
