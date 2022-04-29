#![allow(dead_code)]

use tokio::sync::{mpsc, oneshot};

pub mod core {
    #[derive(Default)]
    pub struct Counter(u8);

    impl Counter {
        pub fn count(&self) -> u8 {
            self.0
        }

        pub fn equals(&self, other: u8) -> bool {
            self.0 == other
        }

        pub fn increment(&mut self) -> Result<(), &'static str> {
            if self.0 == 255 {
                return Err("Cannot count above 255");
            }

            self.0 += 1;

            Ok(())
        }

        pub fn decrement(&mut self) -> Result<(), &'static str> {
            if self.0 == 0 {
                return Err("Cannot count below 0");
            }

            self.0 -= 1;

            Ok(())
        }
    }
}

pub mod event {
    use super::*;
    use crate::core::*;

    #[derive(Debug)]
    pub enum CounterEvent {
        GetCount {
            response: oneshot::Sender<u8>,
        },
        Equals {
            value: u8,
            response: oneshot::Sender<bool>,
        },
        Increment {
            response: oneshot::Sender<Result<(), &'static str>>,
        },
        Decrement {
            response: oneshot::Sender<Result<(), &'static str>>,
        },
    }

    pub async fn dispatch(counter: &mut Counter, event: CounterEvent) {
        use CounterEvent::*;

        match event {
            GetCount { response } => {
                let _ = response.send(counter.count());
            }
            Equals { value, response } => {
                let _ = response.send(counter.equals(value));
            }
            Increment { response } => {
                let _ = response.send(counter.increment());
            }
            Decrement { response } => {
                let _ = response.send(counter.decrement());
            }
        }
    }

    pub async fn run() {
        let (tx, mut rx) = mpsc::channel(8);

        let core_task = tokio::spawn(async move {
            let mut counter = Counter::default();
            while let Some(event) = rx.recv().await {
                dispatch(&mut counter, event).await;
            }
        });

        let client_task = tokio::spawn(async move {
            {
                let (response_tx, response_rx) = oneshot::channel();
                let event = CounterEvent::Increment {
                    response: response_tx,
                };
                tx.send(event).await.unwrap();
                let response = response_rx.await;
                println!("Increment response: {:?}", response);
            }
            {
                let (response_tx, response_rx) = oneshot::channel();
                let event = CounterEvent::GetCount {
                    response: response_tx,
                };
                tx.send(event).await.unwrap();
                let response = response_rx.await;
                println!("GetCount response: {:?}", response);
            }
        });

        client_task.await.unwrap();
        core_task.await.unwrap();
    }
}
