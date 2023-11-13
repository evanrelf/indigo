use crate::{Buffer, Mode};
use slotmap::SlotMap;

slotmap::new_key_type! { pub struct BufferKey; }

#[derive(Debug)]
pub struct Editor {
    buffers: SlotMap<BufferKey, Buffer>,
    current_buffer: BufferKey,
    mode: Mode,
}

impl Default for Editor {
    fn default() -> Self {
        let mut buffers = SlotMap::with_key();
        let current_buffer = buffers.insert(Buffer::default());
        Self {
            buffers,
            current_buffer,
            mode: Mode::default(),
        }
    }
}

impl Editor {
    #[must_use]
    pub fn get_buffer(&self, buffer_key: BufferKey) -> Option<&Buffer> {
        self.buffers.get(buffer_key)
    }

    #[must_use]
    pub fn get_buffer_mut(&mut self, buffer_key: BufferKey) -> Option<&mut Buffer> {
        self.buffers.get_mut(buffer_key)
    }

    #[must_use]
    pub fn current_buffer(&self) -> &Buffer {
        self.buffers.get(self.current_buffer).unwrap()
    }

    #[must_use]
    pub fn current_buffer_mut(&mut self) -> &mut Buffer {
        self.buffers.get_mut(self.current_buffer).unwrap()
    }

    #[must_use]
    pub fn current_buffer_key(&self) -> BufferKey {
        self.current_buffer
    }

    #[must_use]
    pub fn buffers(&self) -> &SlotMap<BufferKey, Buffer> {
        &self.buffers
    }

    // TODO: Should `mode` just be `pub`? Or are there invariants that need to be enforced, so this
    // should be more restrictive?

    #[must_use]
    pub fn mode(&self) -> &Mode {
        &self.mode
    }

    #[must_use]
    pub fn mode_mut(&mut self) -> &mut Mode {
        &mut self.mode
    }

    #[must_use]
    pub fn insert_buffer(&mut self, buffer: Buffer) -> BufferKey {
        self.buffers.insert(buffer)
    }

    // pub fn remove_buffer(&mut self, buffer_key: BufferKey) -> anyhow::Result<()> {
    //     if !self.buffers.contains_key(buffer_key) {
    //         anyhow::bail!("Buffer does not exist");
    //     }
    //     if self.buffers.len() == 1 {
    //         anyhow::bail!("Cannot remove last buffer");
    //     }
    //     // TODO: Update `current_buffer` to stay valid
    //     self.buffers.remove(buffer_key);
    //     Ok(())
    // }

    pub fn set_current_buffer(&mut self, buffer_key: BufferKey) -> anyhow::Result<()> {
        if !self.buffers.contains_key(buffer_key) {
            anyhow::bail!("Buffer does not exist");
        }
        self.current_buffer = buffer_key;
        Ok(())
    }

    pub fn assert_valid(&self) {
        assert!(
            self.buffers.get(self.current_buffer).is_some(),
            "`current_buffer` index is valid"
        );

        for buffer in self.buffers.values() {
            buffer.assert_valid();
        }
    }
}
