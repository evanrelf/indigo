use anyhow::anyhow;
use camino::{Utf8Path, Utf8PathBuf};
use std::collections::HashMap;

// Needs to remain dyn compatible.
pub trait Io {
    fn read_file(&mut self, path: &Utf8Path) -> anyhow::Result<Vec<u8>>;

    fn write_file(&mut self, path: &Utf8Path, bytes: &[u8]) -> anyhow::Result<()>;

    fn file_exists(&mut self, path: &Utf8Path) -> anyhow::Result<bool>;
}

impl<I: Io + ?Sized> Io for Box<I> {
    fn read_file(&mut self, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
        I::read_file(self, path)
    }
    fn write_file(&mut self, path: &Utf8Path, bytes: &[u8]) -> anyhow::Result<()> {
        I::write_file(self, path, bytes)
    }
    fn file_exists(&mut self, path: &Utf8Path) -> anyhow::Result<bool> {
        I::file_exists(self, path)
    }
}

// Trivial I/O implementation that panics if you do anything. This is the default I/O implementation
// for `Editor`.
pub struct NoIo;

impl Io for NoIo {
    fn read_file(&mut self, _path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
        panic!("No I/O implementation configured");
    }
    fn write_file(&mut self, _path: &Utf8Path, _bytes: &[u8]) -> anyhow::Result<()> {
        panic!("No I/O implementation configured");
    }
    fn file_exists(&mut self, _path: &Utf8Path) -> anyhow::Result<bool> {
        panic!("No I/O implementation configured");
    }
}

#[cfg_attr(not(test), expect(dead_code))]
#[derive(Default)]
pub(crate) struct TestIo {
    pub filesystem: HashMap<Utf8PathBuf, Vec<u8>>,
}

impl Io for TestIo {
    fn read_file(&mut self, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
        if let Some(bytes) = self.filesystem.get(path) {
            Ok(bytes.clone())
        } else {
            Err(anyhow!("File not found: `{path}`"))
        }
    }

    fn write_file(&mut self, path: &Utf8Path, bytes: &[u8]) -> anyhow::Result<()> {
        self.filesystem.insert(path.to_path_buf(), bytes.to_vec());
        Ok(())
    }

    fn file_exists(&mut self, path: &Utf8Path) -> anyhow::Result<bool> {
        let exists = self.filesystem.contains_key(path);
        Ok(exists)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() -> anyhow::Result<()> {
        let mut io = TestIo::default();
        io.write_file("foo.rs".into(), b"fn foo() {}")?;
        io.write_file("bar.rs".into(), b"fn bar() {}")?;
        assert!(io.file_exists("foo.rs".into())?);
        assert_eq!(&io.read_file("foo.rs".into())?, b"fn foo() {}");
        assert_eq!(&io.read_file("bar.rs".into())?, b"fn bar() {}");
        assert!(io.read_file("baz.rs".into()).is_err());
        assert!(!io.file_exists("baz.rs".into())?);
        io.write_file("foo.rs".into(), b"fn foo() { panic!() }")?;
        assert_eq!(&io.read_file("foo.rs".into())?, b"fn foo() { panic!() }");
        Ok(())
    }
}
