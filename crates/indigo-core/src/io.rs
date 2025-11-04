use camino::{Utf8Path, Utf8PathBuf};
use std::{collections::HashMap, error::Error};
use thiserror::Error;

pub trait Io {
    type Error: Error;

    fn read_file(&mut self, path: impl AsRef<Utf8Path>) -> Result<Vec<u8>, Self::Error>;

    fn write_file(&mut self, path: impl AsRef<Utf8Path>, bytes: &[u8]) -> Result<(), Self::Error>;
}

#[derive(Debug, Error)]
pub(crate) enum TestIoError {
    #[error("File not found: `{0}`")]
    FileNotFound(Utf8PathBuf),
}

#[derive(Default)]
pub(crate) struct TestIo {
    filesystem: HashMap<Utf8PathBuf, Vec<u8>>,
}

impl Io for TestIo {
    type Error = TestIoError;

    fn read_file(&mut self, path: impl AsRef<Utf8Path>) -> Result<Vec<u8>, Self::Error> {
        let path = path.as_ref();
        if let Some(bytes) = self.filesystem.get(path) {
            Ok(bytes.clone())
        } else {
            Err(TestIoError::FileNotFound(path.to_path_buf()))
        }
    }

    fn write_file(&mut self, path: impl AsRef<Utf8Path>, bytes: &[u8]) -> Result<(), Self::Error> {
        let path = path.as_ref();
        self.filesystem.insert(path.to_path_buf(), bytes.to_vec());
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() -> Result<(), TestIoError> {
        let mut io = TestIo::default();
        io.write_file("foo.rs", b"fn foo() {}")?;
        io.write_file("bar.rs", b"fn bar() {}")?;
        assert_eq!(&io.read_file("foo.rs")?, b"fn foo() {}");
        assert_eq!(&io.read_file("bar.rs")?, b"fn bar() {}");
        assert!(io.read_file("baz.rs").is_err());
        io.write_file("foo.rs", b"fn foo() { panic!() }")?;
        assert_eq!(&io.read_file("foo.rs")?, b"fn foo() { panic!() }");
        Ok(())
    }
}
