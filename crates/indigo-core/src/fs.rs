use anyhow::anyhow;
use camino::{Utf8Path, Utf8PathBuf};
use std::{collections::HashMap, fs};

// Needs to remain dyn compatible.
pub trait Fs {
    fn read(&mut self, path: &Utf8Path) -> anyhow::Result<Vec<u8>>;

    fn write(&mut self, path: &Utf8Path, bytes: &[u8]) -> anyhow::Result<()>;

    fn exists(&mut self, path: &Utf8Path) -> anyhow::Result<bool>;
}

impl<T: Fs + ?Sized> Fs for Box<T> {
    fn read(&mut self, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
        T::read(self, path)
    }
    fn write(&mut self, path: &Utf8Path, bytes: &[u8]) -> anyhow::Result<()> {
        T::write(self, path, bytes)
    }
    fn exists(&mut self, path: &Utf8Path) -> anyhow::Result<bool> {
        T::exists(self, path)
    }
}

// Trivial filesystem implementation that panics if you do anything. This is the default filesystem
// implementation for `Editor`.
pub struct NoFs;

impl Fs for NoFs {
    fn read(&mut self, _path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
        panic!("No filesystem implementation configured");
    }
    fn write(&mut self, _path: &Utf8Path, _bytes: &[u8]) -> anyhow::Result<()> {
        panic!("No filesystem implementation configured");
    }
    fn exists(&mut self, _path: &Utf8Path) -> anyhow::Result<bool> {
        panic!("No filesystem implementation configured");
    }
}

pub struct RealFs;

impl Fs for RealFs {
    fn read(&mut self, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
        Ok(fs::read(path)?)
    }

    fn write(&mut self, path: &Utf8Path, bytes: &[u8]) -> anyhow::Result<()> {
        Ok(fs::write(path, bytes)?)
    }

    fn exists(&mut self, path: &Utf8Path) -> anyhow::Result<bool> {
        Ok(fs::exists(path)?)
    }
}

#[cfg_attr(not(test), expect(dead_code))]
#[derive(Default)]
pub(crate) struct TestFs(pub HashMap<Utf8PathBuf, Vec<u8>>);

impl Fs for TestFs {
    fn read(&mut self, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
        if let Some(bytes) = self.0.get(path) {
            Ok(bytes.clone())
        } else {
            Err(anyhow!("File not found: `{path}`"))
        }
    }

    fn write(&mut self, path: &Utf8Path, bytes: &[u8]) -> anyhow::Result<()> {
        self.0.insert(path.to_path_buf(), bytes.to_vec());
        Ok(())
    }

    fn exists(&mut self, path: &Utf8Path) -> anyhow::Result<bool> {
        let exists = self.0.contains_key(path);
        Ok(exists)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() -> anyhow::Result<()> {
        let mut fs = TestFs::default();
        fs.write("foo.rs".into(), b"fn foo() {}")?;
        fs.write("bar.rs".into(), b"fn bar() {}")?;
        assert!(fs.exists("foo.rs".into())?);
        assert_eq!(&fs.read("foo.rs".into())?, b"fn foo() {}");
        assert_eq!(&fs.read("bar.rs".into())?, b"fn bar() {}");
        assert!(fs.read("baz.rs".into()).is_err());
        assert!(!fs.exists("baz.rs".into())?);
        fs.write("foo.rs".into(), b"fn foo() { panic!() }")?;
        assert_eq!(&fs.read("foo.rs".into())?, b"fn foo() { panic!() }");
        Ok(())
    }
}
