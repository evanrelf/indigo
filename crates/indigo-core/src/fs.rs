//! Abstract filesystem interface.

use crate::IndigoBuildHasher;
use anyhow::{Context as _, anyhow};
use camino::{Utf8Path, Utf8PathBuf};
use std::{
    collections::HashMap,
    env, fs,
    sync::{Arc, Mutex},
};

pub trait Fs {
    fn read(&self, path: &Utf8Path) -> anyhow::Result<Vec<u8>>;

    fn write(&self, path: &Utf8Path, bytes: &[u8]) -> anyhow::Result<()>;

    fn exists(&self, path: &Utf8Path) -> anyhow::Result<bool>;
}

const _ASSERT_FS_IS_DYN_COMPATIBLE: Option<&dyn Fs> = None;

impl<T: Fs + ?Sized> Fs for Arc<T> {
    fn read(&self, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
        T::read(self, path)
    }
    fn write(&self, path: &Utf8Path, bytes: &[u8]) -> anyhow::Result<()> {
        T::write(self, path, bytes)
    }
    fn exists(&self, path: &Utf8Path) -> anyhow::Result<bool> {
        T::exists(self, path)
    }
}

/// Trivial filesystem implementation that panics if you do anything. This is the default filesystem
/// implementation for `Server`.
pub struct NoFs;

impl Fs for NoFs {
    fn read(&self, _path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
        panic!("No filesystem implementation configured");
    }

    fn write(&self, _path: &Utf8Path, _bytes: &[u8]) -> anyhow::Result<()> {
        panic!("No filesystem implementation configured");
    }

    fn exists(&self, _path: &Utf8Path) -> anyhow::Result<bool> {
        panic!("No filesystem implementation configured");
    }
}

/// Real filesystem implementation that uses functions from `std::fs`.
pub struct RealFs;

impl Fs for RealFs {
    fn read(&self, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
        Ok(fs::read(path)?)
    }

    fn write(&self, path: &Utf8Path, bytes: &[u8]) -> anyhow::Result<()> {
        Ok(fs::write(path, bytes)?)
    }

    fn exists(&self, path: &Utf8Path) -> anyhow::Result<bool> {
        Ok(fs::exists(path)?)
    }
}

/// In-memory filesystem implementation for testing. Currently only supports canonicalized paths
/// (i.e. absolute, normalized, and no symlinks).
#[derive(Default)]
pub struct TestFs {
    pub files: Arc<Mutex<HashMap<Utf8PathBuf, Vec<u8>, IndigoBuildHasher>>>,
}

impl Fs for TestFs {
    fn read(&self, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
        let files = self.files.lock().unwrap();
        if let Some(bytes) = files.get(path) {
            Ok(bytes.clone())
        } else {
            Err(anyhow!("File not found: `{path}`"))
        }
    }

    fn write(&self, path: &Utf8Path, bytes: &[u8]) -> anyhow::Result<()> {
        let mut files = self.files.lock().unwrap();
        files.insert(path.to_path_buf(), bytes.to_vec());
        Ok(())
    }

    fn exists(&self, path: &Utf8Path) -> anyhow::Result<bool> {
        let files = self.files.lock().unwrap();
        let exists = files.contains_key(path);
        Ok(exists)
    }
}

pub fn home_dir() -> anyhow::Result<Utf8PathBuf> {
    let home = env::home_dir().context("Failed to get home directory")?;
    Ok(Utf8PathBuf::try_from(home)?)
}

pub fn pwd() -> anyhow::Result<Utf8PathBuf> {
    // Try to use the shell's `$PWD` (doesn't resolve symlinks) if possible
    let unix_cwd = Utf8PathBuf::try_from(env::current_dir()?)?;
    let Ok(shell_pwd) = env::var("PWD").map(Utf8PathBuf::from) else {
        // Missing or not UTF-8
        return Ok(unix_cwd);
    };
    if !shell_pwd.is_absolute() {
        // Not absolute
        return Ok(unix_cwd);
    }
    if !fs::canonicalize(&shell_pwd).is_ok_and(|pwd| pwd == unix_cwd) {
        // Stale
        return Ok(unix_cwd);
    }
    Ok(shell_pwd)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() -> anyhow::Result<()> {
        let fs = TestFs::default();
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
