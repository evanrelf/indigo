use camino::Utf8Path;
use pathdiff::diff_utf8_paths;
use std::borrow::Cow;

#[must_use]
pub fn display_path<'a>(path: &'a Utf8Path, pwd: Option<&Utf8Path>) -> Cow<'a, str> {
    match pwd.and_then(|pwd| diff_utf8_paths(path, pwd)) {
        None => Cow::Borrowed(path.as_str()),
        Some(path) => Cow::Owned(path.into_string()),
    }
}
