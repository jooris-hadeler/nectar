use std::{
    env, fs, io,
    path::PathBuf,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};

use miette::NamedSource;

/// A globally unique identifier for a [`Source`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceId(pub(crate) usize);

/// A [`Source`] is a source file on disk, it has a path, content and an id.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Source {
    /// The id of this source file.
    pub id: SourceId,
    /// The absolute path on disk of this source file.
    pub path: PathBuf,
    /// The content of the source file.
    pub content: Arc<String>,
}

impl Source {
    /// Creates a new [`Source`] from a file on disk.
    ///
    /// This returns an [`io::Result`] since the canonicalization of the path,
    /// and the reading of the file might return an error.
    ///
    /// The id of the source is automatically incremented, that way the id
    /// is always globally unique.
    pub fn read_from_disk<P: Into<PathBuf>>(path: P) -> io::Result<Source> {
        static mut SOURCE_ID_COUNTER: AtomicUsize = AtomicUsize::new(0);

        let id = SourceId(unsafe { SOURCE_ID_COUNTER.fetch_add(1, Ordering::Relaxed) });
        let path = fs::canonicalize(path.into())?;
        let content = Arc::new(fs::read_to_string(&path)?);

        Ok(Source { id, path, content })
    }

    /// Returns an iterator over the characters in the source code.
    pub fn chars(&self) -> impl Iterator<Item = char> + '_ {
        self.content.chars()
    }

    /// Calculates the relative path to the current directory.
    fn relative_path(&self) -> Option<PathBuf> {
        pathdiff::diff_paths(&self.path, env::current_dir().ok()?)
    }

    /// Returns a [`str`] to of the path, if the path cannot be displayed
    /// we return `<invalid path>`.
    pub fn path_str(&self) -> String {
        self.relative_path()
            .as_ref()
            .unwrap_or(&self.path)
            .to_str()
            .unwrap_or("<cannot display path>")
            .to_string()
    }
}

impl From<&Source> for NamedSource<Arc<String>> {
    fn from(src: &Source) -> Self {
        NamedSource::new(src.path_str(), src.content.clone())
    }
}
