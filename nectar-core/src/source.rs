use std::{
    collections::{hash_map::Entry, HashMap},
    env, fs, io,
    ops::Range,
    path::PathBuf,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::span::Span;

/// A globally unique identifier for a [`Source`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceId(pub(crate) usize);

/// A [`Source`] is a source file on disk, it has a path, content and an id.
#[derive(Debug, PartialEq, Eq)]
pub struct Source {
    /// The id of this source file.
    pub id: SourceId,
    /// The absolute path on disk of this source file.
    pub path: PathBuf,
    /// The content of the source file.
    pub content: String,
    /// The line positions in the source file.
    pub lines: Vec<Line>,
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
        let content = fs::read_to_string(&path)?;
        let lines = find_line_positions(&content);

        Ok(Source {
            id,
            path,
            content,
            lines,
        })
    }

    /// Returns an iterator over the characters in the source code.
    pub fn chars(&self) -> impl Iterator<Item = char> + '_ {
        self.content.chars()
    }

    /// Returns a line from its index.
    pub fn line(&self, idx: usize) -> Option<Line> {
        self.lines.get(idx).copied()
    }

    /// Returns an iterator over the lines in the source code.
    pub fn lines(&self) -> impl Iterator<Item = Line> + '_ {
        self.lines.iter().copied()
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

    /// Returns the [`Line`] that the given offset appears in, aswell as the row and column of the offset.
    pub fn get_line_from_offset(&self, offset: usize) -> Option<(Line, usize, usize)> {
        if offset > self.content.len() {
            return None;
        }

        let idx = self
            .lines
            .binary_search_by_key(&offset, |line| line.offset)
            .unwrap_or_else(|idx| idx.saturating_sub(1));

        let line = self.line(idx)?;

        Some((line, idx, offset - line.offset))
    }

    /// Get the range of lines that this span runs across.
    pub fn get_line_range(&self, span: Span) -> Range<usize> {
        assert_eq!(span.source_id, self.id);

        let start = self
            .get_line_from_offset(span.start)
            .map_or(0, |(_, l, _)| l);

        let end = self
            .get_line_from_offset(span.end.saturating_sub(1).max(span.start))
            .map_or(self.lines.len(), |(_, l, _)| l + 1);

        start..end
    }

    /// Get the source text for a line, includes trailing whitespace and the newline
    pub fn line_text(&self, line: Line) -> Option<&'_ str> {
        self.content.get(line.byte_span())
    }
}

/// Represents the position and length of a Line in a Source.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Line {
    pub offset: usize,
    pub length: usize,
    pub byte_offset: usize,
    pub byte_length: usize,
}

impl Line {
    /// Returns the byte span of the Line.
    pub fn byte_span(&self) -> Range<usize> {
        self.byte_offset..(self.byte_offset + self.byte_length)
    }
}

/// Finds the positions and lengths of the lines in the input String.
fn find_line_positions(content: &str) -> Vec<Line> {
    if content.is_empty() {
        return vec![Line {
            offset: 0,
            length: 0,
            byte_offset: 0,
            byte_length: 0,
        }];
    }

    let mut lines = Vec::new();
    let mut line_iterator = content.split_inclusive(['\r', '\n']).peekable();

    let mut offset = 0;
    let mut byte_offset = 0;

    while let Some(line) = line_iterator.next() {
        let mut length = line.chars().count();
        let mut byte_length = line.len();

        if line.ends_with('\r') && line_iterator.next_if_eq(&"\n").is_some() {
            byte_length += 1;
            length += 1;
        }

        lines.push(Line {
            offset,
            length,
            byte_offset,
            byte_length,
        });

        offset += length;
        byte_offset += byte_length;
    }

    lines
}

#[derive(Debug, Default)]
pub struct Sources {
    sources: HashMap<SourceId, Source>,
}

impl Sources {
    /// Registers a [`Source`], if the [`Source`] wasn't registered yet we return the [`SourceId`] else [`None`].
    pub fn register(&mut self, source: Source) -> Option<SourceId> {
        let id = source.id;

        if let Entry::Vacant(e) = self.sources.entry(id) {
            e.insert(source);
            Some(id)
        } else {
            None
        }
    }

    /// Returns a reference to a [`Source`] by it [`SourceId`].
    pub fn get(&self, source_id: SourceId) -> Option<&Source> {
        self.sources.get(&source_id)
    }
}
