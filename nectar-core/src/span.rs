use miette::SourceSpan;

use crate::source::SourceId;

/// A [`Span`] represents a section of the content in a [`super::source::Source`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    /// The [`SourceId`] of the [`super::source::Source`] we are in.
    pub source_id: SourceId,
    /// The offset of the start in characters.
    pub start: usize,
    /// The offset of the end in characters.
    pub end: usize,
}

impl Span {
    /// Creates a new [`Span`] from an id, start and end offset.
    pub fn new(source_id: SourceId, start: usize, end: usize) -> Self {
        Self {
            source_id,
            start,
            end,
        }
    }

    /// Returns the length of the [`Span`] in characters.
    pub const fn len(&self) -> usize {
        self.end.saturating_sub(self.start)
    }

    /// Returns whether or not the [`Span`] contains no characters.
    pub const fn is_empty(&self) -> bool {
        self.end < self.start
    }
}

impl From<Span> for SourceSpan {
    fn from(span: Span) -> Self {
        SourceSpan::new(
            span.start.into(),
            span.start.saturating_sub(span.end).max(1),
        )
    }
}
