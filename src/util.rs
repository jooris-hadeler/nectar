use std::fmt::Debug;

use miette::SourceSpan;

/// Collects an iterator of `Result<S, E>` into a single `Result<Vec<S>, Vec<E>>`.
///
/// This function processes an iterator of `Result` values and separates all `Ok` and `Err` variants.
/// If the iterator contains only `Ok` values, it returns `Ok(Vec<S>)` with all the success values collected.
/// If the iterator contains any `Err` values, it returns `Err(Vec<E>)` with all the error values collected.
///
/// # Type Parameters
/// - `S`: The type of the success values contained in `Ok`.
/// - `E`: The type of the error values contained in `Err`.
///
/// # Arguments
/// - `iter`: An iterator yielding `Result<S, E>` items.
///
/// # Returns
/// - `Ok(Vec<S>)` if all items in the iterator are `Ok`.
/// - `Err(Vec<E>)` if one or more items in the iterator are `Err`.
pub fn collect_results<S: Debug, E: Debug, I: Iterator<Item = Result<S, E>>>(
    iter: I,
) -> Result<Vec<S>, Vec<E>> {
    let (oks, errs): (Vec<_>, Vec<_>) = iter.partition(Result::is_ok);

    if errs.is_empty() {
        Ok(oks.into_iter().map(Result::unwrap).collect())
    } else {
        Err(errs.into_iter().map(Result::unwrap_err).collect())
    }
}

/// A trait that extends functionality for types representing source spans.
pub trait SourceSpanExt {
    /// Combines two source spans into a single span that encompasses both.
    ///
    /// # Parameters
    /// - `self`: The first source span.
    /// - `other`: The second source span to join with the first.
    ///
    /// # Returns
    /// A new source span that starts at the beginning of `self` and ends at the
    /// furthest extent of `other`.
    fn join(self, other: Self) -> Self;
}

impl SourceSpanExt for SourceSpan {
    fn join(self, other: Self) -> Self {
        let length = other.offset().saturating_sub(self.offset()) + other.len();
        Self::new(self.offset().into(), length)
    }
}
