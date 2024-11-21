use std::{fmt::Display, io};

use yansi::{Paint, Style};

use crate::{source::Sources, span::Span};

/// Represents the severity of a diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Severity {
    Note,
    Warning,
    Error,
}

impl Severity {
    /// Returns the [`Style`] to paint the Severity in.
    pub fn style(&self) -> Style {
        match self {
            Severity::Note => Style::new().bright_blue(),
            Severity::Warning => Style::new().bright_yellow(),
            Severity::Error => Style::new().bright_red(),
        }
        .bold()
    }
}

impl Display for Severity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", format!("{self:?}").paint(self.style()))
    }
}

/// Represents a diagnostic emitted during compilation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    severity: Severity,
    stage: Option<String>,
    message: String,
    help: Option<String>,
    span: Span,
}

impl Diagnostic {
    /// Creates a new [`Diagnostic`] with the required fields.
    pub fn new<S: ToString>(severity: Severity, message: S, span: Span) -> Self {
        Self {
            severity,
            stage: None,
            message: message.to_string(),
            help: None,
            span,
        }
    }

    /// Sets the stage the diagnostic occured at.
    pub fn with_stage<S: ToString>(mut self, stage: S) -> Self {
        self.stage = Some(stage.to_string());
        self
    }

    /// Sets the optional help message.
    pub fn with_help<S: ToString>(mut self, help: S) -> Self {
        self.help = Some(help.to_string());
        self
    }

    /// Prints the diagnostic.
    pub fn print(self, mut stream: impl io::Write, sources: &Sources) -> io::Result<()> {
        write!(stream, "{}", self.severity)?;

        if let Some(stage) = self.stage {
            write!(stream, "{}", format!("({stage})").white().bold())?;
        }

        writeln!(stream, ": {}", self.message.white())?;

        let Some(source) = sources.get(self.span.source_id) else {
            return Ok(());
        };

        let lines = source.get_line_range(self.span);
        let line_width = lines.end.ilog10() as usize + 1;

        write!(stream, "  {}{} ", " ".repeat(line_width), "╭".bright_blue())?;

        match source.get_line_from_offset(self.span.start) {
            Some((_, row, col)) => writeln!(
                stream,
                "{}",
                format!("{}:{}:{}", source.path_str(), row + 1, col + 1).bright_blue()
            ),
            None => writeln!(stream, "{}", source.path_str().bright_blue()),
        }?;

        for line_idx in lines {
            let line = source.line(line_idx).unwrap();

            let Some(line_text) = source.line_text(line).map(str::trim_end) else {
                continue;
            };

            if line_text.is_empty() {
                continue;
            }

            writeln!(
                stream,
                " {} {} {}",
                (line_idx + 1).bright_blue(),
                "│".bright_blue(),
                line_text
            )?;

            let span_start = self.span.start.saturating_sub(line.offset);
            let span_end = span_start.saturating_add(self.span.len()).min(line.length);
            let span_len = span_end.saturating_sub(span_start);

            let span_section = " ".repeat(span_start) + &"‾".repeat(span_len);
            let span_section = span_section.paint(self.severity.style());

            writeln!(
                stream,
                " {} {} {}",
                " ".repeat(line_width),
                "│".bright_blue(),
                span_section
            )?;
        }

        if let Some(help) = self.help {
            writeln!(
                stream,
                " {} {} {}: {}",
                " ".repeat(line_width),
                "├".bright_blue(),
                "Help".bright_green().bold(),
                help
            )?;
        }

        writeln!(stream, "  {}{} ", " ".repeat(line_width), "╯".bright_blue())?;

        Ok(())
    }
}
