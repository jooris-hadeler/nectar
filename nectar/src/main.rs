use std::{env, io::stderr, process::exit};

use nectar_core::{
    diag::{Diagnostic, Severity},
    source::{Source, Sources},
    span::Span,
};

fn main() {
    let Some(path) = env::args().nth(1) else {
        eprintln!("usage: nectar <file>");
        exit(1);
    };

    let source = match Source::read_from_disk(path) {
        Ok(source) => source,
        Err(err) => {
            eprintln!("error: failed to read source from disk");
            eprintln!("error: {err}");
            exit(1);
        }
    };

    let mut sources = Sources::default();
    let id: nectar_core::source::SourceId = sources.register(source).unwrap();

    Diagnostic::new(
        Severity::Error,
        "No symbol `io` found in current scope.",
        Span::new(id, 23, 25),
    )
    .with_stage("Sema")
    .with_help("Maybe try importing `io` using `import io;`.")
    .print(stderr(), &sources)
    .unwrap();
}
