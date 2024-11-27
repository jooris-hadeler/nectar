use std::{env, fs, io::stdout, process::exit, sync::Arc};

use lexer::Lexer;
use miette::{NamedSource, Severity};
use parser::Parser;
use syntax_tree::visualize::SyntaxTreeVisualizer;

mod lexer;
mod parser;
mod syntax_tree;
mod token;
mod util;

fn main() {
    let Some(path) = env::args().nth(1) else {
        let report = miette::miette!(
            severity = Severity::Error,
            code = "cli::invalid_arguments",
            help = "Try adding a path argument.",
            "Missing path argument."
        );

        eprintln!("{report:?}");
        return;
    };

    let content = fs::read_to_string(&path).unwrap();
    let source = Arc::new(NamedSource::new(path, content));

    let lexer = Lexer::new(&source);
    let tokens = match util::collect_results(lexer) {
        Ok(tokens) => tokens,
        Err(reports) => {
            for report in reports {
                eprintln!("{report:?}");
            }
            exit(-1);
        }
    };

    let mut parser = Parser::new(&source, tokens.into_iter());
    let syntax_tree = match parser.parse() {
        Ok(syntax_tree) => syntax_tree,
        Err(err) => {
            eprintln!("{err:?}");
            exit(-1);
        }
    };

    let mut stdout = stdout();
    let mut stv = SyntaxTreeVisualizer::new(&mut stdout);
    stv.write(&syntax_tree)
        .expect("Failed to write SyntaxTree.");
}
