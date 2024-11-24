use std::{env, process::exit};

use miette::{miette as report, Severity};
use nectar_core::source::Source;
use nectar_lexer::Lexer;

fn main() {
    let Some(path) = env::args().nth(1) else {
        eprintln!("usage: nectar <file>");
        exit(1);
    };

    let source = match Source::read_from_disk(path) {
        Ok(source) => source,
        Err(err) => {
            eprintln!(
                "{:?}",
                report!(
                    code = "os::error",
                    severity = Severity::Error,
                    help = "Did you type in the path correctly?",
                    "Failed to read source file.\nDetail: {}",
                    err.kind().to_string()
                )
            );
            exit(1);
        }
    };

    let tokens = {
        let mut lexer = Lexer::new(&source);
        lexer.lex();

        if handle_reports(lexer.take_reports()) {
            exit(-1);
        }

        lexer.take_tokens()
    };

    for token in tokens {
        println!("{token:?}");
    }
}

fn handle_reports(reports: Vec<miette::Report>) -> bool {
    let mut should_exit = false;

    for report in reports {
        should_exit |= report
            .severity()
            .is_some_and(|severity| severity >= Severity::Error);

        eprintln!("{:?}", report);
    }

    should_exit
}
