use std::{char, iter::Peekable, rc::Rc, str::Chars};

use miette::{miette as report, LabeledSpan, NamedSource, Severity};
use nectar_core::{source::Source, span::Span};
use token::{Token, TokenKind};

pub mod token;

/// Holds the state needed for lexical analysis.
pub struct Lexer<'a> {
    /// The [Source] we are currently lexing.
    source: &'a Source,
    /// The list of characters in said [Source].
    content: Peekable<Chars<'a>>,
    /// The current position in the [Source].
    position: usize,
    /// The list of tokens produced by the lexer.
    tokens: Vec<Token>,
    /// The list of reports produced by the lexer.
    reports: Vec<miette::Report>,
}

impl<'a> Lexer<'a> {
    /// Creates a new [Lexer] for a given [Source].
    pub fn new(source: &'a Source) -> Self {
        Self {
            content: source.content.chars().peekable(),
            source,
            position: 0,
            tokens: Vec::new(),
            reports: Vec::new(),
        }
    }

    /// Takes the reports that were generated during lexing.
    pub fn take_reports(&mut self) -> Vec<miette::Report> {
        std::mem::take(&mut self.reports)
    }

    /// Add a report to the reports vector.
    fn add_report(&mut self, report: miette::Report) {
        let src: NamedSource<_> = self.source.into();
        self.reports.push(report.with_source_code(src));
    }

    /// Takes the tokens that were generated during lexing.
    pub fn take_tokens(&mut self) -> Vec<Token> {
        std::mem::take(&mut self.tokens)
    }

    /// Add a token to the tokens vector.
    fn add_token(&mut self, token: Token) {
        self.tokens.push(token);
    }

    /// Peeks at the next character.
    #[inline]
    fn peek(&mut self) -> Option<char> {
        self.content.peek().copied()
    }

    /// Returns the next character and advances the iterator.
    #[inline]
    fn next(&mut self) -> Option<char> {
        self.content.next().inspect(|_| {
            self.position += 1;
        })
    }

    /// Skips whitespace in the character iterator.
    fn skip_whitespace(&mut self) {
        while self.peek().is_some_and(char::is_whitespace) {
            self.next();
        }
    }

    /// Advances the iterator if the next character is equal to `expected`.
    fn try_next(&mut self, expected: char) -> bool {
        if self.peek().is_some_and(|ch| ch == expected) {
            self.next();
            true
        } else {
            false
        }
    }

    /// Lexes an identifier token.
    fn lex_identifier(&mut self) {
        let mut buffer = String::new();
        let start = self.position;

        while self
            .peek()
            .is_some_and(|ch| matches!(ch, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'))
        {
            buffer.push(self.next().unwrap());
        }

        let span = Span::new(self.source.id, start, self.position);

        let kind = match buffer.as_str() {
            "fn" => TokenKind::KwFn,
            "if" => TokenKind::KwIf,
            "else" => TokenKind::KwElse,
            "return" => TokenKind::KwReturn,
            "and" => TokenKind::KwAnd,
            "or" => TokenKind::KwOr,
            _ => TokenKind::Identifier,
        };

        let is_identifier = kind == TokenKind::Identifier;

        self.add_token(Token {
            kind,
            span,
            text: is_identifier.then_some(Rc::new(buffer)),
        })
    }

    /// Lexes a number token.
    fn lex_number(&mut self) {
        let mut buffer = String::new();
        let start = self.position;

        while self.peek().is_some_and(|ch| ch.is_ascii_digit()) {
            buffer.push(self.next().unwrap());
        }

        let span = Span::new(self.source.id, start, self.position);

        self.add_token(Token {
            kind: TokenKind::Integer,
            span,
            text: Some(Rc::new(buffer)),
        })
    }

    /// Lexes a string token.
    fn lex_string(&mut self) {
        let mut buffer = String::new();
        let start = self.position;

        self.next();

        while self.peek().is_some_and(|ch| ch != '"') {
            buffer.push(self.next().unwrap());
        }

        let span = Span::new(self.source.id, start, start + 1);

        if self.next().is_none() {
            let label =
                LabeledSpan::new_with_span(Some("The string begins here.".to_string()), span);

            self.add_report(report!(
                severity = Severity::Error,
                code = "lexer::unterminated_string",
                help = "Try adding a closing `\"`.",
                labels = vec![label],
                "Unterminated string found in source code."
            ));
        }

        self.add_token(Token {
            kind: TokenKind::String,
            span,
            text: Some(Rc::new(buffer)),
        })
    }

    /// Lexes a punctuation token.
    fn lex_puncuation(&mut self) {
        let start = self.position;

        let kind = match self.next().unwrap() {
            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            '[' => TokenKind::OpenBracket,
            ']' => TokenKind::CloseBracket,
            '{' => TokenKind::OpenBrace,
            '}' => TokenKind::CloseBrace,

            '.' => TokenKind::Dot,
            ',' => TokenKind::Comma,
            ':' => TokenKind::Colon,
            ';' => TokenKind::SemiColon,

            '-' => {
                if self.try_next('>') {
                    TokenKind::Arrow
                } else {
                    TokenKind::Minus
                }
            }

            '=' => {
                if self.try_next('>') {
                    TokenKind::FatArrow
                } else if self.try_next('=') {
                    TokenKind::Equal
                } else {
                    TokenKind::Assign
                }
            }

            '<' => {
                if self.try_next('=') {
                    TokenKind::LessEqual
                } else {
                    TokenKind::LessThan
                }
            }

            '>' => {
                if self.try_next('=') {
                    TokenKind::GreaterEqual
                } else {
                    TokenKind::GreaterThan
                }
            }

            '!' => {
                if self.try_next('=') {
                    TokenKind::Unequal
                } else {
                    TokenKind::Bang
                }
            }

            '+' => TokenKind::Plus,
            '*' => TokenKind::Asterisk,
            '/' => TokenKind::Slash,
            '%' => TokenKind::Percent,
            '&' => TokenKind::Ampersand,
            '|' => TokenKind::Pipe,
            '^' => TokenKind::Caret,

            ch => {
                self.next();

                let span = Span::new(self.source.id, start, self.position);
                let label = LabeledSpan::new_with_span(
                    Some("This character is invalid.".to_string()),
                    span,
                );

                self.add_report(report!(
                    severity = Severity::Error,
                    code = "lexer::invalid_character",
                    help = "Try removing this character.",
                    labels = vec![label],
                    "Invalid character `{}` found in source code.",
                    ch
                ));
                return;
            }
        };

        self.add_token(Token {
            kind,
            span: Span::new(self.source.id, start, self.position),
            text: None,
        })
    }

    /// Lexes the tokens in the [Source].
    pub fn lex(&mut self) {
        loop {
            self.skip_whitespace();

            let Some(ch) = self.peek() else {
                break;
            };

            match ch {
                'a'..='z' | 'A'..='Z' | '_' => self.lex_identifier(),
                '0'..='9' => self.lex_number(),
                '"' => self.lex_string(),
                _ => self.lex_puncuation(),
            }
        }
    }
}
