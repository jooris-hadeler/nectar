use std::{iter::Peekable, str::Chars, sync::Arc};

use miette::{LabeledSpan, NamedSource, Severity, SourceOffset, SourceSpan};

use crate::token::{Token, TokenKind};

/// A lexical analyzer (lexer) for processing source code.
///
/// The `Lexer` is responsible for iterating over the characters of a source code file
/// and producing tokens that represent meaningful lexical elements, such as keywords,
/// identifiers, numbers, and punctuation.
pub struct Lexer<'a>
where
    NamedSource<String>: 'a,
{
    /// The source code to be lexed, wrapped in an `Arc` for shared ownership.
    source: Arc<NamedSource<String>>,
    /// A peekable iterator over the characters of the source code.
    input: Peekable<Chars<'a>>,
    /// The current position (character offset) in the source code.
    position: usize,
}

impl<'a> Lexer<'a> {
    /// Creates a new `Lexer` instance for the given source code.
    pub fn new(source: &'a Arc<NamedSource<String>>) -> Self {
        Self {
            input: source.inner().chars().peekable(),
            source: source.clone(),
            position: 0,
        }
    }

    /// Peeks at the next character in the input without advancing the position.
    ///
    /// # Returns
    /// - `Some(char)` if there is a next character.
    /// - `None` if the end of the input is reached.
    #[inline]
    fn peek(&mut self) -> Option<char> {
        self.input.peek().copied()
    }

    /// Advances the input and returns the next character.
    /// This increments the position counter.
    ///
    /// # Returns
    /// - `Some(char)` if there is a next character.
    /// - `None` if the end of the input is reached.
    #[inline]
    fn next(&mut self) -> Option<char> {
        self.input.next().inspect(|_| self.position += 1)
    }

    /// Tries to consume the next character if it matches the expected character.
    ///
    /// # Arguments
    /// - `expected_char`: The character to match.
    ///
    /// # Returns
    /// - `true` if the character matches and was consumed.
    /// - `false` otherwise.
    fn try_next(&mut self, expected_char: char) -> bool {
        if self.peek().is_some_and(|ch| ch == expected_char) {
            self.next();
            true
        } else {
            false
        }
    }

    /// Skips over any whitespace characters in the input.
    fn skip_whitespace(&mut self) {
        while self.peek().is_some_and(char::is_whitespace) {
            self.next();
        }
    }

    /// Produces the next token in the input, skipping over whitespace.
    ///
    /// # Returns
    /// - `Some(Ok(Token))` if a token is successfully lexed.
    /// - `Some(Err(miette::Report))` if a lexing error occurs.
    /// - `None` if the end of the input is reached.
    pub fn next_token(&mut self) -> Option<miette::Result<Token>> {
        self.skip_whitespace();

        if self.peek().is_none() {
            None
        } else {
            Some(self.next_token_no_eof())
        }
    }

    /// Produces the next token in the input, assuming it is not at the end of the file.
    ///
    /// # Returns
    /// - `Ok(Token)` if a token is successfully lexed.
    /// - `Err(miette::Report)` if a lexing error occurs.
    fn next_token_no_eof(&mut self) -> miette::Result<Token> {
        let peek = self.peek().unwrap();

        match peek {
            'a'..='z' | 'A'..='Z' | '_' => self.next_identifier_token(),
            '0'..='9' => self.next_number_token(),
            '"' => self.next_string_token(),
            _ => self.next_puncutation_token(),
        }
    }

    /// Lexes an identifier or keyword token.
    ///
    /// Identifiers consist of alphanumeric characters and underscores. Keywords
    /// are predefined sequences like `fn`, `if`, or `return`.
    ///
    /// # Returns
    /// - `Ok(Token)` containing the identifier or keyword token.
    /// - `Err(miette::Report)` if lexing fails.
    fn next_identifier_token(&mut self) -> miette::Result<Token> {
        let start: SourceOffset = self.position.into();
        let mut buffer = String::new();

        while self
            .peek()
            .is_some_and(|ch| matches!(ch, '0'..='9' | 'a' ..='z' | 'A'..='Z' | '_'))
        {
            buffer.push(self.next().unwrap());
        }

        let kind = match buffer.as_str() {
            "fn" => TokenKind::KwFn,
            "if" => TokenKind::KwIf,
            "else" => TokenKind::KwElse,
            "elif" => TokenKind::KwElif,
            "let" => TokenKind::KwLet,
            "return" => TokenKind::KwReturn,
            "and" => TokenKind::KwAnd,
            "or" => TokenKind::KwOr,
            "true" | "false" => TokenKind::Boolean,
            _ => TokenKind::Identifier,
        };

        let length = buffer.chars().count();
        let is_identifier = kind == TokenKind::Identifier;

        assert!(length > 0, "identifier must be atleast one character long");

        let span = SourceSpan::new(start, length);
        let text = is_identifier.then_some(buffer);

        Ok(Token { kind, span, text })
    }

    /// Lexes a numeric literal token.
    ///
    /// Supports decimal, hexadecimal (`0x`), octal (`0o`), and binary (`0b`) literals.
    ///
    /// # Returns
    /// - `Ok(Token)` containing the numeric token.
    /// - `Err(miette::Report)` if the literal is invalid.
    fn next_number_token(&mut self) -> miette::Result<Token> {
        let start: SourceOffset = self.position.into();
        let mut buffer = String::new();

        while self
            .peek()
            .is_some_and(|ch| matches!(ch, '0'..='9' | 'a'..='f' | 'A'..='F' | 'x' | 'o'))
        {
            buffer.push(self.next().unwrap());
        }

        let length = buffer.chars().count();
        let span = SourceSpan::new(start, length);

        let is_valid_digit: fn(char) -> bool = if buffer.starts_with("0x") {
            |ch| ch.is_ascii_hexdigit()
        } else if buffer.starts_with("0o") {
            |ch| matches!(ch, '0'..='7')
        } else if buffer.starts_with("0b") {
            |ch| matches!(ch, '0' | '1')
        } else {
            |ch| ch.is_ascii_digit()
        };

        if !buffer.chars().skip(2).all(is_valid_digit) {
            let labeled_span = LabeledSpan::new_primary_with_span(
                Some("This integer literal is invalid.".to_string()),
                span,
            );

            let report = miette::miette!(
                severity = Severity::Error,
                code = "lexer::invalid_integer",
                help = "Integer literals can be either hexadecimal (`0x`), octal (`0b`), binary (`0b`) or decimal.",
                labels = vec![labeled_span],
                "Invalid integer literal found in source text."
            );

            return Err(report.with_source_code(self.source.clone()));
        }

        Ok(Token {
            kind: TokenKind::Integer,
            span,
            text: Some(buffer),
        })
    }

    /// Lexes a string literal token.
    ///
    /// String literals are enclosed in double quotes (`"`). Unterminated strings
    /// produce an error.
    ///
    /// # Returns
    /// - `Ok(Token)` containing the string token.
    /// - `Err(miette::Report)` if the string is unterminated.
    fn next_string_token(&mut self) -> miette::Result<Token> {
        let start: SourceOffset = self.position.into();
        let mut buffer = String::new();

        // Skip the opening `"`.
        self.next();

        while self.peek().is_some_and(|ch| ch != '"') {
            buffer.push(self.next().unwrap());
        }

        // Check if closing `"` exists and skip it.
        if self.next().is_none() {
            let labeled_span = LabeledSpan::new_primary_with_span(
                Some("The string begins here.".to_string()),
                SourceSpan::new(start, 1),
            );

            let report = miette::miette!(
                severity = Severity::Error,
                code = "lexer::unterminated_string",
                help = "Try adding a terminating `\"`.",
                labels = vec![labeled_span],
                "Unterminated string literal found in source text."
            );

            return Err(report.with_source_code(self.source.clone()));
        }

        let length = self.position.saturating_sub(start.offset());
        let span = SourceSpan::new(start, length);

        Ok(Token {
            kind: TokenKind::String,
            span,
            text: Some(buffer),
        })
    }

    /// Lexes a punctuation or operator token.
    ///
    /// Matches symbols such as `+`, `-`, `*`, `=`, `(`, `)` and others.
    ///
    /// # Returns
    /// - `Ok(Token)` containing the punctuation token.
    /// - `Err(miette::Report)` if the character is invalid.
    fn next_puncutation_token(&mut self) -> miette::Result<Token> {
        let start: SourceOffset = self.position.into();
        let ch = self.next().unwrap();

        let kind = match ch {
            '+' => TokenKind::Plus,
            '-' => {
                if self.try_next('>') {
                    TokenKind::Arrow
                } else {
                    TokenKind::Minus
                }
            }
            '*' => TokenKind::Asterisk,
            '/' => TokenKind::Slash,
            '&' => TokenKind::Ampersand,
            '|' => TokenKind::Pipe,
            '^' => TokenKind::Caret,
            '!' => {
                if self.try_next('>') {
                    TokenKind::Unequal
                } else {
                    TokenKind::Bang
                }
            }
            '=' => {
                if self.try_next('=') {
                    TokenKind::Equal
                } else if self.try_next('>') {
                    TokenKind::FatArrow
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
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '{' => TokenKind::LeftBrace,
            '}' => TokenKind::RightBrace,
            '[' => TokenKind::LeftBracket,
            ']' => TokenKind::RightBracket,
            '.' => TokenKind::Dot,
            ':' => TokenKind::Colon,
            ',' => TokenKind::Comma,
            ';' => TokenKind::SemiColon,

            ch => {
                let labeled_span = LabeledSpan::new_primary_with_span(
                    Some("This character is invalid.".to_string()),
                    SourceSpan::new(start, 1),
                );

                let report = miette::miette!(
                    severity = Severity::Error,
                    code = "lexer::invalid_character",
                    help = "Try deleting this character.",
                    labels = vec![labeled_span],
                    "Invalid character `{ch}` found in source text."
                );

                return Err(report.with_source_code(self.source.clone()));
            }
        };

        let length = self.position.saturating_sub(start.offset());
        let span = SourceSpan::new(start, length);

        Ok(Token {
            kind,
            span,
            text: None,
        })
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = miette::Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}
