use std::{iter::Peekable, sync::Arc};

use miette::{LabeledSpan, NamedSource, Severity};

use crate::{
    syntax_tree::{
        ArrayType, BuiltinKind, Declaration, Expression, ExpressionKind, FunctionDeclaration,
        FunctionParameter, Identifier, Module, Type, TypeKind,
    },
    token::{Token, TokenKind},
    util::SourceSpanExt,
};

/// A parser that processes a stream of tokens into a higher-level abstract syntax structure.
///
/// The `Parser` takes an iterator of tokens and provides methods to traverse and interpret
/// them. It is responsible for converting a linear stream of tokens into a structured
/// representation, such as an abstract syntax tree (AST) or module.
///
/// # Type Parameters
/// - `I`: An iterator type that yields `Token` items.
pub struct Parser<I: Iterator<Item = Token>> {
    /// The source code associated with the tokens, wrapped in an `Arc` for shared ownership.
    source: Arc<NamedSource<String>>,
    /// A peekable iterator over the stream of tokens.
    tokens: Peekable<I>,
}

impl<I: Iterator<Item = Token>> Parser<I> {
    /// Creates a new `Parser` instance.
    ///
    /// # Arguments
    /// - `source`: A reference to the source code wrapped in an `Arc<NamedSource<String>>`.
    /// - `tokens`: An iterator of tokens to be parsed.
    ///
    /// # Returns
    /// A `Parser` instance configured to parse the provided tokens.
    pub fn new(source: &Arc<NamedSource<String>>, tokens: I) -> Self {
        Self {
            source: source.clone(),
            tokens: tokens.peekable(),
        }
    }

    /// Peeks at the next token in the stream without consuming it.
    ///
    /// This allows the parser to inspect the upcoming token and make decisions
    /// without advancing the iterator.
    ///
    /// # Returns
    /// - `Some(&Token)` if there is a next token.
    /// - `None` if the end of the token stream is reached.
    #[inline]
    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    /// Advances the token stream and returns the next token.
    ///
    /// This method consumes the current token and moves the iterator forward.
    ///
    /// # Returns
    /// - `Some(Token)` if there is a next token.
    /// - `None` if the end of the token stream is reached.
    #[inline]
    fn next(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    /// Checks if the next token in the token stream matches the expected kind.
    ///
    /// This method peeks at the next token without consuming it and compares its
    /// kind to the specified `expected_kind`. It is useful for lookahead parsing,
    /// where you want to determine the next token's type before deciding how to proceed.
    ///
    /// # Parameters
    /// - `expected_kind`: The kind of token to compare against the next token in the stream.
    ///
    /// # Returns
    /// - `true` if the next token exists and its kind matches the `expected_kind`.
    /// - `false` if the token stream is empty or the next token's kind does not match.
    ///
    /// # Notes
    /// - This method does not consume the token from the stream; it only performs a peek.
    /// - It is often used in conjunction with methods like `expect` or in control flow
    ///   to decide which parsing method to invoke next.
    fn is(&mut self, expected_kind: TokenKind) -> bool {
        self.peek().is_some_and(|tk| tk.kind == expected_kind)
    }

    /// Consumes the next token from the token stream and verifies its kind.
    ///
    /// This method retrieves the next token and ensures that it matches the expected
    /// [`TokenKind`]. If the token matches, it is returned. If the token does not match
    /// the expected kind, or if the token stream has reached the end prematurely,
    /// an error is returned with detailed diagnostic information.
    ///
    /// # Parameters
    /// - `expected_kind`: The kind of token that is expected to be encountered next
    ///   in the token stream.
    ///
    /// # Returns
    /// - `Ok(Token)` if the next token matches the `expected_kind`.
    /// - `Err(miette::Report)` if the next token does not match the expected kind, or if
    ///   the token stream has reached the end prematurely.
    ///
    /// # Notes
    /// - This method is commonly used to enforce the structure of the parsed syntax,
    ///   ensuring that required tokens are present and of the correct type.
    /// - The error reports include source spans and suggestions, making it easier to
    ///   identify and fix issues in the source code.
    fn expect(&mut self, expected_kind: TokenKind) -> miette::Result<Token> {
        match self.next() {
            Some(token) => {
                if token.kind == expected_kind {
                    Ok(token)
                } else {
                    let labeled_span = LabeledSpan::new_primary_with_span(
                        Some(format!("This token should be {expected_kind:?}.")),
                        token.span,
                    );

                    let report = miette::miette!(
                        severity = Severity::Error,
                        code = "parser::unexpected_token",
                        help = format!("Try adding a {expected_kind:?} token."),
                        labels = vec![labeled_span],
                        "Unexpected token found in token stream."
                    );

                    Err(report.with_source_code(self.source.clone()))
                }
            }
            None => {
                let report = miette::miette!(
                    severity = Severity::Error,
                    code = "parser::unexpected_eof",
                    "Unexpected end of file."
                );

                Err(report.with_source_code(self.source.clone()))
            }
        }
    }

    /// Parses the token stream into a [`Module`] representation.
    ///
    /// This method interprets the tokens to construct a high-level representation,
    /// such as a module or an abstract syntax tree.
    ///
    /// # Returns
    /// - `Ok(Module)` containing the parsed module.
    /// - `Err(miette::Report)` if a parsing error occurs.
    pub fn parse(&mut self) -> miette::Result<Module> {
        let mut declarations = Vec::new();

        while self.peek().is_some() {
            declarations.push(self.parse_declaration()?);
        }

        let name = self.source.name().to_string();

        Ok(Module { name, declarations })
    }

    /// Parses a single declaration from the token stream.
    ///
    /// This method processes the tokens provided by the parser to construct
    /// a [`Declaration`], representing a single entity in the module, such as a
    /// function, variable, or type definition.
    ///
    /// # Returns
    /// - `Ok(Declaration)` if a valid declaration is successfully parsed.
    /// - `Err(miette::Report)` if there is an error during parsing, such as
    ///   encountering unexpected tokens or invalid syntax.
    fn parse_declaration(&mut self) -> miette::Result<Declaration> {
        // We can safely unwrap here since this function is only called in `parse`.
        let &Token { kind, span, .. } = self.peek().unwrap();

        match kind {
            TokenKind::KwFn => self.parse_function_declaration().map(Declaration::Function),

            _ => {
                let labeled_span = LabeledSpan::new_primary_with_span(
                    Some("This token is unexpected.".to_string()),
                    span,
                );

                let report = miette::miette!(
                    severity = Severity::Error,
                    code = "parser::invalid_token",
                    help = "Did you mean `fn`, `struct` or `enum`?",
                    labels = vec![labeled_span],
                    "Invalid token at top level."
                );

                Err(report.with_source_code(self.source.clone()))
            }
        }
    }

    /// Parses a function declaration from the token stream.
    ///
    /// This method processes the tokens provided by the parser to construct a
    /// [`FunctionDeclaration`] object, representing the definition of a function. It
    /// extracts the function's name, parameters, optional return type, and optional body.
    ///
    /// # Returns
    /// - `Ok(FunctionDeclaration)` if the function declaration is successfully parsed.
    /// - `Err(miette::Report)` if an error occurs during parsing, such as unexpected
    ///   tokens, missing required components (e.g., function name or parameter list), or
    ///   invalid syntax.
    fn parse_function_declaration(&mut self) -> miette::Result<FunctionDeclaration> {
        self.expect(TokenKind::KwFn)?;

        let name_token = self.expect(TokenKind::Identifier)?;
        let name = Identifier {
            name: name_token.text.unwrap(),
            span: name_token.span,
        };

        self.expect(TokenKind::LeftParen)?;
        let parameters = self.parse_function_parameters()?;
        self.expect(TokenKind::RightParen)?;

        let return_type = if self.is(TokenKind::Arrow) {
            // Skip `->` token.
            self.next();

            Some(self.parse_type()?)
        } else {
            None
        };

        let body = if self.is(TokenKind::LeftBrace) {
            Some(self.parse_compound_statement()?)
        } else {
            self.expect(TokenKind::SemiColon)?;
            None
        };

        Ok(FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
        })
    }

    /// Parses the parameters of a function declaration from the token stream.
    ///
    /// This method processes the tokens in the stream to extract the parameters of a
    /// function. It constructs a [`Vec<FunctionParameter>`], each representing a parameter
    /// with its associated name and optional type. The parsing handles the structure and
    /// syntax of function parameter lists, typically enclosed in parentheses.
    ///
    /// # Returns
    /// - `Ok(Vec<FunctionParameter>)` if the parameters are successfully parsed.
    /// - `Err(miette::Report)` if an error occurs during parsing, such as invalid syntax
    ///   or unexpected tokens.
    ///
    /// # Notes
    /// - The implementation assumes that the function parameters are enclosed in parentheses,
    ///   and each parameter includes a type annotation.
    /// - This method is typically used when parsing function declarations, and the exact
    ///   grammar for parameters may depend on the language's specification (e.g., parameter
    ///   types, default values, etc.).
    fn parse_function_parameters(&mut self) -> miette::Result<Vec<FunctionParameter>> {
        let mut parameters = Vec::new();

        while !self.is(TokenKind::RightParen) {
            if !parameters.is_empty() {
                self.expect(TokenKind::Comma)?;
            }

            parameters.push(self.parse_function_parameter()?);
        }

        Ok(parameters)
    }

    /// Parses a single function parameter from the token stream.
    ///
    /// This method processes the tokens in the stream to extract a single function parameter,
    /// which includes the parameter's name and optionally its type. It handles the syntax of
    /// function parameters, typically in the form `parameter_name: type`. If no type is
    /// provided, the parameter will be parsed with a `None` for its type.
    ///
    /// # Returns
    /// - `Ok(FunctionParameter)` if the parameter is successfully parsed, containing the
    ///   parameter's name and optional type.
    /// - `Err(miette::Report)` if an error occurs during parsing, such as invalid syntax
    ///   or unexpected tokens.
    ///
    /// # Notes
    /// - The implementation assumes that the function parameters are provided in the form
    ///   `name: type`. It may be extended to handle more complex parameter
    ///   scenarios, such as default values or variadic parameters.
    /// - This method is typically invoked during the parsing of a function declaration, and
    ///   handles the individual components of the function's parameter list.
    fn parse_function_parameter(&mut self) -> miette::Result<FunctionParameter> {
        let name_token = self.expect(TokenKind::Identifier)?;
        let name = Identifier {
            name: name_token.text.unwrap(),
            span: name_token.span,
        };

        self.expect(TokenKind::Colon)?;

        let type_ = self.parse_type()?;

        Ok(FunctionParameter { name, type_ })
    }

    /// Parses a type declaration, which can be a pointer type (*), array type ([]), or named type.
    ///
    /// # Returns
    /// - `Ok(Type)` - The parsed type with its kind and source span.
    /// - `Err` - If there's a parsing error.
    fn parse_type(&mut self) -> miette::Result<Type> {
        if self.is(TokenKind::Asterisk) {
            self.parse_pointer_type()
        } else if self.is(TokenKind::LeftBracket) {
            self.parse_array_type()
        } else {
            self.parse_named_type()
        }
    }

    /// Parses a pointer type declaration (e.g., *i32).
    ///
    /// # Returns
    /// - `Ok(Type)` - A pointer type wrapping the parsed inner type.
    /// - `Err` - If the asterisk is missing or inner type parsing fails.
    fn parse_pointer_type(&mut self) -> miette::Result<Type> {
        let asterisk_token = self.expect(TokenKind::Asterisk)?;

        let inner_type = self.parse_type()?;

        let span = asterisk_token.span.join(inner_type.span);

        Ok(Type {
            kind: TypeKind::Pointer(Box::new(inner_type)),
            span,
        })
    }

    /// Parses an array type declaration (e.g., [i32; 10]).
    ///
    /// # Returns
    /// - `Ok(Type)` - An array type with the specified inner type and size.
    /// - `Err` - If the syntax is invalid or parsing fails.
    fn parse_array_type(&mut self) -> miette::Result<Type> {
        let left_bracket_token = self.expect(TokenKind::LeftBracket)?;

        let inner_type = Box::new(self.parse_type()?);

        self.expect(TokenKind::SemiColon)?;

        let size = self.parse_expression()?;

        let right_bracket_token = self.expect(TokenKind::RightBracket)?;

        let span = left_bracket_token.span.join(right_bracket_token.span);

        Ok(Type {
            kind: TypeKind::Array(ArrayType { inner_type, size }),
            span,
        })
    }

    /// Parses a named type, which can be either a builtin type or a custom identifier.
    ///
    /// Recognizes the following builtin types:
    /// - Signed integers: i8, i16, i32, i64
    /// - Unsigned integers: u8, u16, u32, u64
    /// - Boolean: bool
    ///
    /// Any other identifier is treated as a custom named type.
    ///
    /// # Returns
    /// - `Ok(Type)` - The parsed builtin or named type.
    /// - `Err` - If the identifier is missing or invalid.
    fn parse_named_type(&mut self) -> miette::Result<Type> {
        let name_token = self.expect(TokenKind::Identifier)?;
        let name = name_token.text.unwrap();

        let kind = match name.as_str() {
            "i8" => TypeKind::Builtin(BuiltinKind::I8),
            "i16" => TypeKind::Builtin(BuiltinKind::I16),
            "i32" => TypeKind::Builtin(BuiltinKind::I32),
            "i64" => TypeKind::Builtin(BuiltinKind::I64),
            "u8" => TypeKind::Builtin(BuiltinKind::U8),
            "u16" => TypeKind::Builtin(BuiltinKind::U16),
            "u32" => TypeKind::Builtin(BuiltinKind::U32),
            "u64" => TypeKind::Builtin(BuiltinKind::U64),
            "bool" => TypeKind::Builtin(BuiltinKind::Bool),
            _ => {
                let name = Identifier {
                    name,
                    span: name_token.span,
                };

                TypeKind::Named(name)
            }
        };

        Ok(Type {
            kind,
            span: name_token.span,
        })
    }

    /// Parses a compound statement (code block) surrounded by curly braces.
    ///
    /// # Returns
    /// - `Ok(Statement)` - If the compound statement was successfully parsed.
    /// - `Err` - If either the opening or closing brace is missing.
    fn parse_compound_statement(&mut self) -> miette::Result<()> {
        self.expect(TokenKind::LeftBrace)?;
        self.expect(TokenKind::RightBrace)?;

        Ok(())
    }

    /// Parses an expression from the token stream.
    ///
    /// # Returns
    /// - `Ok(Expression)` - If the expression was successfully parsed.
    /// - `Err` - If something went wrong during parsing.
    fn parse_expression(&mut self) -> miette::Result<Expression> {
        let int_token = self.expect(TokenKind::Integer)?;
        let text = int_token.text.unwrap();

        let (radix, text) = if text.starts_with("0x") {
            (16, &text[2..])
        } else if text.starts_with("0o") {
            (8, &text[2..])
        } else if text.starts_with("0b") {
            (2, &text[2..])
        } else {
            (10, text.as_str())
        };

        let value = match u64::from_str_radix(text, radix) {
            Ok(value) => value,
            Err(err) => {
                let labeled_span = LabeledSpan::new_primary_with_span(
                    Some("This integer is invalid.".to_string()),
                    int_token.span,
                );

                let report = miette::miette!(
                    severity = Severity::Error,
                    code = "parser::invalid_integer",
                    help = "Maybe the integer doesn't fit inside the 64-bit bounds?",
                    labels = vec![labeled_span],
                    "Invalid integer literal found in token stream."
                );

                println!("{radix} {err:?}");

                return Err(report.with_source_code(self.source.clone()));
            }
        };

        Ok(Expression {
            kind: ExpressionKind::Integer(value),
            span: int_token.span,
        })
    }
}
