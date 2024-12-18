use std::{collections::HashMap, iter::Peekable, sync::Arc};

use lazy_static::lazy_static;
use miette::{LabeledSpan, NamedSource, Severity};

use crate::{
    syntax_tree::{
        ArrayType, BinaryExpression, BinaryOperator, BuiltinKind, CallExpression,
        CompoundStatement, Declaration, Expression, ExpressionKind, FunctionDeclaration,
        FunctionParameter, Identifier, IfStatement, LetStatement, Module, ReturnStatement,
        Statement, Type, TypeKind, UnaryExpression, UnaryOperator,
    },
    token::{Token, TokenKind},
    util::SourceSpanExt,
};

lazy_static! {
    /// A mapping of token kinds to their corresponding prefix operators, along with their precedence.
    ///
    /// The `PREFIX_OPERATORS` map defines how tokens are interpreted as prefix unary operators in expressions.
    /// Each entry maps a `TokenKind` (e.g., `Minus`, `Not`) to:
    /// - A `UnaryOperator` that represents the operation.
    /// - A single precedence value (`u8`), which determines the binding strength of the operator.
    static ref PREFIX_OPERATORS: HashMap<TokenKind, (UnaryOperator, u8)> = HashMap::from([
        (TokenKind::Minus, (UnaryOperator::Negate, 20)),
        (TokenKind::Bang, (UnaryOperator::Not, 18))
    ]);

    /// A mapping of token kinds to their corresponding binary operators, along with their precedence and associativity.
    ///
    /// The `INFIX_OPERATORS` map is used to define how tokens are interpreted as binary operators in expressions.
    /// Each entry maps a `TokenKind` (e.g., `Plus`, `Minus`) to:
    /// - A `BinaryOperator` that represents the operation.
    /// - A pair of precedence levels (`u8, u8`):
    ///   - The left binding power (LBP) for when the operator is used on the left-hand side of an expression.
    ///   - The right binding power (RBP) for when the operator is used on the right-hand side of an expression.
    static ref INFIX_OPERATORS: HashMap<TokenKind, (BinaryOperator, u8, u8)> = HashMap::from([
        (TokenKind::Assign, (BinaryOperator::Assign, 2, 1)),
        (TokenKind::KwOr, (BinaryOperator::LogicalOr, 3, 4)),
        (TokenKind::KwAnd, (BinaryOperator::LogicalAnd, 5, 6)),
        (TokenKind::Equal, (BinaryOperator::Equal, 7, 8)),
        (TokenKind::Unequal, (BinaryOperator::Unequal, 7, 8)),
        (TokenKind::LessThan, (BinaryOperator::LessThan, 9, 10)),
        (TokenKind::LessEqual, (BinaryOperator::LessEqual, 9, 10)),
        (TokenKind::GreaterThan, (BinaryOperator::GreaterThan, 9, 10)),
        (
            TokenKind::GreaterEqual,
            (BinaryOperator::GreaterEqual, 9, 10)
        ),
        (TokenKind::Pipe, (BinaryOperator::BitwiseOr, 11, 12)),
        (TokenKind::Ampersand, (BinaryOperator::BitwiseAnd, 13, 14)),
        (TokenKind::Caret, (BinaryOperator::BitwiseXor, 15, 16)),
        (TokenKind::Plus, (BinaryOperator::Add, 17, 18)),
        (TokenKind::Minus, (BinaryOperator::Subtract, 17, 18)),
        (TokenKind::Asterisk, (BinaryOperator::Multiply, 19, 20)),
        (TokenKind::Slash, (BinaryOperator::Divide, 19, 20)),
    ]);

    /// A mapping of token kinds to their corresponding postfix operators, along with their precedence.
    ///
    /// The `POSTFIX_OPERATORS` map defines how tokens are interpreted as postfix unary operators in expressions.
    /// Each entry maps a `TokenKind` (e.g., `Increment`, `Decrement`) to:
    /// - A `UnaryOperator` that represents the operation.
    /// - A single precedence value (`u8`), which determines the strength of binding for the operator.
    static ref POSTFIX_OPERATORS: HashMap<TokenKind, (UnaryOperator, u8)> = HashMap::from([
        // Use Negate as a placeholder operator for Call expressions.
        (TokenKind::LeftParen, (UnaryOperator::Negate, 32))
    ]);
}

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
    /// - `Err(miette::Report)` - If there's a parsing error.
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
    /// - `Err(miette::Report)` - If the asterisk is missing or inner type parsing fails.
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
    /// - `Err(miette::Report)` - If the syntax is invalid or parsing fails.
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
    /// - `Err(miette::Report)` - If the identifier is missing or invalid.
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

    /// Parses a statement from the current token stream.
    ///
    /// This function identifies and parses different types of statements based on the
    /// current token, including `let` statements, `return` statements, compound statements,
    /// and expression statements. If the token stream ends unexpectedly, an error is returned.
    ///
    /// # Returns
    ///
    /// * `Ok(Statement)` - The parsed statement, wrapped in a [`Statement`] enum variant.
    /// * `Err(miette::Report)` - If an unexpected token or syntax error is encountered.
    fn parse_statement(&mut self) -> miette::Result<Statement> {
        let Some(&Token { kind, .. }) = self.peek() else {
            let report = miette::miette!(
                severity = Severity::Error,
                code = "parser::unexpected_eof",
                help = "Did you forget a closing `}`?",
                "Unexpected end of file."
            );

            return Err(report.with_source_code(self.source.clone()));
        };

        match kind {
            TokenKind::KwIf => self.parse_if_statement(false).map(Statement::If),
            TokenKind::KwLet => self.parse_let_statement().map(Statement::Let),
            TokenKind::KwReturn => self.parse_return_statement().map(Statement::Return),
            TokenKind::LeftBrace => self.parse_compound_statement().map(Statement::Compound),

            // Otherwise try parsing an expression statement.
            _ => self.parse_expression_statement().map(Statement::Expression),
        }
    }

    /// Parses an `if` or `elif` statement from the current token stream.
    ///
    /// This function handles the parsing of conditional control flow statements, including
    /// the main `if` statement, `elif` branches, and the optional `else` block.
    /// It recursively parses nested `elif` branches as [`IfStatement`] instances.
    ///
    /// # Parameters
    ///
    /// - `expect_elif_token` (`bool`): Indicates whether the function expects the current token
    ///   to be `elif`. This is used to differentiate between an initial `if` statement and
    ///   subsequent `elif` branches.
    ///
    /// # Returns
    ///
    /// * `Ok(IfStatement)` - The parsed `IfStatement` representing the conditional control flow.
    /// * `Err(miette::Report)` - If a syntax error or unexpected token is encountered during parsing.
    fn parse_if_statement(&mut self, expect_elif_token: bool) -> miette::Result<IfStatement> {
        if expect_elif_token {
            self.expect(TokenKind::KwElif)?;
        } else {
            self.expect(TokenKind::KwIf)?;
        }

        let condition = self.parse_expression()?;

        let consequence = self.parse_compound_statement()?;

        let alternative = if self.is(TokenKind::KwElif) {
            Some(Statement::If(self.parse_if_statement(true)?))
        } else if self.is(TokenKind::KwElse) {
            self.next();
            Some(Statement::Compound(self.parse_compound_statement()?))
        } else {
            None
        };

        let consequence = Box::new(consequence);
        let alternative = alternative.map(Box::new);

        Ok(IfStatement {
            condition,
            consequence,
            alternative,
        })
    }

    /// Parses a `let` statement from the input and returns a `LetStatement` structure.
    ///
    /// # Returns
    ///
    /// * `Ok(LetStatement)` - If the `let` statement is successfully parsed.
    /// * `Err` - If any part of the statement is malformed or missing.
    fn parse_let_statement(&mut self) -> miette::Result<LetStatement> {
        self.expect(TokenKind::KwLet)?;

        let name_token = self.expect(TokenKind::Identifier)?;
        let name = Identifier {
            name: name_token.text.unwrap(),
            span: name_token.span,
        };

        let type_ = if self.is(TokenKind::Colon) {
            self.next();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(TokenKind::Assign)?;

        let value = self.parse_expression()?;

        self.expect(TokenKind::SemiColon)?;

        Ok(LetStatement { name, type_, value })
    }

    /// Parses a `return` statement from the input.
    ///
    /// This function handles the `return` keyword, an optional return value expression,
    /// and ensures the statement ends with a semicolon (`;`).
    ///
    /// # Returns
    ///
    /// * `Ok(ReturnStatement)` - If the `return` statement is successfully parsed, including an optional value.
    /// * `Err` - If the `return` keyword, optional expression, or terminating semicolon is missing or invalid.
    fn parse_return_statement(&mut self) -> miette::Result<ReturnStatement> {
        let return_token = self.expect(TokenKind::KwReturn)?;
        let span = return_token.span;

        let value = if !self.is(TokenKind::SemiColon) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.expect(TokenKind::SemiColon)?;

        Ok(ReturnStatement { value, span })
    }

    /// Parses a compound statement (code block) surrounded by curly braces.
    ///
    /// # Returns
    /// - `Ok(CompoundStatement)` - If the compound statement was successfully parsed.
    /// - `Err(miette::Report)` - If either the opening or closing brace is missing.
    fn parse_compound_statement(&mut self) -> miette::Result<CompoundStatement> {
        let mut statements = Vec::new();

        self.expect(TokenKind::LeftBrace)?;

        while !self.is(TokenKind::RightBrace) {
            statements.push(self.parse_statement()?);
        }

        self.expect(TokenKind::RightBrace)?;

        Ok(CompoundStatement { statements })
    }

    /// Parses an expression statement from the input and ensures it is followed by a semicolon.
    ///
    /// This function attempts to parse an expression using the `parse_expression` method.
    /// After successfully parsing the expression, it verifies that the next token is a semicolon (`;`).
    /// If parsing the expression or expecting the semicolon fails, an error is returned.
    ///
    /// # Returns
    ///
    /// * `Ok(Expression)` - If the expression is successfully parsed and the semicolon is present.
    /// * `Err` - If parsing the expression fails or the expected semicolon is missing.
    fn parse_expression_statement(&mut self) -> miette::Result<Expression> {
        let expression = self.parse_expression()?;
        self.expect(TokenKind::SemiColon)?;
        Ok(expression)
    }

    /// Parses an expression from the token stream.
    ///
    /// # Returns
    /// - `Ok(Expression)` - If the expression was successfully parsed.
    /// - `Err(miette::Report)` - If something went wrong during parsing.
    fn parse_expression(&mut self) -> miette::Result<Expression> {
        self.parse_expression_with_binding_power(0)
    }

    /// Parses an expression with respect to a given minimum binding power.
    ///
    /// This function is used to parse complex expressions by recursively resolving
    /// prefix, postfix, and infix operators according to their precedence and associativity.
    ///
    /// # Parameters
    ///
    /// - `min_binding_power` (`u8`): The minimum binding power for the current parsing context,
    ///   ensuring that operators with lower precedence are not included in the current expression.
    ///
    /// # Returns
    ///
    /// * `Ok(Expression)` - The parsed expression.
    /// * `Err(miette::Report)` - If an unexpected token or syntax error is encountered during parsing.
    fn parse_expression_with_binding_power(
        &mut self,
        min_binding_power: u8,
    ) -> miette::Result<Expression> {
        let mut left = self.parse_atom_expression()?;

        loop {
            let Some(&Token { kind, span, .. }) = self.peek() else {
                break;
            };

            if let Some(&(op, left_binding_power)) = POSTFIX_OPERATORS.get(&kind) {
                if left_binding_power < min_binding_power {
                    break;
                }

                // Skip operator token.
                self.next();

                left = if kind == TokenKind::LeftParen {
                    let arguments = self.parse_call_arguments()?;
                    let right_paren_token = self.expect(TokenKind::RightParen)?;

                    let span = left.span.join(right_paren_token.span);
                    let call_expression = CallExpression {
                        function: Box::new(left),
                        arguments,
                    };

                    Expression {
                        kind: ExpressionKind::Call(call_expression),
                        span,
                    }
                } else {
                    let span = left.span.join(span);
                    let unary_expression = UnaryExpression {
                        expr: Box::new(left),
                        op,
                    };

                    Expression {
                        kind: ExpressionKind::Unary(unary_expression),
                        span,
                    }
                };

                continue;
            }

            if let Some(&(op, left_binding_power, right_binding_power)) = INFIX_OPERATORS.get(&kind)
            {
                if left_binding_power < min_binding_power {
                    break;
                }

                // Skip operator token.
                self.next();

                let right = self.parse_expression_with_binding_power(right_binding_power)?;

                let span = left.span.join(right.span);
                let binary_expression = BinaryExpression {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                };

                left = Expression {
                    kind: ExpressionKind::Binary(binary_expression),
                    span,
                };

                continue;
            }

            break;
        }

        Ok(left)
    }

    /// Parses the arguments of a function call.
    ///
    /// This function processes the comma-separated list of arguments inside a function call's parentheses.
    /// Each argument is parsed as an [`Expression`]. The parsing continues until a `RightParen` token is encountered.
    ///
    /// # Returns
    ///
    /// * `Ok(Vec<Expression>)` - A vector of parsed expressions representing the function call arguments.
    /// * `Err(miette::Report)` - If a syntax error or unexpected token is encountered during parsing.
    fn parse_call_arguments(&mut self) -> miette::Result<Vec<Expression>> {
        let mut arguments = Vec::new();

        while !self.is(TokenKind::RightParen) {
            if !arguments.is_empty() {
                self.expect(TokenKind::Comma)?;
            }

            arguments.push(self.parse_expression()?);
        }

        Ok(arguments)
    }

    /// Parses the simplest form of an expression, known as an "atomic" expression.
    ///
    /// This function handles parsing of basic expressions such as literals, identifiers, or
    /// expressions prefixed by a unary operator. If a token does not correspond to a valid atomic
    /// expression, an error is returned.
    ///
    /// # Returns
    ///
    /// * `Ok(Expression)` - The parsed atomic expression.
    /// * `Err(miette::Report)` - If an unexpected token or syntax error is encountered.
    fn parse_atom_expression(&mut self) -> miette::Result<Expression> {
        let Some(&Token { kind, span, .. }) = self.peek() else {
            let report = miette::miette!(
                severity = Severity::Error,
                code = "parser::unexpected_eof",
                help = "Did you forget a closing `}`?",
                "Unexpected end of file."
            );

            return Err(report.with_source_code(self.source.clone()));
        };

        if let Some(&(op, right_binding_power)) = PREFIX_OPERATORS.get(&kind) {
            // Skip prefix operator.
            self.next();

            let expr = self.parse_expression_with_binding_power(right_binding_power)?;

            let span = span.join(expr.span);
            let unary_expression = UnaryExpression {
                expr: Box::new(expr),
                op,
            };

            return Ok(Expression {
                kind: ExpressionKind::Unary(unary_expression),
                span,
            });
        }

        match kind {
            TokenKind::Integer => self.parse_integer_expression(),
            TokenKind::Identifier => self.parse_identifier_expression(),
            TokenKind::String => self.parse_string_expression(),
            TokenKind::LeftParen => {
                let left_paren_token = self.next().unwrap();
                let mut expr = self.parse_expression()?;

                let right_paren_token = self.expect(TokenKind::RightParen)?;
                expr.span = left_paren_token.span.join(right_paren_token.span);

                Ok(expr)
            }

            _ => {
                let labeled_span = LabeledSpan::new_primary_with_span(
                    Some("This token was unexpected.".to_string()),
                    span,
                );

                let report = miette::miette!(
                    severity = Severity::Error,
                    code = "parser::unexpected_token",
                    help = "Expected an `integer` or `identifier`.",
                    labels = vec![labeled_span],
                    "Unexpected token found in token stream."
                );

                Err(report.with_source_code(self.source.clone()))
            }
        }
    }

    /// Parses an identifier as an expression.
    ///
    /// This function expects an identifier token and converts it into an `Identifier` expression.
    /// If the current token is not an identifier, an error is returned.
    ///
    /// # Returns
    ///
    /// * `Ok(Expression)` - An `Expression` representing the parsed identifier.
    /// * `Err(miette::Report)` - If the expected identifier token is not found.
    fn parse_identifier_expression(&mut self) -> miette::Result<Expression> {
        let identifier_token = self.expect(TokenKind::Identifier)?;
        let identifier = Identifier {
            name: identifier_token.text.unwrap(),
            span: identifier_token.span,
        };

        Ok(Expression {
            kind: ExpressionKind::Identifier(identifier),
            span: identifier_token.span,
        })
    }

    /// Parses an integer literal as an expression.
    ///
    /// This function processes integer tokens and supports different radices (e.g., decimal, binary,
    /// octal, hexadecimal). It validates the literal and converts it into an `Integer` expression.
    /// If the integer is invalid or out of bounds, an error is returned.
    ///
    /// # Returns
    ///
    /// * `Ok(Expression)` - An `Expression` representing the parsed integer literal.
    /// * `Err(miette::Report)` - If the integer is invalid or not within 64-bit bounds.
    fn parse_integer_expression(&mut self) -> miette::Result<Expression> {
        let int_token = self.expect(TokenKind::Integer)?;
        let text = int_token.text.unwrap();

        let (radix, text) = if let Some(stripped) = text.strip_prefix("0x") {
            (16, stripped)
        } else if let Some(stripped) = text.strip_prefix("0o") {
            (8, stripped)
        } else if let Some(stripped) = text.strip_prefix("0b") {
            (2, stripped)
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

    /// Parses a string token from the input and constructs a `String` expression.
    ///
    /// This function expects the current token to be of kind `TokenKind::String`, retrieves its textual value,
    /// and wraps it in an `Expression` of kind `ExpressionKind::String`. The resulting `Expression` also includes
    /// the span information from the token for tracking its position in the source.
    ///
    /// # Returns
    ///
    /// * `Ok(Expression)` - An `Expression` representing the parsed string literal.
    /// * `Err(miette::Report)` - If the expected string token is not found.
    fn parse_string_expression(&mut self) -> miette::Result<Expression> {
        let string_token = self.expect(TokenKind::String)?;
        let text = string_token.text.unwrap();

        Ok(Expression {
            kind: ExpressionKind::String(text),
            span: string_token.span,
        })
    }
}
