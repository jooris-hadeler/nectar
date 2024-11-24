use std::rc::Rc;

use nectar_core::span::Span;

/// Represents the kind of a [`Token`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Error,

    Identifier,
    Integer,
    Boolean,
    String,

    KwFn,
    KwIf,
    KwElse,
    KwReturn,
    KwAnd,
    KwOr,

    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,

    Dot,
    Comma,
    Colon,
    SemiColon,

    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    Ampersand,
    Pipe,
    Caret,
    Assign,
    Bang,

    Equal,
    Unequal,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,

    Arrow,
    FatArrow,
}

/// Represents a token found in a [`nectar_core::source::Source`]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub text: Option<Rc<String>>,
}
