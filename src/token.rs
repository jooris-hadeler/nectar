use miette::SourceSpan;

/// Represents a single token in the source code, combining both its semantic type
/// and location information.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    /// Identifies the semantic category of the token (see [`TokenKind`])
    pub kind: TokenKind,
    /// Contains the position information of the token in the source code
    pub span: SourceSpan,
    /// Optional original text of the token. Used for identifiers, strings, and
    /// other tokens where the exact source text needs to be preserved. For most
    /// tokens like operators and keywords, this will be None since the TokenKind
    /// fully determines their textual representation.
    pub text: Option<String>,
}

/// Represents all possible types of tokens in the language.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    /// Names of variables, functions, etc.
    Identifier,
    /// Numeric literal
    Integer,
    /// True/false value
    Boolean,
    /// Text literal
    String,

    /// Function declaration (`fn`)
    KwFn,
    /// Conditional statement (`if`)
    KwIf,
    /// Alternative branch (`else`)
    KwElse,
    /// Alternative conditional (`elif`)
    KwElif,
    /// Variable declaration (`let`)
    KwLet,
    /// Return statement (`return`)
    KwReturn,

    /// Parentheses (`(`)
    LeftParen,
    /// Parentheses (`)`)
    RightParen,
    /// Curly braces (`{`)
    LeftBrace,
    /// Curly braces (`}`)
    RightBrace,
    /// Square brackets (`[`)
    LeftBracket,
    /// Square brackets (`]`)
    RightBracket,

    /// Member access operator (`.`)
    Dot,
    /// Type annotation or label marker (`:`)
    Colon,
    /// Separator (`,`)
    Comma,
    /// Statement terminator (`;`)
    SemiColon,

    /// Bang (`!`)
    Bang,
    /// Plus (`+`)
    Plus,
    /// Minus (`-`)
    Minus,
    /// Asterisk (`*`)
    Asterisk,
    /// Slash (`/`)
    Slash,
    /// Ampersand (`&`)
    Ampersand,
    /// Pipe (`|`)
    Pipe,
    /// Caret (`^`)
    Caret,

    /// Assign (`=`)
    Assign,
    /// Equal (`==`)
    Equal,
    /// Unequal (`!=`)
    Unequal,
    /// Less Than (`<`)
    LessThan,
    /// Less Than or Equal (`<=`),
    LessEqual,
    /// Greater Than (`>`)
    GreaterThan,
    /// Greater Than or Equal (`>=`)
    GreaterEqual,

    /// Return type marker (`->`)
    Arrow,
    /// Closure or Case marker (`=>`)
    FatArrow,
}
