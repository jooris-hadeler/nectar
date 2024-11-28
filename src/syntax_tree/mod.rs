use miette::SourceSpan;

pub mod visualize;

/// Represents a module in the source code.
///
/// A module is a top-level construct that typically encapsulates a collection
/// of related declarations, such as functions, variables, types, or other
/// structural components of the language.
#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    /// The name of the module.
    pub name: String,
    /// A collection of declarations that belong to this module.
    pub declarations: Vec<Declaration>,
}

/// Represents an identifier in the source code.
///
/// Identifiers are used to name various entities such as variables, functions, or
/// parameters. They include the name of the entity and the span of source code where
/// the identifier is located.
#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    /// The textual name of the identifier.
    pub name: String,
    /// The span in the source code where the identifier is located.
    pub span: SourceSpan,
}

/// Represents a declaration within a module.
///
/// Declarations define the various entities within a module, such as
/// functions, variables, or types. This enum can be expanded to include
/// specific variants for each type of declaration.
#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    /// A function declaration.
    ///
    /// This variant holds a [`FunctionDeclaration`] struct, which contains
    /// the details of a function such as its name, parameters, return type,
    /// and body. It is used to represent function declarations in the source code.
    Function(FunctionDeclaration),
}

/// Represents a function declaration within the source code.
///
/// A function declaration defines a named function, its parameters, return type,
/// and optionally its body. Functions may be declared without a body in cases such
/// as interfaces or abstract definitions.
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    /// The name of the function as an identifier.
    pub name: Identifier,
    /// The list of parameters the function accepts.
    pub parameters: Vec<FunctionParameter>,
    /// The return type of the function, if specified.
    pub return_type: Option<Type>,
    /// The body of the function, if defined.
    pub body: Option<CompoundStatement>,
}

/// Represents a single parameter in a function declaration.
///
/// Function parameters define the inputs that a function accepts. Each parameter
/// has a name and optionally a type.
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParameter {
    /// The name of the parameter as an identifier.
    pub name: Identifier,
    /// The type of the parameter, if specified.
    pub type_: Type,
}

/// Represents a type in the language, containing the specific type variant and its location in source code.
#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    /// The specific kind/variant of this type.
    pub kind: TypeKind,
    /// The location of this type in the source code.
    pub span: SourceSpan,
}

/// Represents all possible type variants in the language.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    /// A built-in primitive type like integers or booleans.
    Builtin(BuiltinKind),
    /// A pointer type that references another type.
    Pointer(Box<Type>),
    /// A fixed-size array type.
    Array(ArrayType),
    /// A named type.
    Named(Identifier),
}

/// Represents all built-in primitive types supported by the language.
#[derive(Debug, Clone, PartialEq)]
pub enum BuiltinKind {
    /// 8-bit signed integer
    I8,
    /// 16-bit signed integer
    I16,
    /// 32-bit signed integer
    I32,
    /// 64-bit signed integer
    I64,
    /// 8-bit unsigned integer
    U8,
    /// 16-bit unsigned integer
    U16,
    /// 32-bit unsigned integer
    U32,
    /// 64-bit unsigned integer
    U64,
    /// Boolean type (true/false)
    Bool,
}

/// Represents a fixed-size array type with a specific element type.
#[derive(Debug, Clone, PartialEq)]
pub struct ArrayType {
    /// The type of elements stored in the array.
    pub inner_type: Box<Type>,
    /// The fixed size of the array.
    pub size: Expression,
}

/// Represents different types of statements in the language
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    /// Represents a variable declaration (let) statement
    Let(LetStatement),
    /// Represents an if statement.
    If(IfStatement),
    /// Represents a return statement
    Return(ReturnStatement),
    /// Represents a compound statement containing multiple statements
    Compound(CompoundStatement),
    /// Represents an expression statement
    Expression(Expression),
}

/// Represents a variable declaration statement with an optional type and a value
#[derive(Debug, Clone, PartialEq)]
pub struct LetStatement {
    /// The name of the variable being declared
    pub name: Identifier,
    /// Optional type annotation for the variable
    pub type_: Option<Type>,
    /// The initial value assigned to the variable
    pub value: Expression,
}

/// Represents an `if` statement for conditional control flow.
#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    /// The condition to evaluate for the `if` statement.
    pub condition: Expression,
    /// The block of statements to execute if the condition evaluates to true.
    pub consequence: Box<CompoundStatement>,
    /// An optional block of statements to execute if the condition evaluates to false.
    ///
    /// We use [Statement] here since the alternative could be a [CompoundStatement] or
    /// an [IfStatement].
    pub alternative: Option<Box<Statement>>,
}

/// Represents a return statement that may or may not return a value
#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStatement {
    /// Optional expression to be returned
    pub value: Option<Expression>,
    /// The span in the source code where this statement is located.
    pub span: SourceSpan,
}

/// Represents a group of statements that can be executed sequentially
#[derive(Debug, Clone, PartialEq)]
pub struct CompoundStatement {
    /// A vector of statements to be executed in order
    pub statements: Vec<Statement>,
}

/// Represents an expression in a source code, consisting of its type (`kind`)
/// and the span of source code it covers (`span`).
#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    /// The specific kind of expression, such as an integer literal.
    pub kind: ExpressionKind,
    /// The span in the source code where this expression is located.
    pub span: SourceSpan,
}

/// Enum representing the different kinds of expressions that can occur in the source code.
#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    /// Represents an integer literal with its value.
    Integer(u64),
    /// Represents a string literal with its value.
    String(String),
    /// Represents an identifier literal.
    Identifier(Identifier),
    /// Represents a binary expression.
    Binary(BinaryExpression),
    /// Represents a unary expression.
    Unary(UnaryExpression),
    /// Represents a function call expression.
    Call(CallExpression),
}

/// Represents a binary expression in the syntax tree.
///
/// A binary expression consists of two sub-expressions (`left` and `right`)
/// and a binary operator (`op`) that connects them. This structure is used to
/// represent expressions like `a + b`, `x * y`, or `p && q`.
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    /// The left-hand side of the binary expression.
    pub left: Box<Expression>,
    /// The binary operator (e.g., `+`, `-`, `*`, `/`, etc.).
    pub op: BinaryOperator,
    /// The right-hand side of the binary expression.
    pub right: Box<Expression>,
}

/// Represents a binary operator in an expression.
///
/// This enumeration defines the various binary operators that can be used in expressions,
/// including arithmetic operations and bitwise operations.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOperator {
    /// The assign operator (`=`), used to assign values.
    Assign,
    /// The addition operator (`+`), used to sum two values.
    Add,
    /// The subtraction operator (`-`), used to compute the difference between two values.
    Subtract,
    /// The multiplication operator (`*`), used to compute the product of two values.
    Multiply,
    /// The division operator (`/`), used to compute the quotient of two values.
    Divide,

    /// The bitwise AND operator (`&`), performs a bitwise AND operation on two values.
    BitwiseAnd,
    /// The bitwise OR operator (`|`), performs a bitwise OR operation on two values.
    BitwiseOr,
    /// The bitwise XOR operator (`^`), performs a bitwise XOR operation on two values.
    BitwiseXor,

    /// The equality operator (`==`), used to check if two values are equal.
    Equal,
    /// The inequality operator (`!=`), used to check if two values are not equal.
    Unequal,
    /// The less-than operator (`<`), used to check if the left value is less than the right value.
    LessThan,
    /// The less-than-or-equal-to operator (`<=`), used to check if the left value is less than or equal to the right value.
    LessEqual,
    /// The greater-than operator (`>`), used to check if the left value is greater than the right value.
    GreaterThan,
    /// The greater-than-or-equal-to operator (`>=`), used to check if the left value is greater than or equal to the right value.
    GreaterEqual,

    /// The logical or operator (`or`), performs a logical OR operation on two values.
    LogicalOr,
    /// The logical and operator (`and`), performs a logical AND operation on two values.
    LogicalAnd,
}

/// Represents a unary expression in the syntax tree.
///
/// A unary expression consists of a single sub-expression (`expr`) and a unary operator (`op`)
/// that acts upon it. This structure is used to represent expressions like `-x` or `!flag`.
#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression {
    /// The operand of the unary expression.
    pub expr: Box<Expression>,
    /// The unary operator (e.g., negation `-`, logical NOT `!`).
    pub op: UnaryOperator,
}

/// Represents a unary operator in an expression.
///
/// This enumeration defines the various unary operators that can be applied to an operand,
/// such as negation or logical NOT.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOperator {
    /// Represents the arithmetic negation operator (`-`).
    Negate,
    /// Represents the logical NOT operator (`!`).
    Not,
}

/// Represents a function call expression.
///
/// A `CallExpression` consists of a function being invoked and a list of arguments
/// passed to the function.
#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    /// The expression representing the function being called.
    /// This can be an identifier, a member access, or any valid callable expression.
    pub function: Box<Expression>,
    /// A list of expressions representing the arguments passed to the function during the call.
    pub arguments: Vec<Expression>,
}
