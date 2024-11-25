use miette::SourceSpan;

/// Represents a module in the source code.
///
/// A module is a top-level construct that typically encapsulates a collection
/// of related declarations, such as functions, variables, types, or other
/// structural components of the language.
#[derive(Debug)]
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
#[derive(Debug)]
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
#[derive(Debug)]
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
#[derive(Debug)]
pub struct FunctionDeclaration {
    /// The name of the function as an identifier.
    pub name: Identifier,
    /// The list of parameters the function accepts.
    pub parameters: Vec<FunctionParameter>,
    /// The return type of the function, if specified.
    pub return_type: Option<Type>,
    /// The body of the function, if defined.
    pub body: Option<()>,
}

/// Represents a single parameter in a function declaration.
///
/// Function parameters define the inputs that a function accepts. Each parameter
/// has a name and optionally a type.
#[derive(Debug)]
pub struct FunctionParameter {
    /// The name of the parameter as an identifier.
    pub name: Identifier,
    /// The type of the parameter, if specified.
    pub type_: Type,
}

/// Represents a type in the language, containing the specific type variant and its location in source code.
#[derive(Debug)]
pub struct Type {
    /// The specific kind/variant of this type.
    pub kind: TypeKind,
    /// The location of this type in the source code.
    pub span: SourceSpan,
}

/// Represents all possible type variants in the language.
#[derive(Debug)]
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
#[derive(Debug)]
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
#[derive(Debug)]
pub struct ArrayType {
    /// The type of elements stored in the array.
    pub inner_type: Box<Type>,
    /// The fixed size of the array.
    pub size: Expression,
}

/// Represents an expression in a source code, consisting of its type (`kind`)
/// and the span of source code it covers (`span`).
#[derive(Debug)]
pub struct Expression {
    /// The specific kind of expression, such as an integer literal.
    pub kind: ExpressionKind,
    /// The span in the source code where this expression is located.
    pub span: SourceSpan,
}

/// Enum representing the different kinds of expressions that can occur in the source code.
#[derive(Debug)]
pub enum ExpressionKind {
    /// Represents an integer literal with its value.
    Integer(u64),
}
