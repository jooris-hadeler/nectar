use std::io::{self, Write};

use super::{
    ArrayType, BinaryExpression, CallExpression, CompoundStatement, Declaration, Expression,
    ExpressionKind, FunctionDeclaration, FunctionParameter, IfStatement, LetStatement, Module,
    ReturnStatement, Statement, Type, TypeKind, UnaryExpression,
};

/// A utility for visualizing a syntax tree as a Graphviz graph.
///
/// This struct is responsible for traversing a syntax tree and writing
/// its structure to a Graphviz-compatible DOT file. This visualization
/// can be used for debugging or analysis of the syntax tree.
///
/// # Generics
/// - `'a`: The lifetime of the writer.
/// - `W`: The writer type implementing the `Write` trait.
pub struct SyntaxTreeVisualizer<'a, W: Write> {
    /// The output buffer where the graph data will be written.
    buffer: &'a mut W,
    /// A counter to assign unique IDs to graph nodes.
    counter: usize,
}

impl<'a, W: Write> SyntaxTreeVisualizer<'a, W> {
    /// Creates a new `SyntaxTreeVisualizer` with the given buffer.
    ///
    /// # Parameters
    /// - `buffer`: A mutable reference to a writer where the graph will be written.
    ///
    /// # Returns
    /// A new instance of `SyntaxTreeVisualizer`.
    pub fn new(buffer: &'a mut W) -> Self {
        Self { buffer, counter: 0 }
    }

    /// Writes the visualization of a syntax tree for the given module to the buffer.
    ///
    /// # Parameters
    /// - `module`: The root module of the syntax tree to visualize.
    ///
    /// # Returns
    /// - `Ok(())` if the visualization is successfully written.
    /// - `Err(io::Error)` if an I/O error occurs.
    pub fn write(&mut self, module: &Module) -> io::Result<()> {
        writeln!(self.buffer, "digraph SyntaxTree {{")?;
        writeln!(self.buffer, "    node [shape=box];")?;
        self.visualize_module(module)?;
        writeln!(self.buffer, "}}")?;
        Ok(())
    }

    /// Generates the next unique ID for a graph node.
    ///
    /// # Returns
    /// A unique `usize` ID.
    fn next_id(&mut self) -> usize {
        let id = self.counter;
        self.counter += 1;
        id
    }

    /// Writes a node to the graph with an optional parent edge.
    ///
    /// # Parameters
    /// - `label`: The label to display on the node.
    /// - `parent_id`: An optional ID of the parent node to connect this node to.
    ///
    /// # Returns
    /// - The unique ID of the newly created node.
    fn write_node<S: ToString>(&mut self, label: S, parent_id: Option<usize>) -> io::Result<usize> {
        let node_id = self.next_id();
        let label = label.to_string();

        writeln!(self.buffer, "    node_{node_id} [label=\"{label}\"]")?;
        if let Some(parent) = parent_id {
            writeln!(self.buffer, "    node_{parent} -> node_{node_id}")?;
        }

        Ok(node_id)
    }

    /// Visualizes a module and its declarations.
    ///
    /// # Parameters
    /// - `module`: The module to visualize.
    ///
    /// # Returns
    /// `Ok(())` if the module is successfully visualized, or an `io::Error` on failure.
    fn visualize_module(&mut self, module: &Module) -> io::Result<()> {
        let module_id = self.write_node(format!("Module: {}", module.name), None)?;

        for declaration in module.declarations.iter() {
            match declaration {
                Declaration::Function(function_declaration) => {
                    self.visualize_function_declaration(module_id, function_declaration)?;
                }
            }
        }

        Ok(())
    }

    /// Visualizes a function declaration and its components.
    ///
    /// # Parameters
    /// - `module_id`: The ID of the parent module node.
    /// - `function_declaration`: The function declaration to visualize.
    ///
    /// # Returns
    /// `Ok(())` if the function is successfully visualized, or an `io::Error` on failure.
    fn visualize_function_declaration(
        &mut self,
        module_id: usize,
        function_declaration: &FunctionDeclaration,
    ) -> io::Result<()> {
        let function_id = self.write_node(
            format!("FunctionDeclaration: {}", function_declaration.name.name),
            Some(module_id),
        )?;

        for function_parameter in function_declaration.parameters.iter() {
            self.visualize_function_parameter(function_id, function_parameter)?;
        }

        if let Some(return_type) = &function_declaration.return_type {
            self.visualize_type(function_id, return_type)?;
        }

        if let Some(body) = &function_declaration.body {
            self.visualize_compound_statement(function_id, body)?;
        }

        Ok(())
    }

    /// Visualizes a function parameter and its type.
    ///
    /// # Parameters
    /// - `function_id`: The ID of the parent function node.
    /// - `function_parameter`: The function parameter to visualize.
    ///
    /// # Returns
    /// `Ok(())` if the parameter is successfully visualized, or an `io::Error` on failure.
    fn visualize_function_parameter(
        &mut self,
        function_id: usize,
        function_parameter: &FunctionParameter,
    ) -> io::Result<()> {
        let parameter_id = self.write_node(
            format!("FunctionParameter: {}", function_parameter.name.name),
            Some(function_id),
        )?;

        self.visualize_type(parameter_id, &function_parameter.type_)
    }

    /// Visualizes a type and its details.
    ///
    /// # Parameters
    /// - `parent_id`: The ID of the parent node.
    /// - `type_`: The type to visualize.
    ///
    /// # Returns
    /// `Ok(())` if the type is successfully visualized, or an `io::Error` on failure.
    fn visualize_type(&mut self, parent_id: usize, type_: &Type) -> io::Result<()> {
        match &type_.kind {
            TypeKind::Builtin(builtin_kind) => {
                self.write_node(format!("BuiltinType: {:?}", builtin_kind), Some(parent_id))?;
            }
            TypeKind::Pointer(inner_type) => {
                let pointer_id = self.write_node("PointerType", Some(parent_id))?;
                self.visualize_type(pointer_id, inner_type)?
            }
            TypeKind::Array(array_type) => self.visualize_array_type(parent_id, array_type)?,
            TypeKind::Named(identifier) => {
                self.write_node(format!("NamedType: {}", identifier.name), Some(parent_id))?;
            }
        }

        Ok(())
    }

    /// Visualizes an array type and its components.
    ///
    /// # Parameters
    /// - `parent_id`: The ID of the parent node.
    /// - `type_`: The array type to visualize.
    ///
    /// # Returns
    /// `Ok(())` if the array type is successfully visualized, or an `io::Error` on failure.
    fn visualize_array_type(&mut self, parent_id: usize, type_: &ArrayType) -> io::Result<()> {
        let type_id = self.write_node("ArrayType", Some(parent_id))?;

        self.visualize_type(type_id, &type_.inner_type)?;
        self.visualize_expression(type_id, &type_.size)?;

        Ok(())
    }

    /// Visualizes a statement and its specific kind.
    ///
    /// # Parameters
    /// - `parent_id`: The ID of the parent node.
    /// - `statement`: The statement to visualize.
    ///
    /// # Returns
    /// `Ok(())` if the statement is successfully visualized, or an `io::Error` on failure.
    fn visualize_statement(&mut self, parent_id: usize, statement: &Statement) -> io::Result<()> {
        match statement {
            Statement::Let(let_statement) => self.visualize_let_statement(parent_id, let_statement),
            Statement::If(if_statement) => self.visualize_if_statement(parent_id, if_statement),
            Statement::Return(return_statement) => {
                self.visualize_return_statement(parent_id, return_statement)
            }
            Statement::Compound(compound_statement) => {
                self.visualize_compound_statement(parent_id, compound_statement)
            }
            Statement::Expression(expression) => self.visualize_expression(parent_id, expression),
        }
    }

    /// Visualizes a `let` statement, including its name, type, and value.
    ///
    /// # Parameters
    /// - `parent_id`: The ID of the parent node.
    /// - `let_statement`: The `let` statement to visualize.
    ///
    /// # Returns
    /// `Ok(())` if the statement is successfully visualized, or an `io::Error` on failure.
    fn visualize_let_statement(
        &mut self,
        parent_id: usize,
        let_statement: &LetStatement,
    ) -> io::Result<()> {
        let let_id = self.write_node(
            format!("LetStatement: {}", let_statement.name.name),
            Some(parent_id),
        )?;

        if let Some(type_) = &let_statement.type_ {
            self.visualize_type(let_id, type_)?;
        }

        self.visualize_expression(let_id, &let_statement.value)?;

        Ok(())
    }

    /// Visualizes an `if` statement, including its condition, consequence, and optional alternative.
    ///
    /// # Parameters
    /// - `parent_id`: The ID of the parent node.
    /// - `if_statement`: The `if` statement to visualize.
    ///
    /// # Returns
    /// `Ok(())` if the statement is successfully visualized, or an `io::Error` on failure.
    fn visualize_if_statement(
        &mut self,
        parent_id: usize,
        if_statement: &IfStatement,
    ) -> io::Result<()> {
        let if_id = self.write_node("IfStatement", Some(parent_id))?;

        self.visualize_expression(if_id, &if_statement.condition)?;

        let consequence_id = self.write_node("Consequence", Some(if_id))?;
        self.visualize_compound_statement(consequence_id, &if_statement.consequence)?;

        if let Some(alternative) = &if_statement.alternative {
            let alternative_id = self.write_node("Alternative", Some(if_id))?;
            self.visualize_statement(alternative_id, alternative)?;
        }

        Ok(())
    }

    /// Visualizes a `return` statement and its optional value.
    ///
    /// # Parameters
    /// - `parent_id`: The ID of the parent node.
    /// - `return_statement`: The `return` statement to visualize.
    ///
    /// # Returns
    /// `Ok(())` if the statement is successfully visualized, or an `io::Error` on failure.
    fn visualize_return_statement(
        &mut self,
        parent_id: usize,
        return_statement: &ReturnStatement,
    ) -> io::Result<()> {
        let return_id = self.write_node("Return", Some(parent_id))?;

        if let Some(value) = &return_statement.value {
            self.visualize_expression(return_id, value)?;
        }

        Ok(())
    }

    /// Visualizes a compound statement, which is a sequence of statements.
    ///
    /// # Parameters
    /// - `parent_id`: The ID of the parent node.
    /// - `compound_statement`: The compound statement to visualize.
    ///
    /// # Returns
    /// `Ok(())` if the statement is successfully visualized, or an `io::Error` on failure.
    fn visualize_compound_statement(
        &mut self,
        parent_id: usize,
        compound_statement: &CompoundStatement,
    ) -> io::Result<()> {
        let compound_id = self.write_node("CompoundStatement", Some(parent_id))?;

        for statement in compound_statement.statements.iter() {
            self.visualize_statement(compound_id, statement)?;
        }

        Ok(())
    }

    /// Visualizes an expression and its specific kind.
    ///
    /// # Parameters
    /// - `parent_id`: The ID of the parent node.
    /// - `expression`: The expression to visualize.
    ///
    /// # Returns
    /// `Ok(())` if the expression is successfully visualized, or an `io::Error` on failure.
    fn visualize_expression(
        &mut self,
        parent_id: usize,
        expression: &Expression,
    ) -> io::Result<()> {
        match &expression.kind {
            ExpressionKind::Integer(value) => {
                self.write_node(format!("Integer: {value}"), Some(parent_id))?;
            }
            ExpressionKind::Identifier(identifier) => {
                self.write_node(format!("Identifier: {}", identifier.name), Some(parent_id))?;
            }
            ExpressionKind::Binary(binary_expression) => {
                self.visualize_binary_expression(parent_id, binary_expression)?;
            }
            ExpressionKind::Unary(unary_expression) => {
                self.visualize_unary_expression(parent_id, unary_expression)?;
            }
            ExpressionKind::Call(call_expression) => {
                self.visualize_call_expression(parent_id, call_expression)?;
            }
        }

        Ok(())
    }

    /// Visualizes a binary expression, including its left operand, operator, and right operand.
    ///
    /// # Parameters
    /// - `parent_id`: The ID of the parent node.
    /// - `binary_expression`: The binary expression to visualize.
    ///
    /// # Returns
    /// `Ok(())` if the expression is successfully visualized, or an `io::Error` on failure.
    fn visualize_binary_expression(
        &mut self,
        parent_id: usize,
        binary_expression: &BinaryExpression,
    ) -> io::Result<()> {
        let binary_id = self.write_node("BinaryExpression", Some(parent_id))?;

        self.visualize_expression(binary_id, &binary_expression.left)?;
        self.write_node(format!("Op: {:?}", binary_expression.op), Some(binary_id))?;
        self.visualize_expression(binary_id, &binary_expression.right)?;

        Ok(())
    }

    /// Visualizes a unary expression, including its operator and operand.
    ///
    /// # Parameters
    /// - `parent_id`: The ID of the parent node.
    /// - `unary_expression`: The unary expression to visualize.
    ///
    /// # Returns
    /// `Ok(())` if the expression is successfully visualized, or an `io::Error` on failure.
    fn visualize_unary_expression(
        &mut self,
        parent_id: usize,
        unary_expression: &UnaryExpression,
    ) -> io::Result<()> {
        let unary_id = self.write_node("UnaryExpression", Some(parent_id))?;

        self.visualize_expression(parent_id, &unary_expression.expr)?;
        self.write_node(format!("Op: {:?}", unary_expression.op), Some(unary_id))?;

        Ok(())
    }

    /// Visualizes a call expression, including the function being called and its arguments.
    ///
    /// # Parameters
    /// - `parent_id`: The ID of the parent node.
    /// - `call_expression`: The call expression to visualize.
    ///
    /// # Returns
    /// `Ok(())` if the expression is successfully visualized, or an `io::Error` on failure.
    fn visualize_call_expression(
        &mut self,
        parent_id: usize,
        call_expression: &CallExpression,
    ) -> io::Result<()> {
        let call_id = self.write_node("CallExpressions", Some(parent_id))?;

        self.visualize_expression(call_id, &call_expression.function)?;

        let arguments_id = self.write_node("CallArguments", Some(call_id))?;

        for argument in call_expression.arguments.iter() {
            self.visualize_expression(arguments_id, argument)?;
        }

        Ok(())
    }
}
