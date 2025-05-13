use crate::span::Span;

#[derive(PartialEq, Eq, Debug)]
pub struct Program(pub Vec<ExpressionStatement>);

#[derive(PartialEq, Eq, Debug)]
pub struct ExpressionStatement {
    pub expr: Expression,
    pub discarded: bool,
}

#[derive(PartialEq, Eq, Debug)]
pub struct Expression {
    pub span: Span,
    pub kind: ExpressionKind,
}
impl Expression {
    pub fn new(kind: ExpressionKind, span: Span) -> Self {
        Self { span, kind }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum ExpressionKind {
    Unit,
    BoolLiteral(bool),
    IntLiteral(isize),
    Ident(String),

    Prefix {
        operator: PrefixOperator,
        expression: Box<Expression>,
    },
    Infix {
        left: Box<Expression>,
        operator: InfixOperator,
        right: Box<Expression>,
    },

    VariableDecl {
        name: String,
        value: Box<Expression>,
        mutable: bool,
        ty: Option<Type>,
    },

    Function {
        parameters: Vec<FunctionParameter>,
        return_type: Option<Type>,
        body: Vec<ExpressionStatement>,
    },

    FunctionCall {
        name: String,
        arguments: Vec<FunctionArgument>,
    },
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum PrefixOperator {
    LogicalNot,
    Negative,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum InfixOperator {
    Plus,
    Minus,
    Divide,
    Multiply,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
}

#[derive(PartialEq, Eq, Debug)]
pub struct FunctionArgument {
    pub label: Option<String>,
    pub value: Expression,
}

#[derive(PartialEq, Eq, Debug)]
pub enum FunctionParameter {
    LabeledAtCallsite {
        internal_name: String,
        external_name: Option<String>,
        ty: Type,
    },
    UnlabeledAtCallsite {
        name: String,
        ty: Type,
    },
}

#[derive(PartialEq, Eq, Debug)]
pub struct VariableDecl {
    pub name: String,
    pub value: Expression,
    pub mutable: bool,
    pub ty: Option<Type>,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Type {
    Ident(String),
}

#[derive(Ord, Eq, PartialEq, PartialOrd, Debug)]
pub enum Precedence {
    Lowest,
    Comparison,
    Sum,     // + or -
    Product, // * or /
    Group,   // ( )
    Prefix,  // -X or !X
}
