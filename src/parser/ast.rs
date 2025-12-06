use crate::span::Span;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Program(pub Vec<Expression>);

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Expression {
    pub span: Span,
    pub kind: ExpressionKind,
}
impl Expression {
    pub const fn new(kind: ExpressionKind, span: Span) -> Self {
        Self { span, kind }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum ExpressionKind {
    BoolLiteral(bool),
    IntLiteral(isize),
    Ident {
        name: String,
        namespace: Option<String>,
    },
    Block(Vec<Expression>),
    Conditional {
        condition: Box<Expression>,
        true_expression: Box<Expression>,
        false_expression: Box<Expression>,
    },
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
        body: Box<Expression>,
    },
    FunctionCall {
        name: String,
        namespace: Option<String>,
        arguments: Vec<FunctionArgument>,
    },
    Use {
        segments: Vec<String>,
        tail: Vec<UseTailItem>,
    },
}

impl ExpressionKind {
    pub fn is_const(&self) -> bool {
        #[allow(clippy::enum_glob_use)]
        use crate::parser::ast::ExpressionKind::*;

        match self {
            Block(expressions) => expressions.iter().all(|expr| expr.kind.is_const()),
            Prefix { expression, .. } => expression.kind.is_const(),
            Infix { left, right, .. } => left.kind.is_const() && right.kind.is_const(),
            VariableDecl { value, mutable, .. } => !mutable && value.kind.is_const(),
            FunctionCall { .. } => false,
            Conditional {
                condition,
                true_expression,
                false_expression,
            } => {
                condition.kind.is_const()
                    && true_expression.kind.is_const()
                    && false_expression.kind.is_const()
            }
            BoolLiteral(_) | IntLiteral(_) | Ident { .. } | Function { .. } | Use { .. } => true,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct UseTailItem {
    pub name: String,
    pub alias: Option<String>,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum PrefixOperator {
    LogicalNot,
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

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FunctionArgument {
    pub label: Option<String>,
    pub value: Expression,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FunctionParameter {
    pub modifier: ParameterModifier,
    pub labeled_at_callsite: bool,
    pub internal_name: String,
    pub external_name: Option<String>,
    pub ty: Type,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum ParameterModifier {
    Read,
    Mut,
    Take,
}

#[derive(PartialEq, Eq, Debug)]
pub struct VariableDecl {
    pub name: String,
    pub value: Expression,
    pub mutable: bool,
    pub ty: Option<Type>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    Ident(String),
}

#[derive(Ord, Eq, PartialEq, PartialOrd, Debug)]
pub enum Precedence {
    Lowest,
    Equality,   // == or !=
    Comparison, // <, <=, >, >=
    Sum,        // + or -
    Product,    // * or /
    Group,      // ( )
    Prefix,     // -X or !X
}
