#[derive(PartialEq, Eq, Debug)]
pub struct Program(pub Vec<ExpressionStatement>);

#[derive(PartialEq, Eq, Debug)]
pub struct ExpressionStatement {
    pub expr: Expression,
    pub discarded: bool,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Expression {
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
        name: String,
        parameters: Vec<FunctionParameter>,
        return_type: Type,
        body: Vec<ExpressionStatement>,
    },

    FunctionCall {
        name: String,
        arguments: Vec<Argument>,
    },
}

#[derive(PartialEq, Eq, Debug)]
pub enum PrefixOperator {
    Not,
    Positive,
    Negative,
}

#[derive(PartialEq, Eq, Debug)]
pub enum InfixOperator {
    Plus,
    Minus,
    Divide,
    Multiply,
    Exponent,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
}

#[derive(PartialEq, Eq, Debug)]
pub struct Argument {
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

#[derive(Ord, Eq, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Sum,     // + or -
    Product, // * or /
    Group,   // ( )
}
