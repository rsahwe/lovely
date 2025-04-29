#[derive(PartialEq, Eq, Debug)]
pub struct Program(pub Vec<Statement>);

#[derive(PartialEq, Eq, Debug)]
pub enum Statement {
    Expression(Expression),
    VariableDecl(VariableDecl),
    FunctionDecl(FunctionDecl),
}

#[derive(PartialEq, Eq, Debug)]
pub enum Expression {
    IntLiteral(isize),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Ident(String),
    FunctionCall(FunctionCall),
}

#[derive(PartialEq, Eq, Debug)]
pub struct InfixExpression {
    pub operator: InfixOperator,
    pub lh_expression: Box<Expression>,
    pub rh_expression: Box<Expression>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct PrefixExpression {
    pub operator: PrefixOperator,
    pub expression: Box<Expression>,
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
pub struct FunctionCall {
    pub name: String,
    pub arguments: Vec<Argument>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct Argument {
    pub label: Option<String>,
    pub value: Expression,
}

#[derive(PartialEq, Eq, Debug)]
pub struct VariableDecl {
    pub name: String,
    pub value: Expression,
    pub mutable: bool,
    pub ty: Type,
}

#[derive(PartialEq, Eq, Debug)]
pub struct FunctionDecl {
    pub name: String,
    pub parameters: Vec<FunctionParameter>,
    pub return_type: Type,
    pub body: Vec<Statement>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct FunctionParameter {
    pub external_name: Option<String>,
    pub internal_name: String,
    pub labeled: bool,
    pub ty: Type,
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
