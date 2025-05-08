#![allow(dead_code)]

use crate::lexer::tokens::{
    Token,
    TokenKind::{self, *},
};
use ast::{
    Expression, ExpressionStatement, FunctionArgument, FunctionParameter, InfixOperator,
    Precedence, Program, Type,
};

pub mod ast;

type PrefixParseFn = fn(&mut Parser) -> Result<Expression, Error>;

#[derive(Debug, PartialEq, Eq)]
enum Error {
    NoToken,
    NoPrefixParseFn(TokenKind),
    Expected(String),
    Syntax(String),
}

impl Error {
    pub fn expected(s: &str) -> Self {
        Self::Expected(s.to_string())
    }

    pub fn syntax_err(s: &str) -> Self {
        Self::Syntax(format!("syntax error: {s}"))
    }
}

struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    pub fn parse(&mut self) -> Result<Program, Error> {
        let mut stmts = vec![];
        while self.cur_token() != Eof {
            stmts.push(self.parse_expression_statement()?);
        }
        Ok(Program(stmts))
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStatement, Error> {
        let expr = self.parse_expression(Precedence::Lowest)?;
        let has_semicolon = self.check_semicolon();
        if has_semicolon {
            self.advance()
        }

        Ok(ExpressionStatement {
            expr,
            discarded: has_semicolon,
        })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, Error> {
        let mut expr = self.prefix_parse_fn()?(self)?;

        while self.cur_precedence()? > precedence {
            match self.cur_token() {
                IntLiteral(_) => return Err(Error::syntax_err("consecutive ints")),
                Eof => return Ok(expr),
                Plus => {
                    expr =
                        self.parse_infix_expression(expr, InfixOperator::Plus, Precedence::Sum)?;
                }
                Minus => {
                    expr =
                        self.parse_infix_expression(expr, InfixOperator::Minus, Precedence::Sum)?;
                }
                Asterisk => {
                    expr = self.parse_infix_expression(
                        expr,
                        InfixOperator::Multiply,
                        Precedence::Product,
                    )?;
                }
                Slash => {
                    expr = self.parse_infix_expression(
                        expr,
                        InfixOperator::Divide,
                        Precedence::Product,
                    )?;
                }
                tok => return Err(Error::syntax_err(&format!("invalid operator: {tok}"))),
            }
        }

        Ok(expr)
    }

    fn prefix_parse_fn(&mut self) -> Result<PrefixParseFn, Error> {
        let cur_token = self.cur_token();
        match cur_token {
            IntLiteral(_) => Ok(Self::parse_int_literal),
            True | False => Ok(Self::parse_bool_literal),
            Unit => Ok(Self::parse_unit),
            LParen => Ok(Self::parse_grouped_expression),
            Identifier(_) => match self.peek_token() {
                Colon => Ok(Self::parse_variable_declaration),
                LParen => Ok(Self::parse_function_call),
                _ => Ok(Self::parse_variable_ident),
            },
            Fun => Ok(Self::parse_function_expression),
            _ => Err(Error::NoPrefixParseFn(cur_token.clone())),
        }
    }

    fn parse_infix_expression(
        &mut self,
        lhs: Expression,
        operator: InfixOperator,
        precedence: Precedence,
    ) -> Result<Expression, Error> {
        self.advance();
        let rhs = self.parse_expression(precedence)?;
        Ok(Expression::Infix {
            left: Box::new(lhs),
            operator,
            right: Box::new(rhs),
        })
    }

    fn parse_int_literal(&mut self) -> Result<Expression, Error> {
        Ok(Expression::IntLiteral(self.expect_int()?))
    }

    fn parse_bool_literal(&mut self) -> Result<Expression, Error> {
        match self.cur_token() {
            True => {
                self.expect_token(True)?;
                Ok(Expression::BoolLiteral(true))
            }
            False => {
                self.expect_token(False)?;
                Ok(Expression::BoolLiteral(false))
            }
            _ => Err(Error::expected("true or false")),
        }
    }

    fn parse_unit(&mut self) -> Result<Expression, Error> {
        self.expect_token(Unit)?;
        Ok(Expression::Unit)
    }

    fn parse_variable_ident(&mut self) -> Result<Expression, Error> {
        Ok(Expression::Ident(self.expect_ident()?))
    }

    fn parse_type(&mut self) -> Result<Type, Error> {
        Ok(Type::Ident(self.expect_ident()?))
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, Error> {
        self.advance();
        let expr = self.parse_expression(Precedence::Lowest)?;
        match self.cur_token() {
            RParen => {
                self.advance();
                Ok(expr)
            }
            _ => Err(Error::expected(")")),
        }
    }

    fn parse_function_call(&mut self) -> Result<Expression, Error> {
        let name = self.expect_ident()?;
        self.expect_token(LParen)?;

        let mut arguments = vec![];

        while self.cur_token() != RParen {
            arguments.push(self.parse_function_argument()?);
            if self.cur_token() == Comma {
                self.expect_token(Comma)?;
                continue;
            } else {
                break;
            }
        }

        self.expect_token(RParen)?;

        Ok(Expression::FunctionCall { name, arguments })
    }

    fn parse_function_argument(&mut self) -> Result<FunctionArgument, Error> {
        let mut label = None;
        let value: Expression;

        if let Identifier(name) = self.cur_token() {
            self.expect_ident()?;
            if self.cur_token() == Colon {
                self.expect_token(Colon)?;
                label = Some(name);
                value = self.parse_expression(Precedence::Lowest)?;
            } else {
                value = Expression::Ident(name);
            }
        } else {
            value = self.parse_expression(Precedence::Lowest)?;
        }

        Ok(FunctionArgument { label, value })
    }

    fn parse_variable_declaration(&mut self) -> Result<Expression, Error> {
        let name = self.expect_ident()?;
        self.expect_token(Colon)?;

        let mut ty = None;
        if let Identifier(_) = self.cur_token() {
            ty = Some(self.parse_type()?);
        }

        #[allow(clippy::needless_late_init)]
        let value: Expression;
        let mutable: bool;

        match self.cur_token() {
            Colon => {
                self.expect_token(Colon)?;
                mutable = false;
                value = self.parse_expression(Precedence::Lowest)?;
            }
            Equal => {
                self.expect_token(Equal)?;
                mutable = true;
                value = self.parse_expression(Precedence::Lowest)?;
            }
            _ => return Err(Error::expected(": or =")),
        }

        Ok(Expression::VariableDecl {
            name,
            value: Box::new(value),
            mutable,
            ty,
        })
    }

    fn parse_function_expression(&mut self) -> Result<Expression, Error> {
        self.expect_token(Fun)?;
        self.expect_token(LParen)?;

        let mut parameters = vec![];

        while self.cur_token() != RParen {
            parameters.push(self.parse_function_parameter()?);
            if self.cur_token() == Comma {
                self.expect_token(Comma)?;
                continue;
            } else {
                break;
            }
        }

        self.expect_token(RParen)?;

        let mut return_type = None;

        if let Identifier(_) = self.cur_token() {
            return_type = Some(self.parse_type()?);
        }

        self.expect_token(LBrace)?;

        let mut body = vec![];

        while self.cur_token() != RBrace {
            body.push(self.parse_expression_statement()?);
        }

        self.expect_token(RBrace)?;

        Ok(Expression::Function {
            parameters,
            return_type,
            body,
        })
    }

    fn parse_function_parameter(&mut self) -> Result<FunctionParameter, Error> {
        match self.cur_token() {
            Tilde => {
                self.expect_token(Tilde)?;
                let name = self.expect_ident()?;
                self.expect_token(Colon)?;
                let ty = self.parse_type()?;
                Ok(FunctionParameter::UnlabeledAtCallsite { name, ty })
            }
            Identifier(_) => {
                let first = self.expect_ident()?;
                let mut second = None;
                if let Identifier(name) = self.cur_token() {
                    self.expect_ident()?;
                    second = Some(name);
                }
                self.expect_token(Colon)?;
                let ty = self.parse_type()?;
                Ok(FunctionParameter::LabeledAtCallsite {
                    internal_name: if let Some(second_ident) = &second {
                        second_ident.to_string()
                    } else {
                        first.clone()
                    },
                    external_name: if second.is_some() { Some(first) } else { None },
                    ty,
                })
            }
            _ => Err(Error::expected("parameter name")),
        }
    }

    fn check_semicolon(&mut self) -> bool {
        self.cur_token() == Semicolon
    }

    fn expect_token(&mut self, kind: TokenKind) -> Result<(), Error> {
        let tok = self.cur_token();
        if tok != kind {
            Err(Error::syntax_err(&format!("unexpected token: {}", tok)))
        } else {
            self.advance();
            Ok(())
        }
    }

    fn expect_int(&mut self) -> Result<isize, Error> {
        if let IntLiteral(int) = self.cur_token() {
            self.advance();
            Ok(int)
        } else {
            Err(Error::expected("int literal"))
        }
    }

    fn expect_ident(&mut self) -> Result<String, Error> {
        if let Identifier(str) = self.cur_token() {
            self.advance();
            Ok(str)
        } else {
            Err(Error::expected("identifier"))
        }
    }

    fn cur_precedence(&self) -> Result<Precedence, Error> {
        Ok(match self.cur_token() {
            Plus => Precedence::Sum,
            Minus => Precedence::Sum,
            Asterisk => Precedence::Product,
            Slash => Precedence::Product,
            LParen => Precedence::Group,
            _ => Precedence::Lowest,
        })
    }

    fn cur_token(&self) -> TokenKind {
        self.tokens
            .get(self.position)
            .map_or(Eof, |t| t.kind.clone())
    }

    fn peek_token(&self) -> TokenKind {
        self.tokens
            .get(self.position + 1)
            .map_or(Eof, |t| t.kind.clone())
    }

    fn advance(&mut self) {
        self.position += 1;
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::{Lexer, tokens::Token},
        parser::{
            Parser,
            ast::{
                Expression, ExpressionStatement, FunctionArgument, FunctionParameter,
                InfixOperator, Program, Type,
            },
        },
    };
    use pretty_assertions::assert_eq;

    fn expect_ast(input: &str, ast: Program) {
        let lexer = Lexer::new(input);
        let tokens: Vec<Token> = lexer.collect();
        let mut parser = Parser::new(tokens);
        let parsed = parser.parse();
        assert_eq!(parsed, Ok(ast));
    }

    #[test]
    fn single_value() {
        expect_ast(
            "122",
            Program(vec![ExpressionStatement {
                expr: Expression::IntLiteral(122),
                discarded: false,
            }]),
        );

        expect_ast(
            "122;",
            Program(vec![ExpressionStatement {
                expr: Expression::IntLiteral(122),
                discarded: true,
            }]),
        );

        expect_ast(
            "foo;",
            Program(vec![ExpressionStatement {
                expr: Expression::Ident("foo".to_string()),
                discarded: true,
            }]),
        );

        expect_ast(
            "unit",
            Program(vec![ExpressionStatement {
                expr: Expression::Unit,
                discarded: false,
            }]),
        );

        expect_ast(
            "true; false",
            Program(vec![
                ExpressionStatement {
                    expr: Expression::BoolLiteral(true),
                    discarded: true,
                },
                ExpressionStatement {
                    expr: Expression::BoolLiteral(false),
                    discarded: false,
                },
            ]),
        );
    }

    #[test]
    fn mathematical_expressions() {
        expect_ast(
            "1 + 2 * 3",
            Program(vec![ExpressionStatement {
                expr: Expression::Infix {
                    left: Box::new(Expression::IntLiteral(1)),
                    operator: InfixOperator::Plus,
                    right: Box::new(Expression::Infix {
                        left: Box::new(Expression::IntLiteral(2)),
                        operator: InfixOperator::Multiply,
                        right: Box::new(Expression::IntLiteral(3)),
                    }),
                },
                discarded: false,
            }]),
        );

        expect_ast(
            "6 - 3 - 2",
            Program(vec![ExpressionStatement {
                expr: Expression::Infix {
                    left: Box::new(Expression::Infix {
                        left: Box::new(Expression::IntLiteral(6)),
                        operator: InfixOperator::Minus,
                        right: Box::new(Expression::IntLiteral(3)),
                    }),
                    operator: InfixOperator::Minus,
                    right: Box::new(Expression::IntLiteral(2)),
                },
                discarded: false,
            }]),
        );

        expect_ast(
            "(1 - (3 + 2)) * (122 - (((9))));",
            Program(vec![ExpressionStatement {
                expr: Expression::Infix {
                    left: Box::new(Expression::Infix {
                        left: Box::new(Expression::IntLiteral(1)),
                        operator: InfixOperator::Minus,
                        right: Box::new(Expression::Infix {
                            left: Box::new(Expression::IntLiteral(3)),
                            operator: InfixOperator::Plus,
                            right: Box::new(Expression::IntLiteral(2)),
                        }),
                    }),
                    operator: InfixOperator::Multiply,
                    right: Box::new(Expression::Infix {
                        left: Box::new(Expression::IntLiteral(122)),
                        operator: InfixOperator::Minus,
                        right: Box::new(Expression::IntLiteral(9)),
                    }),
                },
                discarded: true,
            }]),
        );
    }

    #[test]
    fn variable_decls() {
        expect_ast(
            "foo :: 4321",
            Program(vec![ExpressionStatement {
                expr: Expression::VariableDecl {
                    name: "foo".to_string(),
                    value: Box::new(Expression::IntLiteral(4321)),
                    mutable: false,
                    ty: None,
                },
                discarded: false,
            }]),
        );

        expect_ast(
            "bar := 4321;",
            Program(vec![ExpressionStatement {
                expr: Expression::VariableDecl {
                    name: "bar".to_string(),
                    value: Box::new(Expression::IntLiteral(4321)),
                    mutable: true,
                    ty: None,
                },
                discarded: true,
            }]),
        );

        expect_ast(
            "typed_const : Int : 4321",
            Program(vec![ExpressionStatement {
                expr: Expression::VariableDecl {
                    name: "typed_const".to_string(),
                    value: Box::new(Expression::IntLiteral(4321)),
                    mutable: false,
                    ty: Some(Type::Ident("Int".to_string())),
                },
                discarded: false,
            }]),
        );

        expect_ast(
            "typed_mut : Int = 4321;",
            Program(vec![ExpressionStatement {
                expr: Expression::VariableDecl {
                    name: "typed_mut".to_string(),
                    value: Box::new(Expression::IntLiteral(4321)),
                    mutable: true,
                    ty: Some(Type::Ident("Int".to_string())),
                },
                discarded: true,
            }]),
        );
    }

    #[test]
    fn multiple_expression_satements() {
        expect_ast(
            "foo :: 3; foo - 4",
            Program(vec![
                ExpressionStatement {
                    expr: Expression::VariableDecl {
                        name: "foo".to_string(),
                        value: Box::new(Expression::IntLiteral(3)),
                        mutable: false,
                        ty: None,
                    },
                    discarded: true,
                },
                ExpressionStatement {
                    expr: Expression::Infix {
                        left: Box::new(Expression::Ident("foo".to_string())),
                        operator: InfixOperator::Minus,
                        right: Box::new(Expression::IntLiteral(4)),
                    },
                    discarded: false,
                },
            ]),
        );
    }

    #[test]
    fn function_expressions() {
        expect_ast(
            "fun () { unit }",
            Program(vec![ExpressionStatement {
                expr: Expression::Function {
                    parameters: vec![],
                    return_type: None,
                    body: vec![ExpressionStatement {
                        expr: Expression::Unit,
                        discarded: false,
                    }],
                },
                discarded: false,
            }]),
        );

        expect_ast(
            "fun () Int { 8 }",
            Program(vec![ExpressionStatement {
                expr: Expression::Function {
                    parameters: vec![],
                    return_type: Some(Type::Ident("Int".to_string())),
                    body: vec![ExpressionStatement {
                        expr: Expression::IntLiteral(8),
                        discarded: false,
                    }],
                },
                discarded: false,
            }]),
        );

        expect_ast(
            "fun () Int { x :: 8; x }",
            Program(vec![ExpressionStatement {
                expr: Expression::Function {
                    parameters: vec![],
                    return_type: Some(Type::Ident("Int".to_string())),
                    body: vec![
                        ExpressionStatement {
                            expr: Expression::VariableDecl {
                                name: "x".to_string(),
                                value: Box::new(Expression::IntLiteral(8)),
                                mutable: false,
                                ty: None,
                            },
                            discarded: true,
                        },
                        ExpressionStatement {
                            expr: Expression::Ident("x".to_string()),
                            discarded: false,
                        },
                    ],
                },
                discarded: false,
            }]),
        );

        expect_ast(
            "fun (x: Int) Int { x }",
            Program(vec![ExpressionStatement {
                expr: Expression::Function {
                    parameters: vec![FunctionParameter::LabeledAtCallsite {
                        internal_name: "x".to_string(),
                        external_name: None,
                        ty: Type::Ident("Int".to_string()),
                    }],
                    return_type: Some(Type::Ident("Int".to_string())),
                    body: vec![ExpressionStatement {
                        expr: Expression::Ident("x".to_string()),
                        discarded: false,
                    }],
                },
                discarded: false,
            }]),
        );

        expect_ast(
            "fun (external internal: Int) Int { internal }",
            Program(vec![ExpressionStatement {
                expr: Expression::Function {
                    parameters: vec![FunctionParameter::LabeledAtCallsite {
                        internal_name: "internal".to_string(),
                        external_name: Some("external".to_string()),
                        ty: Type::Ident("Int".to_string()),
                    }],
                    return_type: Some(Type::Ident("Int".to_string())),
                    body: vec![ExpressionStatement {
                        expr: Expression::Ident("internal".to_string()),
                        discarded: false,
                    }],
                },
                discarded: false,
            }]),
        );

        expect_ast(
            "fun (~x: Int) Int { x }",
            Program(vec![ExpressionStatement {
                expr: Expression::Function {
                    parameters: vec![FunctionParameter::UnlabeledAtCallsite {
                        name: "x".to_string(),
                        ty: Type::Ident("Int".to_string()),
                    }],
                    return_type: Some(Type::Ident("Int".to_string())),
                    body: vec![ExpressionStatement {
                        expr: Expression::Ident("x".to_string()),
                        discarded: false,
                    }],
                },
                discarded: false,
            }]),
        );
    }

    #[test]
    fn function_calls() {
        expect_ast(
            "print(3)",
            Program(vec![ExpressionStatement {
                expr: Expression::FunctionCall {
                    name: "print".to_string(),
                    arguments: vec![FunctionArgument {
                        label: None,
                        value: Expression::IntLiteral(3),
                    }],
                },
                discarded: false,
            }]),
        );

        expect_ast(
            "print(3, 4)",
            Program(vec![ExpressionStatement {
                expr: Expression::FunctionCall {
                    name: "print".to_string(),
                    arguments: vec![
                        FunctionArgument {
                            label: None,
                            value: Expression::IntLiteral(3),
                        },
                        FunctionArgument {
                            label: None,
                            value: Expression::IntLiteral(4),
                        },
                    ],
                },
                discarded: false,
            }]),
        );

        expect_ast(
            "print(3, and: 4)",
            Program(vec![ExpressionStatement {
                expr: Expression::FunctionCall {
                    name: "print".to_string(),
                    arguments: vec![
                        FunctionArgument {
                            label: None,
                            value: Expression::IntLiteral(3),
                        },
                        FunctionArgument {
                            label: Some("and".to_string()),
                            value: Expression::IntLiteral(4),
                        },
                    ],
                },
                discarded: false,
            }]),
        );
    }
}
