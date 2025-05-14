#![allow(dead_code)]

use std::iter::Peekable;

use crate::{
    lexer::{
        Lexer,
        tokens::TokenKind::{self, *},
    },
    span::Span,
};
use ast::{
    Expression, ExpressionKind, ExpressionStatement, FunctionArgument, FunctionParameter,
    InfixOperator, Precedence, PrefixOperator, Program, Type,
};

pub mod ast;

type PrefixParseFn = Box<dyn Fn(&mut Parser) -> Result<Expression, Error>>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Error {
    NoToken,
    NoPrefixParseFn(TokenKind),
    Expected { expected: String, got: String },
    Syntax(String),
    UnexpectedEof,
}

impl Error {
    pub fn expected(expected: &str, got: &str) -> Self {
        Self::Expected {
            expected: expected.to_string(),
            got: got.to_string(),
        }
    }

    pub fn syntax_err(s: &str) -> Self {
        Self::Syntax(format!("syntax error: {s}"))
    }
}

pub struct Parser<'src> {
    source: String,
    lexer: Peekable<Lexer<'src>>,
}

impl<'src> Parser<'src> {
    pub fn new(source: &'src str) -> Self {
        let lexer = Lexer::new(source).peekable();
        Self {
            source: source.to_string(),
            lexer,
        }
    }

    pub fn parse(&mut self) -> Result<Program, Error> {
        let mut stmts = vec![];
        while self.lexer.peek().is_some() {
            stmts.push(self.parse_expression_statement()?);
        }
        Ok(Program(stmts))
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStatement, Error> {
        let expr = self.parse_expression(Precedence::Lowest)?;
        let has_semicolon = self.check_semicolon()?;
        if has_semicolon {
            self.lexer.next();
        }

        Ok(ExpressionStatement {
            expr,
            discarded: has_semicolon,
        })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, Error> {
        let mut expr = self.prefix_parse_fn()?(self)?;

        while self.cur_precedence()? > precedence {
            match &self.peek_kind() {
                IntLiteral => return Err(Error::syntax_err("consecutive ints")),
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
                GreaterThan => {
                    expr = self.parse_infix_expression(
                        expr,
                        InfixOperator::GreaterThan,
                        Precedence::Comparison,
                    )?;
                }
                LessThan => {
                    expr = self.parse_infix_expression(
                        expr,
                        InfixOperator::LessThan,
                        Precedence::Comparison,
                    )?;
                }
                GreaterThanOrEqual => {
                    expr = self.parse_infix_expression(
                        expr,
                        InfixOperator::GreaterThanOrEqual,
                        Precedence::Comparison,
                    )?;
                }
                LessThanOrEqual => {
                    expr = self.parse_infix_expression(
                        expr,
                        InfixOperator::LessThanOrEqual,
                        Precedence::Comparison,
                    )?;
                }
                tok => return Err(Error::syntax_err(&format!("invalid operator: {tok}"))),
            }
        }

        Ok(expr)
    }

    fn prefix_parse_fn(&mut self) -> Result<PrefixParseFn, Error> {
        let peek_token_kind = self.peek_kind();
        match peek_token_kind {
            IntLiteral => Ok(Box::new(|parser| parser.parse_int_literal())),
            True | False => Ok(Box::new(|parser| parser.parse_bool_literal())),
            Unit => Ok(Box::new(|parser| parser.parse_unit())),
            LParen => Ok(Box::new(|parser| parser.parse_grouped_expression())),
            Identifier => {
                let (name, span) = self.expect_ident()?;
                match self.peek_kind() {
                    Colon => Ok(Box::new(move |parser| {
                        parser.parse_variable_declaration(&name, span.start)
                    })),
                    LParen => Ok(Box::new(move |parser| {
                        parser.parse_function_call(&name, span.start)
                    })),
                    _ => Ok(Box::new(move |parser| {
                        parser.parse_variable_ident(&name, span)
                    })),
                }
            }
            Fun => Ok(Box::new(|parser| parser.parse_function_expression())),
            ExclamationMark => Ok(Box::new(|parser| {
                parser.parse_prefix_expression(PrefixOperator::LogicalNot)
            })),
            Minus => Ok(Box::new(|parser| {
                parser.parse_prefix_expression(PrefixOperator::Negative)
            })),
            _ => Err(Error::NoPrefixParseFn(peek_token_kind.clone())),
        }
    }

    fn parse_prefix_expression(&mut self, operator: PrefixOperator) -> Result<Expression, Error> {
        let Span { start, .. } = self.expect_token(match operator {
            PrefixOperator::LogicalNot => ExclamationMark,
            PrefixOperator::Negative => Minus,
        })?;
        let expr = self.parse_expression(Precedence::Prefix)?;
        let span = expr.span;
        Ok(Expression::new(
            ExpressionKind::Prefix {
                operator,
                expression: Box::new(expr),
            },
            Span::from_range(start, span.end),
        ))
    }

    fn parse_infix_expression(
        &mut self,
        lhs: Expression,
        operator: InfixOperator,
        precedence: Precedence,
    ) -> Result<Expression, Error> {
        self.lexer.next();
        let rhs = self.parse_expression(precedence)?;
        let start_position = lhs.span.start;
        let end_position = rhs.span.end;
        Ok(Expression::new(
            ExpressionKind::Infix {
                left: Box::new(lhs),
                operator,
                right: Box::new(rhs),
            },
            Span::from_range(start_position, end_position),
        ))
    }

    fn parse_int_literal(&mut self) -> Result<Expression, Error> {
        let (num, span) = self.expect_int()?;
        Ok(Expression::new(ExpressionKind::IntLiteral(num), span))
    }

    fn parse_bool_literal(&mut self) -> Result<Expression, Error> {
        match self.peek_kind() {
            True => {
                let span = self.expect_token(True)?;
                Ok(Expression::new(ExpressionKind::BoolLiteral(true), span))
            }
            False => {
                let span = self.expect_token(False)?;
                Ok(Expression::new(ExpressionKind::BoolLiteral(false), span))
            }
            tok => Err(Error::expected("true or false", &tok.to_string())),
        }
    }

    fn parse_unit(&mut self) -> Result<Expression, Error> {
        let span = self.expect_token(Unit)?;
        Ok(Expression::new(ExpressionKind::Unit, span))
    }

    fn parse_variable_ident(&mut self, name: &str, span: Span) -> Result<Expression, Error> {
        Ok(Expression::new(
            ExpressionKind::Ident(name.to_string()),
            span,
        ))
    }

    fn parse_type(&mut self) -> Result<Type, Error> {
        let (name, _) = self.expect_ident()?;
        Ok(Type::Ident(name))
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, Error> {
        let start_position = self.expect_token(LParen)?.start;
        let expr = self.parse_expression(Precedence::Lowest)?;
        match self.peek_kind() {
            RParen => {
                let end_position = self.expect_token(RParen)?.end;
                Ok(Expression::new(
                    expr.kind,
                    Span::from_range(start_position, end_position),
                ))
            }
            tok => Err(Error::expected(")", &tok.to_string())),
        }
    }

    fn parse_function_call(
        &mut self,
        fn_name: &str,
        start_position: usize,
    ) -> Result<Expression, Error> {
        self.expect_token(LParen)?;

        let mut arguments = vec![];

        while self.peek_kind() != &RParen {
            arguments.push(self.parse_function_argument()?);
            if self.peek_kind() == &Comma {
                self.expect_token(Comma)?;
                continue;
            } else {
                break;
            }
        }

        let end_span = self.expect_token(RParen)?;

        Ok(Expression::new(
            ExpressionKind::FunctionCall {
                name: fn_name.to_string(),
                arguments,
            },
            Span::from_range(start_position, end_span.end),
        ))
    }

    fn parse_function_argument(&mut self) -> Result<FunctionArgument, Error> {
        let mut label = None;
        let value: Expression;

        let tok = self.lexer.peek().ok_or(Error::UnexpectedEof)?;
        let span = tok.span;
        if let Identifier = tok.kind {
            let (name, _) = self.expect_ident()?;
            if self.peek_kind() == &Colon {
                self.expect_token(Colon)?;
                label = Some(name);
                value = self.parse_expression(Precedence::Lowest)?;
            } else {
                value = Expression::new(ExpressionKind::Ident(name), span);
            }
        } else {
            value = self.parse_expression(Precedence::Lowest)?;
        }

        Ok(FunctionArgument { label, value })
    }

    fn parse_variable_declaration(
        &mut self,
        name: &str,
        start_position: usize,
    ) -> Result<Expression, Error> {
        self.expect_token(Colon)?;

        let mut ty = None;
        if let Identifier = self.peek_kind() {
            ty = Some(self.parse_type()?);
        }

        #[allow(clippy::needless_late_init)]
        let value: Expression;
        let mutable: bool;

        match self.peek_kind() {
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
            tok => return Err(Error::expected(": or =", &tok.to_string())),
        }

        let end_position = value.span.end;
        Ok(Expression::new(
            ExpressionKind::VariableDecl {
                name: name.to_string(),
                value: Box::new(value),
                mutable,
                ty,
            },
            Span::from_range(start_position, end_position),
        ))
    }

    fn parse_function_expression(&mut self) -> Result<Expression, Error> {
        let start_span = self.expect_token(Fun)?;
        self.expect_token(LParen)?;

        let mut parameters = vec![];

        while self.peek_kind() != &RParen {
            parameters.push(self.parse_function_parameter()?);
            if self.peek_kind() == &Comma {
                self.expect_token(Comma)?;
                continue;
            } else {
                break;
            }
        }

        self.expect_token(RParen)?;

        let mut return_type = None;

        if let Identifier = self.peek_kind() {
            return_type = Some(self.parse_type()?);
        }

        self.expect_token(LBrace)?;

        let mut body = vec![];

        while self.peek_kind() != &RBrace {
            body.push(self.parse_expression_statement()?);
        }

        let end_span = self.expect_token(RBrace)?;

        Ok(Expression::new(
            ExpressionKind::Function {
                parameters,
                return_type,
                body,
            },
            Span::from_range(start_span.start, end_span.end),
        ))
    }

    fn parse_function_parameter(&mut self) -> Result<FunctionParameter, Error> {
        match self.peek_kind() {
            Tilde => {
                self.expect_token(Tilde)?;
                let (name, _) = self.expect_ident()?;
                self.expect_token(Colon)?;
                let ty = self.parse_type()?;
                Ok(FunctionParameter::UnlabeledAtCallsite { name, ty })
            }
            Identifier => {
                let (first, _) = self.expect_ident()?;
                let mut second = None;
                if let Identifier = self.peek_kind() {
                    let (name, _) = self.expect_ident()?;
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
            tok => Err(Error::expected("parameter name", &tok.to_string())),
        }
    }

    fn check_semicolon(&mut self) -> Result<bool, Error> {
        Ok(self.peek_kind() == &Semicolon)
    }

    fn expect_token(&mut self, kind: TokenKind) -> Result<Span, Error> {
        let tok = self.lexer.next().ok_or(Error::UnexpectedEof)?;
        if tok.kind != kind {
            Err(Error::syntax_err(&format!(
                "unexpected token: {}",
                tok.kind
            )))
        } else {
            Ok(tok.span)
        }
    }

    fn expect_int(&mut self) -> Result<(isize, Span), Error> {
        let token = self.lexer.peek().ok_or(Error::UnexpectedEof)?;
        let span = token.span;
        if let IntLiteral = token.kind {
            self.lexer.next();
            Ok((span.slice(&self.source).parse().unwrap(), span))
        } else {
            Err(Error::expected("int literal", &token.kind.to_string()))
        }
    }

    fn expect_ident(&mut self) -> Result<(String, Span), Error> {
        let token = self.lexer.peek().ok_or(Error::UnexpectedEof)?;
        let kind = token.kind.clone();
        let span = token.span;
        if let Identifier = kind {
            self.lexer.next();
            // up to you whether you want to return a `String` or a `&str`
            Ok((span.slice(&self.source).to_owned(), span))
        } else {
            Err(Error::expected("identifier", &kind.to_string()))
        }
    }

    fn cur_precedence(&mut self) -> Result<Precedence, Error> {
        Ok(match self.peek_kind() {
            LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual => Precedence::Comparison,
            Plus | Minus => Precedence::Sum,
            Asterisk | Slash => Precedence::Product,
            LParen => Precedence::Group,
            _ => Precedence::Lowest,
        })
    }

    fn cur_kind(&mut self) -> TokenKind {
        self.lexer.next().map_or(TokenKind::Eof, |t| t.kind)
    }

    fn peek_kind(&mut self) -> &TokenKind {
        self.lexer.peek().map_or(&TokenKind::Eof, |t| &t.kind)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Parser;
    use insta::assert_debug_snapshot as snap;

    use super::ast::Program;

    fn ast(input: &str) -> Program {
        Parser::new(input).parse().unwrap()
    }

    #[test]
    fn single_value() {
        snap!(ast("122"), @r"
        Program(
            [
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 0,
                            end: 3,
                        },
                        kind: IntLiteral(
                            122,
                        ),
                    },
                    discarded: false,
                },
            ],
        )
        ");
        snap!(ast("122;"), @r"
        Program(
            [
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 0,
                            end: 3,
                        },
                        kind: IntLiteral(
                            122,
                        ),
                    },
                    discarded: true,
                },
            ],
        )
        ");
        snap!(ast("foo;"), @r#"
        Program(
            [
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 0,
                            end: 3,
                        },
                        kind: Ident(
                            "foo",
                        ),
                    },
                    discarded: true,
                },
            ],
        )
        "#);
        snap!(ast("unit"), @r"
        Program(
            [
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 0,
                            end: 4,
                        },
                        kind: Unit,
                    },
                    discarded: false,
                },
            ],
        )
        ");
        snap!(ast("true; false"), @r"
        Program(
            [
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 0,
                            end: 4,
                        },
                        kind: BoolLiteral(
                            true,
                        ),
                    },
                    discarded: true,
                },
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 6,
                            end: 11,
                        },
                        kind: BoolLiteral(
                            false,
                        ),
                    },
                    discarded: false,
                },
            ],
        )
        ");
    }

    #[test]
    fn prefix_operators() {
        snap!(ast("!true"), @r"
        Program(
            [
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 0,
                            end: 5,
                        },
                        kind: Prefix {
                            operator: LogicalNot,
                            expression: Expression {
                                span: Span {
                                    start: 1,
                                    end: 5,
                                },
                                kind: BoolLiteral(
                                    true,
                                ),
                            },
                        },
                    },
                    discarded: false,
                },
            ],
        )
        ");
        snap!(ast("-3 * -67"), @r"
        Program(
            [
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 0,
                            end: 8,
                        },
                        kind: Infix {
                            left: Expression {
                                span: Span {
                                    start: 0,
                                    end: 2,
                                },
                                kind: Prefix {
                                    operator: Negative,
                                    expression: Expression {
                                        span: Span {
                                            start: 1,
                                            end: 2,
                                        },
                                        kind: IntLiteral(
                                            3,
                                        ),
                                    },
                                },
                            },
                            operator: Multiply,
                            right: Expression {
                                span: Span {
                                    start: 5,
                                    end: 8,
                                },
                                kind: Prefix {
                                    operator: Negative,
                                    expression: Expression {
                                        span: Span {
                                            start: 6,
                                            end: 8,
                                        },
                                        kind: IntLiteral(
                                            67,
                                        ),
                                    },
                                },
                            },
                        },
                    },
                    discarded: false,
                },
            ],
        )
        ");
    }

    #[test]
    fn mathematical_expressions() {
        snap!(ast("1 + 2 * 3"), @r"
        Program(
            [
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 0,
                            end: 9,
                        },
                        kind: Infix {
                            left: Expression {
                                span: Span {
                                    start: 0,
                                    end: 1,
                                },
                                kind: IntLiteral(
                                    1,
                                ),
                            },
                            operator: Plus,
                            right: Expression {
                                span: Span {
                                    start: 4,
                                    end: 9,
                                },
                                kind: Infix {
                                    left: Expression {
                                        span: Span {
                                            start: 4,
                                            end: 5,
                                        },
                                        kind: IntLiteral(
                                            2,
                                        ),
                                    },
                                    operator: Multiply,
                                    right: Expression {
                                        span: Span {
                                            start: 8,
                                            end: 9,
                                        },
                                        kind: IntLiteral(
                                            3,
                                        ),
                                    },
                                },
                            },
                        },
                    },
                    discarded: false,
                },
            ],
        )
        ");
        snap!(ast("6 - 3 - 2"), @r"
        Program(
            [
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 0,
                            end: 9,
                        },
                        kind: Infix {
                            left: Expression {
                                span: Span {
                                    start: 0,
                                    end: 5,
                                },
                                kind: Infix {
                                    left: Expression {
                                        span: Span {
                                            start: 0,
                                            end: 1,
                                        },
                                        kind: IntLiteral(
                                            6,
                                        ),
                                    },
                                    operator: Minus,
                                    right: Expression {
                                        span: Span {
                                            start: 4,
                                            end: 5,
                                        },
                                        kind: IntLiteral(
                                            3,
                                        ),
                                    },
                                },
                            },
                            operator: Minus,
                            right: Expression {
                                span: Span {
                                    start: 8,
                                    end: 9,
                                },
                                kind: IntLiteral(
                                    2,
                                ),
                            },
                        },
                    },
                    discarded: false,
                },
            ],
        )
        ");
        snap!(ast("(1 - (3 + 2)) * (122 - (((9))));"), @r"
        Program(
            [
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 0,
                            end: 31,
                        },
                        kind: Infix {
                            left: Expression {
                                span: Span {
                                    start: 0,
                                    end: 13,
                                },
                                kind: Infix {
                                    left: Expression {
                                        span: Span {
                                            start: 1,
                                            end: 2,
                                        },
                                        kind: IntLiteral(
                                            1,
                                        ),
                                    },
                                    operator: Minus,
                                    right: Expression {
                                        span: Span {
                                            start: 5,
                                            end: 12,
                                        },
                                        kind: Infix {
                                            left: Expression {
                                                span: Span {
                                                    start: 6,
                                                    end: 7,
                                                },
                                                kind: IntLiteral(
                                                    3,
                                                ),
                                            },
                                            operator: Plus,
                                            right: Expression {
                                                span: Span {
                                                    start: 10,
                                                    end: 11,
                                                },
                                                kind: IntLiteral(
                                                    2,
                                                ),
                                            },
                                        },
                                    },
                                },
                            },
                            operator: Multiply,
                            right: Expression {
                                span: Span {
                                    start: 16,
                                    end: 31,
                                },
                                kind: Infix {
                                    left: Expression {
                                        span: Span {
                                            start: 17,
                                            end: 20,
                                        },
                                        kind: IntLiteral(
                                            122,
                                        ),
                                    },
                                    operator: Minus,
                                    right: Expression {
                                        span: Span {
                                            start: 23,
                                            end: 30,
                                        },
                                        kind: IntLiteral(
                                            9,
                                        ),
                                    },
                                },
                            },
                        },
                    },
                    discarded: true,
                },
            ],
        )
        ");
    }

    #[test]
    fn logical_expressions() {
        snap!(ast("4 > 3;"), @r"
        Program(
            [
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 0,
                            end: 5,
                        },
                        kind: Infix {
                            left: Expression {
                                span: Span {
                                    start: 0,
                                    end: 1,
                                },
                                kind: IntLiteral(
                                    4,
                                ),
                            },
                            operator: GreaterThan,
                            right: Expression {
                                span: Span {
                                    start: 4,
                                    end: 5,
                                },
                                kind: IntLiteral(
                                    3,
                                ),
                            },
                        },
                    },
                    discarded: true,
                },
            ],
        )
        ");
    }

    #[test]
    fn variable_decls() {
        snap!(ast("foo :: 4321"), @r#"
        Program(
            [
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 0,
                            end: 11,
                        },
                        kind: VariableDecl {
                            name: "foo",
                            value: Expression {
                                span: Span {
                                    start: 7,
                                    end: 11,
                                },
                                kind: IntLiteral(
                                    4321,
                                ),
                            },
                            mutable: false,
                            ty: None,
                        },
                    },
                    discarded: false,
                },
            ],
        )
        "#);
        snap!(ast("bar := 4321;"), @r#"
        Program(
            [
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 0,
                            end: 11,
                        },
                        kind: VariableDecl {
                            name: "bar",
                            value: Expression {
                                span: Span {
                                    start: 7,
                                    end: 11,
                                },
                                kind: IntLiteral(
                                    4321,
                                ),
                            },
                            mutable: true,
                            ty: None,
                        },
                    },
                    discarded: true,
                },
            ],
        )
        "#);
        snap!(ast("typed_const : Int : 4321"), @r#"
        Program(
            [
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 0,
                            end: 24,
                        },
                        kind: VariableDecl {
                            name: "typed_const",
                            value: Expression {
                                span: Span {
                                    start: 20,
                                    end: 24,
                                },
                                kind: IntLiteral(
                                    4321,
                                ),
                            },
                            mutable: false,
                            ty: Some(
                                Ident(
                                    "Int",
                                ),
                            ),
                        },
                    },
                    discarded: false,
                },
            ],
        )
        "#);
        snap!(ast("typed_mut : Int = 4321;"), @r#"
        Program(
            [
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 0,
                            end: 22,
                        },
                        kind: VariableDecl {
                            name: "typed_mut",
                            value: Expression {
                                span: Span {
                                    start: 18,
                                    end: 22,
                                },
                                kind: IntLiteral(
                                    4321,
                                ),
                            },
                            mutable: true,
                            ty: Some(
                                Ident(
                                    "Int",
                                ),
                            ),
                        },
                    },
                    discarded: true,
                },
            ],
        )
        "#);
    }

    #[test]
    fn multiple_expression_satements() {
        snap!(ast("foo :: 3; foo - 4"), @r#"
        Program(
            [
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 0,
                            end: 8,
                        },
                        kind: VariableDecl {
                            name: "foo",
                            value: Expression {
                                span: Span {
                                    start: 7,
                                    end: 8,
                                },
                                kind: IntLiteral(
                                    3,
                                ),
                            },
                            mutable: false,
                            ty: None,
                        },
                    },
                    discarded: true,
                },
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 10,
                            end: 17,
                        },
                        kind: Infix {
                            left: Expression {
                                span: Span {
                                    start: 10,
                                    end: 13,
                                },
                                kind: Ident(
                                    "foo",
                                ),
                            },
                            operator: Minus,
                            right: Expression {
                                span: Span {
                                    start: 16,
                                    end: 17,
                                },
                                kind: IntLiteral(
                                    4,
                                ),
                            },
                        },
                    },
                    discarded: false,
                },
            ],
        )
        "#);
    }

    #[test]
    fn function_expressions() {
        snap!(ast("fun () { unit }"), @r"
        Program(
            [
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 0,
                            end: 15,
                        },
                        kind: Function {
                            parameters: [],
                            return_type: None,
                            body: [
                                ExpressionStatement {
                                    expr: Expression {
                                        span: Span {
                                            start: 9,
                                            end: 13,
                                        },
                                        kind: Unit,
                                    },
                                    discarded: false,
                                },
                            ],
                        },
                    },
                    discarded: false,
                },
            ],
        )
        ");
        snap!(ast("fun () Int { 8 }"), @r#"
        Program(
            [
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 0,
                            end: 16,
                        },
                        kind: Function {
                            parameters: [],
                            return_type: Some(
                                Ident(
                                    "Int",
                                ),
                            ),
                            body: [
                                ExpressionStatement {
                                    expr: Expression {
                                        span: Span {
                                            start: 13,
                                            end: 14,
                                        },
                                        kind: IntLiteral(
                                            8,
                                        ),
                                    },
                                    discarded: false,
                                },
                            ],
                        },
                    },
                    discarded: false,
                },
            ],
        )
        "#);
        snap!(ast("fun () Int { x :: 8; x }"), @r#"
        Program(
            [
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 0,
                            end: 24,
                        },
                        kind: Function {
                            parameters: [],
                            return_type: Some(
                                Ident(
                                    "Int",
                                ),
                            ),
                            body: [
                                ExpressionStatement {
                                    expr: Expression {
                                        span: Span {
                                            start: 13,
                                            end: 19,
                                        },
                                        kind: VariableDecl {
                                            name: "x",
                                            value: Expression {
                                                span: Span {
                                                    start: 18,
                                                    end: 19,
                                                },
                                                kind: IntLiteral(
                                                    8,
                                                ),
                                            },
                                            mutable: false,
                                            ty: None,
                                        },
                                    },
                                    discarded: true,
                                },
                                ExpressionStatement {
                                    expr: Expression {
                                        span: Span {
                                            start: 21,
                                            end: 22,
                                        },
                                        kind: Ident(
                                            "x",
                                        ),
                                    },
                                    discarded: false,
                                },
                            ],
                        },
                    },
                    discarded: false,
                },
            ],
        )
        "#);
        snap!(ast("fun (x: Int) Int { x }"), @r#"
        Program(
            [
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 0,
                            end: 22,
                        },
                        kind: Function {
                            parameters: [
                                LabeledAtCallsite {
                                    internal_name: "x",
                                    external_name: None,
                                    ty: Ident(
                                        "Int",
                                    ),
                                },
                            ],
                            return_type: Some(
                                Ident(
                                    "Int",
                                ),
                            ),
                            body: [
                                ExpressionStatement {
                                    expr: Expression {
                                        span: Span {
                                            start: 19,
                                            end: 20,
                                        },
                                        kind: Ident(
                                            "x",
                                        ),
                                    },
                                    discarded: false,
                                },
                            ],
                        },
                    },
                    discarded: false,
                },
            ],
        )
        "#);
        snap!(ast("fun (external internal: Int) Int { internal }"), @r#"
        Program(
            [
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 0,
                            end: 45,
                        },
                        kind: Function {
                            parameters: [
                                LabeledAtCallsite {
                                    internal_name: "internal",
                                    external_name: Some(
                                        "external",
                                    ),
                                    ty: Ident(
                                        "Int",
                                    ),
                                },
                            ],
                            return_type: Some(
                                Ident(
                                    "Int",
                                ),
                            ),
                            body: [
                                ExpressionStatement {
                                    expr: Expression {
                                        span: Span {
                                            start: 35,
                                            end: 43,
                                        },
                                        kind: Ident(
                                            "internal",
                                        ),
                                    },
                                    discarded: false,
                                },
                            ],
                        },
                    },
                    discarded: false,
                },
            ],
        )
        "#);
        snap!(ast("fun (~x: Int) Int { x }"), @r#"
        Program(
            [
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 0,
                            end: 23,
                        },
                        kind: Function {
                            parameters: [
                                UnlabeledAtCallsite {
                                    name: "x",
                                    ty: Ident(
                                        "Int",
                                    ),
                                },
                            ],
                            return_type: Some(
                                Ident(
                                    "Int",
                                ),
                            ),
                            body: [
                                ExpressionStatement {
                                    expr: Expression {
                                        span: Span {
                                            start: 20,
                                            end: 21,
                                        },
                                        kind: Ident(
                                            "x",
                                        ),
                                    },
                                    discarded: false,
                                },
                            ],
                        },
                    },
                    discarded: false,
                },
            ],
        )
        "#);
    }

    #[test]
    fn function_calls() {
        snap!(ast("print(3)"), @r#"
        Program(
            [
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 0,
                            end: 8,
                        },
                        kind: FunctionCall {
                            name: "print",
                            arguments: [
                                FunctionArgument {
                                    label: None,
                                    value: Expression {
                                        span: Span {
                                            start: 6,
                                            end: 7,
                                        },
                                        kind: IntLiteral(
                                            3,
                                        ),
                                    },
                                },
                            ],
                        },
                    },
                    discarded: false,
                },
            ],
        )
        "#);
        snap!(ast("print(3, 4)"), @r#"
        Program(
            [
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 0,
                            end: 11,
                        },
                        kind: FunctionCall {
                            name: "print",
                            arguments: [
                                FunctionArgument {
                                    label: None,
                                    value: Expression {
                                        span: Span {
                                            start: 6,
                                            end: 7,
                                        },
                                        kind: IntLiteral(
                                            3,
                                        ),
                                    },
                                },
                                FunctionArgument {
                                    label: None,
                                    value: Expression {
                                        span: Span {
                                            start: 9,
                                            end: 10,
                                        },
                                        kind: IntLiteral(
                                            4,
                                        ),
                                    },
                                },
                            ],
                        },
                    },
                    discarded: false,
                },
            ],
        )
        "#);
        snap!(ast("print(3, and: 4)"), @r#"
        Program(
            [
                ExpressionStatement {
                    expr: Expression {
                        span: Span {
                            start: 0,
                            end: 16,
                        },
                        kind: FunctionCall {
                            name: "print",
                            arguments: [
                                FunctionArgument {
                                    label: None,
                                    value: Expression {
                                        span: Span {
                                            start: 6,
                                            end: 7,
                                        },
                                        kind: IntLiteral(
                                            3,
                                        ),
                                    },
                                },
                                FunctionArgument {
                                    label: Some(
                                        "and",
                                    ),
                                    value: Expression {
                                        span: Span {
                                            start: 14,
                                            end: 15,
                                        },
                                        kind: IntLiteral(
                                            4,
                                        ),
                                    },
                                },
                            ],
                        },
                    },
                    discarded: false,
                },
            ],
        )
        "#);
    }
}
