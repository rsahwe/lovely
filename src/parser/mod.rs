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
    use crate::{
        parser::{
            Parser,
            ast::{
                Expression, ExpressionKind, ExpressionStatement, FunctionArgument,
                FunctionParameter, InfixOperator, Program, Type,
            },
        },
        span::Span,
    };
    use pretty_assertions::assert_eq;

    use super::ast::PrefixOperator;

    fn expect_ast(input: &str, ast: Program) {
        let mut parser = Parser::new(input);
        let parsed = parser.parse();
        assert_eq!(parsed, Ok(ast));
    }

    #[test]
    fn single_value() {
        expect_ast(
            "122",
            Program(vec![ExpressionStatement {
                expr: Expression::new(ExpressionKind::IntLiteral(122), Span::from_range(0, 3)),
                discarded: false,
            }]),
        );

        expect_ast(
            "122;",
            Program(vec![ExpressionStatement {
                expr: Expression::new(ExpressionKind::IntLiteral(122), Span::from_range(0, 3)),
                discarded: true,
            }]),
        );

        expect_ast(
            "foo;",
            Program(vec![ExpressionStatement {
                expr: Expression::new(
                    ExpressionKind::Ident("foo".to_string()),
                    Span::from_range(0, 3),
                ),
                discarded: true,
            }]),
        );

        expect_ast(
            "unit",
            Program(vec![ExpressionStatement {
                expr: Expression::new(ExpressionKind::Unit, Span::from_range(0, 4)),
                discarded: false,
            }]),
        );

        expect_ast(
            "true; false",
            Program(vec![
                ExpressionStatement {
                    expr: Expression::new(
                        ExpressionKind::BoolLiteral(true),
                        Span::from_range(0, 4),
                    ),
                    discarded: true,
                },
                ExpressionStatement {
                    expr: Expression::new(
                        ExpressionKind::BoolLiteral(false),
                        Span::from_range(6, 11),
                    ),
                    discarded: false,
                },
            ]),
        );
    }

    #[test]
    fn prefix_operators() {
        expect_ast(
            "!true",
            Program(vec![ExpressionStatement {
                expr: Expression::new(
                    ExpressionKind::Prefix {
                        operator: PrefixOperator::LogicalNot,
                        expression: Box::new(Expression::new(
                            ExpressionKind::BoolLiteral(true),
                            Span::from_range(1, 5),
                        )),
                    },
                    Span::from_range(0, 5),
                ),
                discarded: false,
            }]),
        );

        expect_ast(
            "-3 * -67",
            Program(vec![ExpressionStatement {
                expr: Expression::new(
                    ExpressionKind::Infix {
                        left: Box::new(Expression::new(
                            ExpressionKind::Prefix {
                                operator: PrefixOperator::Negative,
                                expression: Box::new(Expression::new(
                                    ExpressionKind::IntLiteral(3),
                                    Span::from_range(1, 2),
                                )),
                            },
                            Span::from_range(0, 2),
                        )),
                        operator: InfixOperator::Multiply,
                        right: Box::new(Expression::new(
                            ExpressionKind::Prefix {
                                operator: PrefixOperator::Negative,
                                expression: Box::new(Expression::new(
                                    ExpressionKind::IntLiteral(67),
                                    Span::from_range(6, 8),
                                )),
                            },
                            Span::from_range(5, 8),
                        )),
                    },
                    Span::from_range(0, 8),
                ),
                discarded: false,
            }]),
        );
    }

    #[test]
    fn mathematical_expressions() {
        expect_ast(
            "1 + 2 * 3",
            Program(vec![ExpressionStatement {
                expr: Expression::new(
                    ExpressionKind::Infix {
                        left: Box::new(Expression::new(
                            ExpressionKind::IntLiteral(1),
                            Span::from_range(0, 1),
                        )),
                        operator: InfixOperator::Plus,
                        right: Box::new(Expression::new(
                            ExpressionKind::Infix {
                                left: Box::new(Expression::new(
                                    ExpressionKind::IntLiteral(2),
                                    Span::from_range(4, 5),
                                )),
                                operator: InfixOperator::Multiply,
                                right: Box::new(Expression::new(
                                    ExpressionKind::IntLiteral(3),
                                    Span::from_range(8, 9),
                                )),
                            },
                            Span::from_range(4, 9),
                        )),
                    },
                    Span::from_range(0, 9),
                ),
                discarded: false,
            }]),
        );

        expect_ast(
            "6 - 3 - 2",
            Program(vec![ExpressionStatement {
                expr: Expression::new(
                    ExpressionKind::Infix {
                        left: Box::new(Expression::new(
                            ExpressionKind::Infix {
                                left: Box::new(Expression::new(
                                    ExpressionKind::IntLiteral(6),
                                    Span::from_range(0, 1),
                                )),
                                operator: InfixOperator::Minus,
                                right: Box::new(Expression::new(
                                    ExpressionKind::IntLiteral(3),
                                    Span::from_range(4, 5),
                                )),
                            },
                            Span::from_range(0, 5),
                        )),
                        operator: InfixOperator::Minus,
                        right: Box::new(Expression::new(
                            ExpressionKind::IntLiteral(2),
                            Span::from_range(8, 9),
                        )),
                    },
                    Span::from_range(0, 9),
                ),
                discarded: false,
            }]),
        );

        expect_ast(
            "(1 - (3 + 2)) * (122 - (((9))));",
            Program(vec![ExpressionStatement {
                expr: Expression::new(
                    ExpressionKind::Infix {
                        left: Box::new(Expression::new(
                            ExpressionKind::Infix {
                                left: Box::new(Expression::new(
                                    ExpressionKind::IntLiteral(1),
                                    Span::from_range(1, 2),
                                )),
                                operator: InfixOperator::Minus,
                                right: Box::new(Expression::new(
                                    ExpressionKind::Infix {
                                        left: Box::new(Expression::new(
                                            ExpressionKind::IntLiteral(3),
                                            Span::from_range(6, 7),
                                        )),
                                        operator: InfixOperator::Plus,
                                        right: Box::new(Expression::new(
                                            ExpressionKind::IntLiteral(2),
                                            Span::from_range(10, 11),
                                        )),
                                    },
                                    Span::from_range(5, 12),
                                )),
                            },
                            Span::from_range(0, 13),
                        )),
                        operator: InfixOperator::Multiply,
                        right: Box::new(Expression::new(
                            ExpressionKind::Infix {
                                left: Box::new(Expression::new(
                                    ExpressionKind::IntLiteral(122),
                                    Span::from_range(17, 20),
                                )),
                                operator: InfixOperator::Minus,
                                right: Box::new(Expression::new(
                                    ExpressionKind::IntLiteral(9),
                                    Span::from_range(23, 30),
                                )),
                            },
                            Span::from_range(16, 31),
                        )),
                    },
                    Span::from_range(0, 31),
                ),
                discarded: true,
            }]),
        );
    }

    #[test]
    fn logical_expressions() {
        expect_ast(
            "4 > 3;",
            Program(vec![ExpressionStatement {
                expr: Expression::new(
                    ExpressionKind::Infix {
                        left: Box::new(Expression::new(
                            ExpressionKind::IntLiteral(4),
                            Span::from_range(0, 1),
                        )),
                        operator: InfixOperator::GreaterThan,
                        right: Box::new(Expression::new(
                            ExpressionKind::IntLiteral(3),
                            Span::from_range(4, 5),
                        )),
                    },
                    Span::from_range(0, 5),
                ),
                discarded: true,
            }]),
        );
    }

    #[test]
    fn variable_decls() {
        expect_ast(
            "foo :: 4321",
            Program(vec![ExpressionStatement {
                expr: Expression::new(
                    ExpressionKind::VariableDecl {
                        name: "foo".to_string(),
                        value: Box::new(Expression::new(
                            ExpressionKind::IntLiteral(4321),
                            Span::from_range(7, 11),
                        )),
                        mutable: false,
                        ty: None,
                    },
                    Span::from_range(0, 11),
                ),
                discarded: false,
            }]),
        );

        expect_ast(
            "bar := 4321;",
            Program(vec![ExpressionStatement {
                expr: Expression::new(
                    ExpressionKind::VariableDecl {
                        name: "bar".to_string(),
                        value: Box::new(Expression::new(
                            ExpressionKind::IntLiteral(4321),
                            Span::from_range(7, 11),
                        )),
                        mutable: true,
                        ty: None,
                    },
                    Span::from_range(0, 11),
                ),
                discarded: true,
            }]),
        );

        expect_ast(
            "typed_const : Int : 4321",
            Program(vec![ExpressionStatement {
                expr: Expression::new(
                    ExpressionKind::VariableDecl {
                        name: "typed_const".to_string(),
                        value: Box::new(Expression::new(
                            ExpressionKind::IntLiteral(4321),
                            Span::from_range(20, 24),
                        )),
                        mutable: false,
                        ty: Some(Type::Ident("Int".to_string())),
                    },
                    Span::from_range(0, 24),
                ),
                discarded: false,
            }]),
        );

        expect_ast(
            "typed_mut : Int = 4321;",
            Program(vec![ExpressionStatement {
                expr: Expression::new(
                    ExpressionKind::VariableDecl {
                        name: "typed_mut".to_string(),
                        value: Box::new(Expression::new(
                            ExpressionKind::IntLiteral(4321),
                            Span::from_range(18, 22),
                        )),
                        mutable: true,
                        ty: Some(Type::Ident("Int".to_string())),
                    },
                    Span::from_range(0, 22),
                ),
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
                    expr: Expression::new(
                        ExpressionKind::VariableDecl {
                            name: "foo".to_string(),
                            value: Box::new(Expression::new(
                                ExpressionKind::IntLiteral(3),
                                Span::from_range(7, 8),
                            )),
                            mutable: false,
                            ty: None,
                        },
                        Span::from_range(0, 8),
                    ),
                    discarded: true,
                },
                ExpressionStatement {
                    expr: Expression::new(
                        ExpressionKind::Infix {
                            left: Box::new(Expression::new(
                                ExpressionKind::Ident("foo".to_string()),
                                Span::from_range(10, 13),
                            )),
                            operator: InfixOperator::Minus,
                            right: Box::new(Expression::new(
                                ExpressionKind::IntLiteral(4),
                                Span::from_range(16, 17),
                            )),
                        },
                        Span::from_range(10, 17),
                    ),
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
                expr: Expression::new(
                    ExpressionKind::Function {
                        parameters: vec![],
                        return_type: None,
                        body: vec![ExpressionStatement {
                            expr: Expression::new(ExpressionKind::Unit, Span::from_range(9, 13)),
                            discarded: false,
                        }],
                    },
                    Span::from_range(0, 15),
                ),
                discarded: false,
            }]),
        );

        expect_ast(
            "fun () Int { 8 }",
            Program(vec![ExpressionStatement {
                expr: Expression::new(
                    ExpressionKind::Function {
                        parameters: vec![],
                        return_type: Some(Type::Ident("Int".to_string())),
                        body: vec![ExpressionStatement {
                            expr: Expression::new(
                                ExpressionKind::IntLiteral(8),
                                Span::from_range(13, 14),
                            ),
                            discarded: false,
                        }],
                    },
                    Span::from_range(0, 16),
                ),
                discarded: false,
            }]),
        );

        expect_ast(
            "fun () Int { x :: 8; x }",
            Program(vec![ExpressionStatement {
                expr: Expression::new(
                    ExpressionKind::Function {
                        parameters: vec![],
                        return_type: Some(Type::Ident("Int".to_string())),
                        body: vec![
                            ExpressionStatement {
                                expr: Expression::new(
                                    ExpressionKind::VariableDecl {
                                        name: "x".to_string(),
                                        value: Box::new(Expression::new(
                                            ExpressionKind::IntLiteral(8),
                                            Span::from_range(18, 19),
                                        )),
                                        mutable: false,
                                        ty: None,
                                    },
                                    Span::from_range(13, 19),
                                ),
                                discarded: true,
                            },
                            ExpressionStatement {
                                expr: Expression::new(
                                    ExpressionKind::Ident("x".to_string()),
                                    Span::from_range(21, 22),
                                ),
                                discarded: false,
                            },
                        ],
                    },
                    Span::from_range(0, 24),
                ),
                discarded: false,
            }]),
        );

        expect_ast(
            "fun (x: Int) Int { x }",
            Program(vec![ExpressionStatement {
                expr: Expression::new(
                    ExpressionKind::Function {
                        parameters: vec![FunctionParameter::LabeledAtCallsite {
                            internal_name: "x".to_string(),
                            external_name: None,
                            ty: Type::Ident("Int".to_string()),
                        }],
                        return_type: Some(Type::Ident("Int".to_string())),
                        body: vec![ExpressionStatement {
                            expr: Expression::new(
                                ExpressionKind::Ident("x".to_string()),
                                Span::from_range(19, 20),
                            ),
                            discarded: false,
                        }],
                    },
                    Span::from_range(0, 22),
                ),
                discarded: false,
            }]),
        );

        expect_ast(
            "fun (external internal: Int) Int { internal }",
            Program(vec![ExpressionStatement {
                expr: Expression::new(
                    ExpressionKind::Function {
                        parameters: vec![FunctionParameter::LabeledAtCallsite {
                            internal_name: "internal".to_string(),
                            external_name: Some("external".to_string()),
                            ty: Type::Ident("Int".to_string()),
                        }],
                        return_type: Some(Type::Ident("Int".to_string())),
                        body: vec![ExpressionStatement {
                            expr: Expression::new(
                                ExpressionKind::Ident("internal".to_string()),
                                Span::from_range(35, 43),
                            ),
                            discarded: false,
                        }],
                    },
                    Span::from_range(0, 45),
                ),
                discarded: false,
            }]),
        );

        expect_ast(
            "fun (~x: Int) Int { x }",
            Program(vec![ExpressionStatement {
                expr: Expression::new(
                    ExpressionKind::Function {
                        parameters: vec![FunctionParameter::UnlabeledAtCallsite {
                            name: "x".to_string(),
                            ty: Type::Ident("Int".to_string()),
                        }],
                        return_type: Some(Type::Ident("Int".to_string())),
                        body: vec![ExpressionStatement {
                            expr: Expression::new(
                                ExpressionKind::Ident("x".to_string()),
                                Span::from_range(20, 21),
                            ),
                            discarded: false,
                        }],
                    },
                    Span::from_range(0, 23),
                ),
                discarded: false,
            }]),
        );
    }

    #[test]
    fn function_calls() {
        expect_ast(
            "print(3)",
            Program(vec![ExpressionStatement {
                expr: Expression::new(
                    ExpressionKind::FunctionCall {
                        name: "print".to_string(),
                        arguments: vec![FunctionArgument {
                            label: None,
                            value: Expression::new(
                                ExpressionKind::IntLiteral(3),
                                Span::from_range(6, 7),
                            ),
                        }],
                    },
                    Span::from_range(0, 8),
                ),
                discarded: false,
            }]),
        );

        expect_ast(
            "print(3, 4)",
            Program(vec![ExpressionStatement {
                expr: Expression::new(
                    ExpressionKind::FunctionCall {
                        name: "print".to_string(),
                        arguments: vec![
                            FunctionArgument {
                                label: None,
                                value: Expression::new(
                                    ExpressionKind::IntLiteral(3),
                                    Span::from_range(6, 7),
                                ),
                            },
                            FunctionArgument {
                                label: None,
                                value: Expression::new(
                                    ExpressionKind::IntLiteral(4),
                                    Span::from_range(9, 10),
                                ),
                            },
                        ],
                    },
                    Span::from_range(0, 11),
                ),
                discarded: false,
            }]),
        );

        expect_ast(
            "print(3, and: 4)",
            Program(vec![ExpressionStatement {
                expr: Expression::new(
                    ExpressionKind::FunctionCall {
                        name: "print".to_string(),
                        arguments: vec![
                            FunctionArgument {
                                label: None,
                                value: Expression::new(
                                    ExpressionKind::IntLiteral(3),
                                    Span::from_range(6, 7),
                                ),
                            },
                            FunctionArgument {
                                label: Some("and".to_string()),
                                value: Expression::new(
                                    ExpressionKind::IntLiteral(4),
                                    Span::from_range(14, 15),
                                ),
                            },
                        ],
                    },
                    Span::from_range(0, 16),
                ),
                discarded: false,
            }]),
        );
    }
}
