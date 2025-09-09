#![allow(dead_code)]

use crate::parser::ast::UseTailItem;
#[allow(clippy::enum_glob_use)]
use crate::{
    lexer::{
        Lexer,
        tokens::TokenKind::{self, *},
    },
    span::Span,
};
use ast::{
    Expression, ExpressionKind, FunctionArgument, FunctionParameter, InfixOperator,
    ParameterModifier, Precedence, PrefixOperator, Program, Type,
};
use std::{fmt::Display, iter::Peekable};

pub mod ast;

type PrefixParseFn = Box<dyn Fn(&mut Parser) -> Result<Expression, Error>>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Error {
    NoToken,
    NoPrefixParseFn(TokenKind),
    Expected { expected: String, got: String },
    Syntax(String),
    UnexpectedEof,
    IllegalGlobalExpression(Expression),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoToken => write!(f, "no token"),
            Self::NoPrefixParseFn(token) => {
                write!(f, "no prefix parse function for token {token:?}")
            }
            Self::Expected { expected, got } => write!(f, "expected {expected}, got {got}"),
            Self::Syntax(msg) => write!(f, "syntax error: {msg}"),
            Self::UnexpectedEof => write!(f, "unexpected end of file"),
            Self::IllegalGlobalExpression(expr) => {
                write!(f, "illegal global expression: {expr:?}")
            }
        }
    }
}

impl std::error::Error for Error {}

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
    inside_function: usize,
}

impl<'src> Parser<'src> {
    pub fn new(source: &'src str) -> Self {
        let lexer = Lexer::new(source).peekable();
        Self {
            source: source.to_string(),
            lexer,
            inside_function: 0,
        }
    }

    pub fn parse(&mut self) -> Result<Program, Error> {
        let mut exprs = vec![];
        while self.lexer.peek().is_some() {
            exprs.push(self.parse_expression(&Precedence::Lowest)?);
        }
        Ok(Program(exprs))
    }

    fn parse_expression(&mut self, precedence: &Precedence) -> Result<Expression, Error> {
        let mut expr = self.prefix_parse_fn()?(self)?;

        while &self.cur_precedence() > precedence {
            match &self.peek_kind() {
                IntLiteral => return Err(Error::syntax_err("consecutive ints")),
                Eof => return Ok(expr),
                Plus => {
                    expr =
                        self.parse_infix_expression(expr, InfixOperator::Plus, &Precedence::Sum)?;
                }
                Minus => {
                    expr =
                        self.parse_infix_expression(expr, InfixOperator::Minus, &Precedence::Sum)?;
                }
                Asterisk => {
                    expr = self.parse_infix_expression(
                        expr,
                        InfixOperator::Multiply,
                        &Precedence::Product,
                    )?;
                }
                Slash => {
                    expr = self.parse_infix_expression(
                        expr,
                        InfixOperator::Divide,
                        &Precedence::Product,
                    )?;
                }
                GreaterThan => {
                    expr = self.parse_infix_expression(
                        expr,
                        InfixOperator::GreaterThan,
                        &Precedence::Comparison,
                    )?;
                }
                LessThan => {
                    expr = self.parse_infix_expression(
                        expr,
                        InfixOperator::LessThan,
                        &Precedence::Comparison,
                    )?;
                }
                GreaterThanOrEqual => {
                    expr = self.parse_infix_expression(
                        expr,
                        InfixOperator::GreaterThanOrEqual,
                        &Precedence::Comparison,
                    )?;
                }
                LessThanOrEqual => {
                    expr = self.parse_infix_expression(
                        expr,
                        InfixOperator::LessThanOrEqual,
                        &Precedence::Comparison,
                    )?;
                }
                DoubleEqual => {
                    expr = self.parse_infix_expression(
                        expr,
                        InfixOperator::Equal,
                        &Precedence::Equality,
                    )?;
                }
                NotEqual => {
                    expr = self.parse_infix_expression(
                        expr,
                        InfixOperator::NotEqual,
                        &Precedence::Equality,
                    )?;
                }
                tok => return Err(Error::syntax_err(&format!("invalid operator: {tok}"))),
            }
        }

        if expr.kind.is_const() || self.inside_function != 0 {
            Ok(expr)
        } else {
            Err(Error::IllegalGlobalExpression(expr))
        }
    }

    #[allow(clippy::redundant_closure_for_method_calls)]
    fn prefix_parse_fn(&mut self) -> Result<PrefixParseFn, Error> {
        let peek_token_kind = self.peek_kind();
        match peek_token_kind {
            IntLiteral => Ok(Box::new(|parser| parser.parse_int_literal())),
            True | False => Ok(Box::new(|parser| parser.parse_bool_literal())),
            LBrace => Ok(Box::new(|parser| parser.parse_block_expression())),
            Identifier => {
                let (name, span) = self.expect_ident()?;
                match self.peek_kind() {
                    Colon => Ok(Box::new(move |parser| {
                        parser.parse_variable_declaration(&name, span.start)
                    })),
                    Hash => {
                        self.expect_token(&Hash)?;
                        let (sub_name, _) = self.expect_ident()?;
                        match self.peek_kind() {
                            LParen => Ok(Box::new(move |parser| {
                                parser.parse_function_call(&sub_name, Some(&name), span.start)
                            })),
                            _ => Ok(Box::new(move |_| {
                                Ok(Parser::parse_variable_ident(&sub_name, Some(&name), span))
                            })),
                        }
                    }
                    LParen => Ok(Box::new(move |parser| {
                        parser.parse_function_call(&name, None, span.start)
                    })),
                    _ => Ok(Box::new(move |_| {
                        Ok(Parser::parse_variable_ident(&name, None, span))
                    })),
                }
            }
            Fun => Ok(Box::new(|parser| parser.parse_function_expression())),
            ExclamationMark => Ok(Box::new(|parser| {
                parser.parse_prefix_expression(PrefixOperator::LogicalNot)
            })),
            Use => Ok(Box::new(|parser| parser.parse_use_expression())),
            _ => Err(Error::NoPrefixParseFn(peek_token_kind.clone())),
        }
    }

    fn parse_prefix_expression(&mut self, operator: PrefixOperator) -> Result<Expression, Error> {
        let Span { start, .. } = self.expect_token(match operator {
            PrefixOperator::LogicalNot => &ExclamationMark,
        })?;
        let expr = self.parse_expression(&Precedence::Prefix)?;
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
        precedence: &Precedence,
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
                let span = self.expect_token(&True)?;
                Ok(Expression::new(ExpressionKind::BoolLiteral(true), span))
            }
            False => {
                let span = self.expect_token(&False)?;
                Ok(Expression::new(ExpressionKind::BoolLiteral(false), span))
            }
            tok => Err(Error::expected("true or false", &tok.to_string())),
        }
    }

    fn parse_variable_ident(name: &str, namespace: Option<&str>, span: Span) -> Expression {
        Expression::new(
            ExpressionKind::Ident {
                name: name.to_string(),
                namespace: namespace.map(std::string::ToString::to_string),
            },
            span,
        )
    }

    fn parse_type(&mut self) -> Result<Type, Error> {
        let (name, _) = self.expect_ident()?;
        Ok(Type::Ident(name))
    }

    fn parse_block_expression(&mut self) -> Result<Expression, Error> {
        let start_span = self.expect_token(&LBrace)?;
        let mut exprs = vec![];
        while self.peek_kind() != &RBrace {
            exprs.push(self.parse_expression(&Precedence::Lowest)?);
        }
        let end_span = self.expect_token(&RBrace)?;
        Ok(Expression::new(
            ExpressionKind::Block(exprs),
            Span::from_range(start_span.start, end_span.end),
        ))
    }

    fn parse_function_call(
        &mut self,
        fn_name: &str,
        namespace: Option<&str>,
        start_position: usize,
    ) -> Result<Expression, Error> {
        self.expect_token(&LParen)?;

        let mut arguments = vec![];

        while self.peek_kind() != &RParen {
            arguments.push(self.parse_function_argument()?);
            if self.peek_kind() == &Comma {
                self.expect_token(&Comma)?;
            } else {
                break;
            }
        }

        let end_span = self.expect_token(&RParen)?;

        Ok(Expression::new(
            ExpressionKind::FunctionCall {
                name: fn_name.to_string(),
                namespace: namespace.map(std::string::ToString::to_string),
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
        if tok.kind == Identifier {
            let (name, _) = self.expect_ident()?;
            if self.peek_kind() == &Colon {
                self.expect_token(&Colon)?;
                label = Some(name);
                value = self.parse_expression(&Precedence::Lowest)?;
            } else if self.peek_kind() == &Hash {
                self.expect_token(&Hash)?;
                let (sub_name, _) = self.expect_ident()?;
                value = Expression::new(
                    ExpressionKind::Ident {
                        name: sub_name,
                        namespace: Some(name),
                    },
                    span,
                );
            } else {
                value = Expression::new(
                    ExpressionKind::Ident {
                        name,
                        namespace: None,
                    },
                    span,
                );
            }
        } else {
            value = self.parse_expression(&Precedence::Lowest)?;
        }

        Ok(FunctionArgument { label, value })
    }

    fn parse_variable_declaration(
        &mut self,
        name: &str,
        start_position: usize,
    ) -> Result<Expression, Error> {
        self.expect_token(&Colon)?;

        let ty = if self.peek_kind() == &Identifier || self.peek_kind() == &Fun {
            Some(self.parse_type()?)
        } else {
            None
        };

        #[allow(clippy::needless_late_init)]
        let value: Expression;
        let mutable: bool;

        match self.peek_kind() {
            Colon => {
                self.expect_token(&Colon)?;
                mutable = false;
                value = self.parse_expression(&Precedence::Lowest)?;
            }
            SingleEqual => {
                self.expect_token(&SingleEqual)?;
                mutable = true;
                value = self.parse_expression(&Precedence::Lowest)?;
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

    fn parse_use_expression(&mut self) -> Result<Expression, Error> {
        let start_span = self.expect_token(&Use)?;
        let mut end_span;

        let mut segments = vec![];
        let mut tail = vec![];

        let (name, span) = self.expect_ident()?;
        segments.push(name);
        end_span = span;

        while self.peek_kind() == &Slash {
            self.expect_token(&Slash)?;
            let (name, span) = self.expect_ident()?;
            segments.push(name);
            end_span = span;
        }

        if self.peek_kind() == &Hash {
            self.expect_token(&Hash)?;
            self.expect_token(&LParen)?;
            tail.push(self.parse_use_expression_tail_item()?);
            while self.peek_kind() == &Comma {
                self.expect_token(&Comma)?;
                tail.push(self.parse_use_expression_tail_item()?);
            }
            let span = self.expect_token(&RParen)?;
            end_span = span;
        }

        Ok(Expression::new(
            ExpressionKind::Use { segments, tail },
            Span::from_range(start_span.start, end_span.end),
        ))
    }

    fn parse_use_expression_tail_item(&mut self) -> Result<UseTailItem, Error> {
        let (name, _) = self.expect_ident()?;
        let alias = if self.peek_kind() == &As {
            self.expect_token(&As)?;
            let (alias_name, _) = self.expect_ident()?;
            Some(alias_name)
        } else {
            None
        };

        Ok(UseTailItem { name, alias })
    }

    fn parse_function_expression(&mut self) -> Result<Expression, Error> {
        let start_span = self.expect_token(&Fun)?;
        self.expect_token(&LParen)?;

        let mut parameters = vec![];

        while self.peek_kind() != &RParen {
            parameters.push(self.parse_function_parameter()?);
            if self.peek_kind() == &Comma {
                self.expect_token(&Comma)?;
            } else {
                break;
            }
        }

        self.expect_token(&RParen)?;

        let return_type = if matches!(self.peek_kind(), RArrow) {
            self.expect_token(&RArrow)?;
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect_token(&Colon)?;

        self.inside_function += 1;
        let body = self.parse_expression(&Precedence::Lowest)?;
        self.inside_function -= 1;

        let end_span = body.span;

        Ok(Expression::new(
            ExpressionKind::Function {
                parameters,
                return_type,
                body: Box::new(body),
            },
            Span::from_range(start_span.start, end_span.end),
        ))
    }

    fn parse_function_parameter(&mut self) -> Result<FunctionParameter, Error> {
        let mut modifier = ParameterModifier::Read;

        match self.peek_kind() {
            Mut => {
                modifier = ParameterModifier::Mut;
                self.expect_token(&Mut)?;
            }
            Take => {
                modifier = ParameterModifier::Take;
                self.expect_token(&Take)?;
            }
            Read => {
                self.expect_token(&Read)?;
            }
            _ => {}
        }

        match self.peek_kind() {
            Tilde => {
                self.expect_token(&Tilde)?;
                let (name, _) = self.expect_ident()?;
                self.expect_token(&Colon)?;
                let ty = self.parse_type()?;
                Ok(FunctionParameter {
                    modifier,
                    internal_name: name,
                    ty,
                    labeled_at_callsite: false,
                    external_name: None,
                })
            }
            Identifier => {
                let (first, _) = self.expect_ident()?;
                let second = if matches!(self.peek_kind(), Identifier) {
                    let (name, _) = self.expect_ident()?;
                    Some(name)
                } else {
                    None
                };
                self.expect_token(&Colon)?;
                let ty = self.parse_type()?;
                Ok(FunctionParameter {
                    modifier,
                    internal_name: second
                        .as_ref()
                        .map_or_else(|| first.clone(), std::string::ToString::to_string),
                    external_name: if second.is_some() { Some(first) } else { None },
                    ty,
                    labeled_at_callsite: true,
                })
            }
            tok => Err(Error::expected("parameter name", &tok.to_string())),
        }
    }

    fn expect_token(&mut self, kind: &TokenKind) -> Result<Span, Error> {
        let tok = self.lexer.next().ok_or(Error::UnexpectedEof)?;
        if &tok.kind == kind {
            Ok(tok.span)
        } else {
            Err(Error::syntax_err(&format!(
                "unexpected token: {}",
                tok.kind
            )))
        }
    }

    fn expect_int(&mut self) -> Result<(isize, Span), Error> {
        let token = self.lexer.peek().ok_or(Error::UnexpectedEof)?;
        let span = token.span;
        if token.kind == IntLiteral {
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
        if kind == Identifier {
            self.lexer.next();
            // up to you whether you want to return a `String` or a `&str`
            Ok((span.slice(&self.source).to_owned(), span))
        } else {
            Err(Error::expected("identifier", &kind.to_string()))
        }
    }

    fn cur_precedence(&mut self) -> Precedence {
        match self.peek_kind() {
            DoubleEqual | NotEqual => Precedence::Equality,
            LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual => Precedence::Comparison,
            Plus | Minus => Precedence::Sum,
            Asterisk | Slash => Precedence::Product,
            LParen => Precedence::Group,
            _ => Precedence::Lowest,
        }
    }

    fn cur_kind(&mut self) -> TokenKind {
        self.lexer.next().map_or(TokenKind::Eof, |t| t.kind)
    }

    fn peek_kind(&mut self) -> &TokenKind {
        self.lexer.peek().map_or(&TokenKind::Eof, |t| &t.kind)
    }
}
