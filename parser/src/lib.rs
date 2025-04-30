#![allow(dead_code)]

use ast::{Expression, InfixOperator, Precedence, Program, Statement};
use lexer::tokens::{
    Token,
    TokenKind::{self, *},
};

mod ast;

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
        let stmt = self.parse_statement()?;
        Ok(Program(vec![stmt]))
    }

    fn parse_statement(&mut self) -> Result<Statement, Error> {
        match self.cur_token() {
            Fun => todo!(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, Error> {
        Ok(Statement::Expression(
            self.parse_expression(Precedence::Lowest)?,
        ))
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
            LParen => Ok(Self::parse_grouped_expression),
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

    fn expect_int(&mut self) -> Result<isize, Error> {
        if let IntLiteral(int) = self.cur_token() {
            self.advance();
            Ok(int)
        } else {
            Err(Error::expected("int literal"))
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

    fn advance(&mut self) {
        self.position += 1;
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Expression, InfixOperator, Program, Statement},
        Parser,
    };
    use lexer::{tokens::Token, Lexer};
    use pretty_assertions::assert_eq;

    fn expect_ast(input: &str, ast: Program) {
        let lexer = Lexer::new(input);
        let tokens: Vec<Token> = lexer.collect();
        let mut parser = Parser::new(tokens);
        let parsed = parser.parse();
        assert_eq!(parsed, Ok(ast));
    }

    #[test]
    fn single_int() {
        expect_ast(
            "122",
            Program(vec![Statement::Expression(Expression::IntLiteral(122))]),
        );
    }

    #[test]
    fn basic_expressions() {
        expect_ast(
            "1 + 2 * 3",
            Program(vec![Statement::Expression(Expression::Infix {
                left: Box::new(Expression::IntLiteral(1)),
                operator: InfixOperator::Plus,
                right: Box::new(Expression::Infix {
                    left: Box::new(Expression::IntLiteral(2)),
                    operator: InfixOperator::Multiply,
                    right: Box::new(Expression::IntLiteral(3)),
                }),
            })]),
        );

        expect_ast(
            "6 - 3 - 2",
            Program(vec![Statement::Expression(Expression::Infix {
                left: Box::new(Expression::Infix {
                    left: Box::new(Expression::IntLiteral(6)),
                    operator: InfixOperator::Minus,
                    right: Box::new(Expression::IntLiteral(3)),
                }),
                operator: InfixOperator::Minus,
                right: Box::new(Expression::IntLiteral(2)),
            })]),
        );

        expect_ast(
            "(1 - (3 + 2)) * (122 - (((9))))",
            Program(vec![Statement::Expression(Expression::Infix {
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
            })]),
        );
    }
}
