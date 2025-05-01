#![allow(dead_code)]

use ast::{Expression, ExpressionStatement, InfixOperator, Precedence, Program, Type};
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
            LParen => Ok(Self::parse_grouped_expression),
            Identifier(_) => match self.peek_token() {
                Colon => Ok(Self::parse_variable_declaration),
                _ => Ok(Self::parse_variable_ident),
            },
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

    fn parse_variable_ident(&mut self) -> Result<Expression, Error> {
        Ok(Expression::Ident(self.expect_ident()?))
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

    fn parse_variable_declaration(&mut self) -> Result<Expression, Error> {
        let name = self.expect_ident()?;
        self.expect_token(Colon)?;

        let mut ty = None;
        if let Identifier(name) = self.cur_token() {
            self.advance();
            ty = Some(Type::Ident(name));
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

    fn check_semicolon(&mut self) -> bool {
        self.cur_token() == Semicolon
    }

    fn expect_token(&mut self, kind: TokenKind) -> Result<(), Error> {
        let tok = self.cur_token();
        if tok != kind {
            Err(Error::Expected(tok.to_string()))
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
        ast::{Expression, ExpressionStatement, InfixOperator, Program, Type},
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
        )
    }
}
