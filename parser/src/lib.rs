#![allow(dead_code)]

use ast::{Expression, Precedence, Program, Statement, VariableDecl};
use lexer::tokens::{Token, TokenKind::*};

mod ast;

#[derive(Debug, PartialEq, Eq)]
enum Error {
    NoToken,
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
        match self.get_token()?.kind {
            Val => todo!(),
            Mut => todo!(),
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
        todo!()
    }

    fn get_token(&mut self) -> Result<&Token, Error> {
        if let Some(tok) = self.tokens.get(self.position) {
            self.position += 1;
            Ok(tok)
        } else {
            Err(Error::NoToken)
        }
    }

    fn advance(&mut self) {
        self.position += 1;
    }
}

#[cfg(test)]
mod tests {
    use lexer::{tokens::Token, Lexer};
    use pretty_assertions::assert_eq;

    use crate::{
        ast::{Expression, InfixExpression, InfixOperator, Program, Statement},
        Parser,
    };

    #[test]
    fn basic_expressions() {
        let input = "1 + 2 * 3";
        let lexer = Lexer::new(input);
        let tokens: Vec<Token> = lexer.collect();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();
        assert_eq!(
            ast,
            Ok(Program(vec![Statement::Expression(Expression::Infix(
                InfixExpression {
                    operator: InfixOperator::Plus,
                    lh_expression: Box::new(Expression::IntLiteral(1)),
                    rh_expression: Box::new(Expression::Infix(InfixExpression {
                        operator: InfixOperator::Multiply,
                        lh_expression: Box::new(Expression::IntLiteral(2)),
                        rh_expression: Box::new(Expression::IntLiteral(3)),
                    })),
                }
            ))]))
        )
    }
}
