use std::fmt::Display;

use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    // keywords:
    Fun, // fun

    // syntax
    LParen,    // (
    RParen,    // )
    LBrace,    // {
    RBrace,    // }
    Colon,     // :
    Comma,     // ,
    Tilde,     // ~
    Semicolon, // ;
    Equal,     // =

    // operators:
    Not,                // !
    Plus,               // +
    Minus,              // -
    Slash,              // /
    Asterisk,           // *
    Exponent,           // ^
    DoubleEqual,        // ==
    NotEqual,           // !=
    LessThan,           // <
    GreaterThan,        // >
    LessThanOrEqual,    // <=
    GreaterThanOrEqual, // >=

    // idents
    Identifier(String), // variable/type names

    IntLiteral(isize),

    Eof,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            TokenKind::Fun => "fun",
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::LBrace => "{",
            TokenKind::RBrace => "}",
            TokenKind::Colon => ":",
            TokenKind::Comma => ",",
            TokenKind::Tilde => "~",
            TokenKind::Semicolon => ";",
            TokenKind::Equal => "=",
            TokenKind::Not => "!",
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Slash => "/",
            TokenKind::Asterisk => "*",
            TokenKind::Exponent => "^",
            TokenKind::DoubleEqual => "=",
            TokenKind::NotEqual => "!=",
            TokenKind::LessThan => "<",
            TokenKind::GreaterThan => ">",
            TokenKind::LessThanOrEqual => "<=",
            TokenKind::GreaterThanOrEqual => ">=",
            TokenKind::Identifier(i) => i,
            TokenKind::IntLiteral(i) => &format!("{i}"),
            TokenKind::Eof => "EOF",
        };
        f.write_str(str)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub position: Span,
}

impl Token {
    pub fn new(kind: TokenKind, start: usize, size: usize) -> Self {
        Token {
            kind,
            position: Span {
                start,
                end: start + size - 1,
            },
        }
    }
}
