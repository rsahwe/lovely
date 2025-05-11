use std::fmt::Display;

use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    // keywords:
    Fun,   // fun
    Unit,  // unit
    True,  // true
    False, // false

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
    BitAnd,             // &
    BitOr,              // |
    BitXor,             // ^
    DoubleEqual,        // ==
    NotEqual,           // !=
    LessThan,           // <
    GreaterThan,        // >
    LessThanOrEqual,    // <=
    GreaterThanOrEqual, // >=

    // idents
    Identifier, // variable/type names

    IntLiteral,

    Eof,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            TokenKind::Fun => "fun",
            TokenKind::Unit => "unit",
            TokenKind::True => "true",
            TokenKind::False => "false",
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
            TokenKind::BitAnd => "&",
            TokenKind::BitOr => "|",
            TokenKind::BitXor => "^",
            TokenKind::DoubleEqual => "=",
            TokenKind::NotEqual => "!=",
            TokenKind::LessThan => "<",
            TokenKind::GreaterThan => ">",
            TokenKind::LessThanOrEqual => "<=",
            TokenKind::GreaterThanOrEqual => ">=",
            TokenKind::Identifier => "identifier",
            TokenKind::IntLiteral => "integer literal",
            TokenKind::Eof => "EOF",
        };
        f.write_str(str)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, start: usize, size: usize) -> Self {
        Token {
            kind,
            span: Span::from_range(start, start + size),
        }
    }
}
