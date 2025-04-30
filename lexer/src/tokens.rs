use std::fmt::Display;

use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    // keywords:
    Val, // val
    Mut, // mut
    Fun, // fun

    // syntax
    LArrow,  // <-
    RArrow,  // ->
    LParen,  // (
    RParen,  // )
    LBrace,  // {
    RBrace,  // }
    Colon,   // :
    Comma,   // ,
    Tilde,   // ~
    Newline, // \n

    // operators:
    Not,                // !
    Plus,               // +
    Minus,              // -
    Slash,              // /
    Asterisk,           // *
    Exponent,           // ^
    Equal,              // =
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
            TokenKind::Val => "val",
            TokenKind::Mut => "mut",
            TokenKind::Fun => "fun",
            TokenKind::LArrow => "<-",
            TokenKind::RArrow => "->",
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::LBrace => "{",
            TokenKind::RBrace => "}",
            TokenKind::Colon => ":",
            TokenKind::Comma => ",",
            TokenKind::Tilde => "~",
            TokenKind::Newline => "\\n",
            TokenKind::Not => "!",
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Slash => "/",
            TokenKind::Asterisk => "*",
            TokenKind::Exponent => "^",
            TokenKind::Equal => "=",
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
