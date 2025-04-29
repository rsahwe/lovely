use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
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
    Divide,             // /
    Multiply,           // *
    Exponent,           // ^
    Equal,              // =
    NotEqual,           // !=
    LessThan,           // <
    GreaterThan,        // >
    LessThanOrEqual,    // <=
    GreaterThanOrEqual, // >=

    // idents
    Identifier(String), // variable/type names

    IntLiteral(usize),

    Eof,
}

#[derive(Clone)]
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
