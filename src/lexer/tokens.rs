use crate::span::Span;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    // keywords:
    Fun,    // fun
    Use,    // use
    As,     // as
    Take,   // take
    Mut,    // mut
    Read,   // read
    Give,   // give
    Break,  // break
    Return, // ret
    Proto,  // proto
    Impl,   // impl
    Type,   // type
    True,   // true
    False,  // false
    If,     // if
    Else,   // else

    // syntax
    LParen,      // (
    RParen,      // )
    LBrace,      // {
    RBrace,      // }
    Colon,       // :
    Comma,       // ,
    Tilde,       // ~
    SingleEqual, // =
    RArrow,      // ->
    Slash,       // /
    Dot,         // .
    Hash,        // #

    // operators:
    ExclamationMark,    // !
    Plus,               // +
    Minus,              // -
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
            Self::Fun => "fun",
            Self::Use => "use",
            Self::As => "as",
            Self::Take => "take",
            Self::Mut => "mut",
            Self::Read => "read",
            Self::Give => "give",
            Self::Break => "break",
            Self::Return => "ret",
            Self::Proto => "proto",
            Self::Impl => "impl",
            Self::Type => "type",
            Self::True => "true",
            Self::False => "false",
            Self::If => "if",
            Self::Else => "else",
            Self::LParen => "(",
            Self::RParen => ")",
            Self::LBrace => "{",
            Self::RBrace => "}",
            Self::Colon => ":",
            Self::Comma => ",",
            Self::Tilde => "~",
            Self::RArrow => "->",
            Self::SingleEqual => "=",
            Self::Slash => "/",
            Self::Dot => ".",
            Self::Hash => "#",
            Self::ExclamationMark => "!",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Asterisk => "*",
            Self::BitAnd => "&",
            Self::BitOr => "|",
            Self::BitXor => "^",
            Self::DoubleEqual => "==",
            Self::NotEqual => "!=",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::LessThanOrEqual => "<=",
            Self::GreaterThanOrEqual => ">=",
            Self::Identifier => "identifier",
            Self::IntLiteral => "integer literal",
            Self::Eof => "EOF",
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
    pub const fn new(kind: TokenKind, start: usize, size: usize) -> Self {
        Self {
            kind,
            span: Span::from_range(start, start + size),
        }
    }
}
