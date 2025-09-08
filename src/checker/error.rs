use super::TypeId;
use crate::{parser::ast::Type, span::Span};

#[derive(Debug)]
pub struct Error {
    pub span: Span,
    pub kind: ErrorKind,
}
#[derive(Debug)]
pub enum ErrorKind {
    TypeMismatch {
        expected: TypeId,
        got: TypeId,
    },
    VariableNotFound {
        name: String,
    },
    TypeNotFound {
        ty: Type,
    },
    VariableIsNotAFunction {
        name: String,
    },
    IncorrectArgCount {
        expected: usize,
        got: usize,
    },
    IncorrectArgLabel {
        expected: Option<String>,
        got: Option<String>,
    },
}
impl Error {
    pub const fn type_mismatch(expected: TypeId, got: TypeId, span: Span) -> Self {
        Self {
            span,
            kind: ErrorKind::TypeMismatch { expected, got },
        }
    }
    pub fn variable_not_found(name: &str, span: Span) -> Self {
        Self {
            span,
            kind: ErrorKind::VariableNotFound {
                name: name.to_string(),
            },
        }
    }
    pub const fn type_not_found(ty: Type, span: Span) -> Self {
        Self {
            span,
            kind: ErrorKind::TypeNotFound { ty },
        }
    }
    pub fn variable_is_not_a_function(name: &str, span: Span) -> Self {
        Self {
            span,
            kind: ErrorKind::VariableIsNotAFunction {
                name: name.to_string(),
            },
        }
    }
    pub const fn incorrect_arg_count(expected: usize, got: usize, span: Span) -> Self {
        Self {
            span,
            kind: ErrorKind::IncorrectArgCount { expected, got },
        }
    }
    pub const fn incorrect_arg_label(
        expected: Option<String>,
        got: Option<String>,
        span: Span,
    ) -> Self {
        Self {
            span,
            kind: ErrorKind::IncorrectArgLabel { expected, got },
        }
    }
}
