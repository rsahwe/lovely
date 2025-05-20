use super::TypeId;
use crate::{parser::ast::Type, span::Span};

#[derive(Debug)]
pub struct Error {
    pub span: Span,
    pub kind: ErrorKind,
}
#[derive(Debug)]
pub enum ErrorKind {
    TypeMismatch { expected: TypeId, got: TypeId },
    VariableNotFound { name: String },
    TypeNotFound { ty: Type },
    VariableIsNotAFunction { name: String },
    IncorrectArgCount { expected: usize, got: usize },
    IncorrectArgLabel { expected: String, got: String },
}
impl Error {
    pub fn type_mismatch(expected: TypeId, got: TypeId, span: Span) -> Error {
        Error {
            span,
            kind: ErrorKind::TypeMismatch { expected, got },
        }
    }
    pub fn variable_not_found(name: &str, span: Span) -> Error {
        Error {
            span,
            kind: ErrorKind::VariableNotFound {
                name: name.to_string(),
            },
        }
    }
    pub fn type_not_found(ty: Type, span: Span) -> Error {
        Error {
            span,
            kind: ErrorKind::TypeNotFound { ty },
        }
    }
    pub fn variable_is_not_a_function(name: &str, span: Span) -> Error {
        Error {
            span,
            kind: ErrorKind::VariableIsNotAFunction {
                name: name.to_string(),
            },
        }
    }
    pub fn incorrect_arg_count(expected: usize, got: usize, span: Span) -> Error {
        Error {
            span,
            kind: ErrorKind::IncorrectArgCount { expected, got },
        }
    }
    pub fn incorrect_arg_label(expected: String, got: String, span: Span) -> Error {
        Error {
            span,
            kind: ErrorKind::IncorrectArgLabel { expected, got },
        }
    }
}
