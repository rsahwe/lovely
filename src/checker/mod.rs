#![allow(dead_code)]

use crate::{
    parser::ast::{Expression, ExpressionKind, ExpressionStatement, Program},
    span::Span,
};
use scopes::{Scope, ScopeId, ScopedType};

mod scopes;

type TypeId = usize;

const INT_ID: usize = 0;
const BOOL_ID: usize = 1;
const UNIT_ID: usize = 2;

struct Checker {
    scopes: Vec<Scope>,
    types: Vec<ScopedType>,
    type_errors: Vec<TypeError>,
}

#[derive(Debug)]
struct TypeError {
    span: Span,
    kind: TypeErrorKind,
}
#[derive(Debug)]
enum TypeErrorKind {
    TypeMismatch { expected: TypeId, got: TypeId },
}
impl TypeError {
    fn mismatch(expected: TypeId, got: TypeId, span: Span) -> TypeError {
        TypeError {
            span,
            kind: TypeErrorKind::TypeMismatch { expected, got },
        }
    }
}

impl Checker {
    fn new() -> Self {
        Self {
            scopes: vec![Scope { parent_scope: None }],
            types: vec![
                // builtin types
                ScopedType::new("Int", 0),
                ScopedType::new("Bool", 0),
                ScopedType::new("Unit", 0),
            ],
            type_errors: vec![],
        }
    }

    fn create_scope(&mut self, parent_id: Option<ScopeId>) -> ScopeId {
        self.scopes.push(Scope {
            parent_scope: parent_id,
        });
        self.scopes.len() - 1
    }

    fn lookup_type(&self, name: &str, scope_id: ScopeId) -> Option<TypeId> {
        let cur_scope = &self.scopes[scope_id];
        if let Some(type_id) = self
            .types
            .iter()
            .position(|t| t.scope_id == scope_id && t.name == name)
        {
            Some(type_id)
        } else if let Some(parent_id) = cur_scope.parent_scope {
            self.lookup_type(name, parent_id)
        } else {
            None
        }
    }

    fn check_program(&mut self, program: &Program) -> CheckedProgram {
        CheckedProgram {
            stmts: program
                .0
                .iter()
                .map(|s| self.check_expression_statment(s).unwrap())
                .collect(),
        }
    }

    fn check_expression_statment(
        &mut self,
        stmt: &ExpressionStatement,
    ) -> Result<CheckedExpressionStatement, TypeError> {
        Ok(CheckedExpressionStatement {
            expr: self.check_expression(&stmt.expr, None)?,
        })
    }

    fn check_expression(
        &mut self,
        expr: &Expression,
        type_hint: Option<TypeId>,
    ) -> Result<CheckedExpression, TypeError> {
        match expr.kind {
            ExpressionKind::Unit => {
                if let Some(type_hint) = type_hint {
                    if type_hint == UNIT_ID {
                        Ok(CheckedExpression::new(CheckedExpressionData::Unit, UNIT_ID))
                    } else {
                        Err(TypeError::mismatch(type_hint, UNIT_ID, expr.span))
                    }
                } else {
                    Ok(CheckedExpression::new(CheckedExpressionData::Unit, UNIT_ID))
                }
            }
            _ => todo!(),
        }
    }
}

struct CheckedProgram {
    stmts: Vec<CheckedExpressionStatement>,
}

struct CheckedExpressionStatement {
    expr: CheckedExpression,
}

struct CheckedExpression {
    type_id: TypeId,
    data: CheckedExpressionData,
}
impl CheckedExpression {
    fn new(data: CheckedExpressionData, type_id: TypeId) -> Self {
        Self { type_id, data }
    }
}

enum CheckedExpressionData {
    Unit,
    BoolLiteral(bool),
    IntLiteral(isize),
    Ident(String),
}

#[cfg(test)]
mod tests {}
