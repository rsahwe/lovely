#![allow(dead_code)]

use std::ops::Deref;

use crate::{
    parser::ast::{Expression, ExpressionStatement, Program},
    span::Span,
};

type ScopeId = usize;
type TypeId = usize;

struct Scope {
    parent_scope: Option<ScopeId>,
}

struct Checker {
    scopes: Vec<Scope>,
    types: Vec<ScopedType>,
}

struct ScopedType {
    name: String,
    scope_id: ScopeId,
}
impl ScopedType {
    fn new(name: &str, scope_id: ScopeId) -> Self {
        ScopedType {
            name: name.to_string(),
            scope_id,
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
        }
    }

    fn create_scope(&mut self, parent_id: Option<ScopeId>) -> ScopeId {
        self.scopes.push(Scope {
            parent_scope: parent_id,
        });
        self.scopes.len() - 1
    }

    fn lookup_type(&self, name: String, scope_id: ScopeId) -> Option<TypeId> {
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
            span: todo!(),
            stmts: program
                .0
                .iter()
                .map(|s| self.check_expression_statment(s))
                .collect(),
        }
    }

    fn check_expression_statment(
        &mut self,
        stmt: &ExpressionStatement,
    ) -> CheckedExpressionStatement {
        CheckedExpressionStatement {
            span: todo!(),
            expr: self.check_expression(&stmt.expr),
        }
    }

    // TODO: add `type_hint`
    fn check_expression(&mut self, expr: &Expression) -> CheckedExpression {
        match expr {
            _ => todo!(),
        }
    }
}

struct CheckedProgram {
    span: Span,
    stmts: Vec<CheckedExpressionStatement>,
}

struct CheckedExpressionStatement {
    span: Span,
    expr: CheckedExpression,
}

struct CheckedExpression {
    span: Span,
    type_id: TypeId,
    data: CheckedExpressionData,
}

enum CheckedExpressionData {
    Unit,
    BoolLiteral(bool),
    IntLiteral(isize),
    Ident(String),
}

#[cfg(test)]
mod tests {}
