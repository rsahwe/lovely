#![allow(dead_code)]

use crate::{
    parser::ast::{
        Expression, ExpressionKind, ExpressionStatement,
        InfixOperator::{self, *},
        PrefixOperator::{self, *},
        Program, Type,
    },
    span::Span,
};
use scopes::{Scope, ScopeId, ScopedType};

mod scopes;

type TypeId = usize;

const INT_ID: usize = 0;
const BOOL_ID: usize = 1;
const UNIT_ID: usize = 2;

pub struct Checker {
    cur_scope: ScopeId,
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
    pub fn new() -> Self {
        Self {
            cur_scope: 0,
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

    fn check_typename(&self, ty: &Type, scope_id: ScopeId) -> Option<TypeId> {
        let cur_scope = &self.scopes[scope_id];
        let Type::Ident(name) = ty;
        if let Some(type_id) = self
            .types
            .iter()
            .position(|t| t.scope_id == scope_id && &t.name == name)
        {
            Some(type_id)
        } else if let Some(parent_id) = cur_scope.parent_scope {
            self.check_typename(ty, parent_id)
        } else {
            None
        }
    }

    pub fn check_program(&mut self, program: &Program) -> CheckedProgram {
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
        match &expr.kind {
            ExpressionKind::Unit => {
                self.typed_expression(CheckedExpressionData::Unit, expr.span, UNIT_ID, type_hint)
            }
            ExpressionKind::BoolLiteral(value) => self.typed_expression(
                CheckedExpressionData::BoolLiteral(*value),
                expr.span,
                BOOL_ID,
                type_hint,
            ),
            ExpressionKind::IntLiteral(value) => self.typed_expression(
                CheckedExpressionData::IntLiteral(*value),
                expr.span,
                INT_ID,
                type_hint,
            ),
            ExpressionKind::Prefix {
                operator,
                expression,
            } => match operator {
                LogicalNot => {
                    let res = self.check_expression(expression, Some(BOOL_ID))?;
                    self.typed_expression(
                        CheckedExpressionData::Prefix {
                            operator: *operator,
                            expression: Box::new(res),
                        },
                        expr.span,
                        BOOL_ID,
                        type_hint,
                    )
                }
                Negative => {
                    let res = self.check_expression(expression, Some(INT_ID))?;
                    self.typed_expression(
                        CheckedExpressionData::Prefix {
                            operator: *operator,
                            expression: Box::new(res),
                        },
                        expr.span,
                        INT_ID,
                        type_hint,
                    )
                }
            },
            ExpressionKind::Infix {
                left,
                operator,
                right,
            } => match operator {
                // ints
                Plus | Minus | Divide | Multiply => {
                    let left = self.check_expression(left, Some(INT_ID))?;
                    let right = self.check_expression(right, Some(INT_ID))?;
                    self.typed_expression(
                        CheckedExpressionData::Infix {
                            left: Box::new(left),
                            operator: *operator,
                            right: Box::new(right),
                        },
                        expr.span,
                        INT_ID,
                        type_hint,
                    )
                }
                LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual => {
                    let left = self.check_expression(left, Some(INT_ID))?;
                    let right = self.check_expression(right, Some(INT_ID))?;
                    self.typed_expression(
                        CheckedExpressionData::Infix {
                            left: Box::new(left),
                            operator: *operator,
                            right: Box::new(right),
                        },
                        expr.span,
                        BOOL_ID,
                        type_hint,
                    )
                }

                // anything
                Equal | NotEqual => {
                    let left = self.check_expression(left, None)?;
                    let right = self.check_expression(right, None)?;
                    if left.type_id != right.type_id {
                        Err(TypeError::mismatch(left.type_id, right.type_id, expr.span))
                    } else {
                        self.typed_expression(
                            CheckedExpressionData::Infix {
                                left: Box::new(left),
                                operator: *operator,
                                right: Box::new(right),
                            },
                            expr.span,
                            BOOL_ID,
                            type_hint,
                        )
                    }
                }
            },
            ExpressionKind::VariableDecl {
                name,
                value,
                mutable,
                ty,
            } => {
                let r_value = if let Some(ty) = ty {
                    self.check_expression(value, self.check_typename(ty, self.cur_scope))?
                } else {
                    self.check_expression(value, None)?
                };
                self.typed_expression(
                    CheckedExpressionData::VariableDecl {
                        name: name.to_string(),
                        value: Box::new(r_value),
                        mutable: *mutable,
                    },
                    expr.span,
                    UNIT_ID,
                    None,
                )
            }
            ExpressionKind::Ident(_) => todo!(),
            ExpressionKind::Function { .. } => todo!(),
            ExpressionKind::FunctionCall { .. } => todo!(),
        }
    }

    fn typed_expression(
        &self,
        res: CheckedExpressionData,
        span: Span,
        res_type: TypeId,
        type_hint: Option<TypeId>,
    ) -> Result<CheckedExpression, TypeError> {
        if let Some(type_hint) = type_hint {
            if type_hint == res_type {
                Ok(CheckedExpression::new(res, res_type))
            } else {
                Err(TypeError::mismatch(type_hint, res_type, span))
            }
        } else {
            Ok(CheckedExpression::new(res, res_type))
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct CheckedProgram {
    stmts: Vec<CheckedExpressionStatement>,
}

#[derive(PartialEq, Eq, Debug)]
struct CheckedExpressionStatement {
    expr: CheckedExpression,
}

#[derive(PartialEq, Eq, Debug)]
struct CheckedExpression {
    type_id: TypeId,
    data: CheckedExpressionData,
}
impl CheckedExpression {
    pub fn new(data: CheckedExpressionData, type_id: TypeId) -> Self {
        Self { type_id, data }
    }
}

#[derive(PartialEq, Eq, Debug)]
enum CheckedExpressionData {
    Unit,
    BoolLiteral(bool),
    IntLiteral(isize),
    Ident(String),

    Prefix {
        operator: PrefixOperator,
        expression: Box<CheckedExpression>,
    },
    Infix {
        left: Box<CheckedExpression>,
        operator: InfixOperator,
        right: Box<CheckedExpression>,
    },

    VariableDecl {
        name: String,
        value: Box<CheckedExpression>,
        mutable: bool,
    },
}
