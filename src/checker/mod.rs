#![allow(dead_code)]

use crate::{
    parser::ast::{
        Expression, ExpressionKind, ExpressionStatement, FunctionParameter,
        InfixOperator::{self, *},
        PrefixOperator::{self, *},
        Program, Type,
    },
    span::Span,
};
use scopes::{Scope, ScopeId, ScopedType, ScopedVariable};

mod scopes;

pub type TypeId = usize;
type VariableId = usize;

const INT_ID: usize = 0;
const BOOL_ID: usize = 1;
const UNIT_ID: usize = 2;

pub struct Checker {
    cur_scope: ScopeId,
    scopes: Vec<Scope>,
    types: Vec<ScopedType>,
    variables: Vec<ScopedVariable>,
    type_errors: Vec<Error>,
}

#[derive(Debug)]
struct Error {
    span: Span,
    kind: ErrorKind,
}
#[derive(Debug)]
enum ErrorKind {
    TypeMismatch { expected: TypeId, got: TypeId },
    VariableNotFound { name: String },
    TypeNotFound { ty: Type },
}
impl Error {
    fn type_mismatch(expected: TypeId, got: TypeId, span: Span) -> Error {
        Error {
            span,
            kind: ErrorKind::TypeMismatch { expected, got },
        }
    }
    fn variable_not_found(name: &str, span: Span) -> Error {
        Error {
            span,
            kind: ErrorKind::VariableNotFound {
                name: name.to_string(),
            },
        }
    }
    fn type_not_found(ty: Type, span: Span) -> Error {
        Error {
            span,
            kind: ErrorKind::TypeNotFound { ty },
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
                ScopedType::named("Int", 0),
                ScopedType::named("Bool", 0),
                ScopedType::named("Unit", 0),
            ],
            variables: vec![],
            type_errors: vec![],
        }
    }

    fn create_scope(&mut self, parent_id: Option<ScopeId>) -> ScopeId {
        self.scopes.push(Scope {
            parent_scope: parent_id,
        });
        self.scopes.len() - 1
    }

    fn check_type_name(&self, ty: &Type, scope_id: ScopeId) -> Option<TypeId> {
        let cur_scope = &self.scopes[scope_id];
        let Type::Ident(name) = ty;
        if let Some(type_id) = self
            .types
            .iter()
            .position(|t| t.scope_id == scope_id && &t.name == name)
        {
            Some(type_id)
        } else if let Some(parent_id) = cur_scope.parent_scope {
            self.check_type_name(ty, parent_id)
        } else {
            None
        }
    }

    fn check_variable_name(
        &self,
        var_name: &str,
        scope_id: ScopeId,
    ) -> Option<(VariableId, TypeId)> {
        let cur_scope = &self.scopes[scope_id];
        if let Some((index, variable)) = self
            .variables
            .iter()
            .enumerate()
            .find(|t| t.1.scope_id == scope_id && t.1.name == var_name)
        {
            Some((index, variable.type_id))
        } else if let Some(parent_id) = cur_scope.parent_scope {
            self.check_variable_name(var_name, parent_id)
        } else {
            None
        }
    }

    fn add_variable(&mut self, var_name: &str, var_type: TypeId) -> VariableId {
        let variable_id = self.variables.len();
        self.variables
            .push(ScopedVariable::new(var_name, self.cur_scope, var_type));
        variable_id
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
    ) -> Result<CheckedExpressionStatement, Error> {
        Ok(CheckedExpressionStatement {
            discarded: stmt.discarded,
            expr: self.check_expression(&stmt.expr, None)?,
        })
    }

    fn check_expression(
        &mut self,
        expr: &Expression,
        type_hint: Option<TypeId>,
    ) -> Result<CheckedExpression, Error> {
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
                        Err(Error::type_mismatch(left.type_id, right.type_id, expr.span))
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
                    self.check_expression(value, self.check_type_name(ty, self.cur_scope))?
                } else {
                    self.check_expression(value, None)?
                };
                let id = self.add_variable(name, r_value.type_id);
                self.typed_expression(
                    CheckedExpressionData::VariableDecl {
                        name: name.to_string(),
                        value: Box::new(r_value),
                        mutable: *mutable,
                        variable_id: id,
                    },
                    expr.span,
                    UNIT_ID,
                    type_hint,
                )
            }
            ExpressionKind::Ident(name) => {
                if let Some((var_id, var_type)) = self.check_variable_name(name, self.cur_scope) {
                    self.typed_expression(
                        CheckedExpressionData::Ident {
                            name: name.to_string(),
                            variable_id: var_id,
                        },
                        expr.span,
                        var_type,
                        type_hint,
                    )
                } else {
                    Err(Error::variable_not_found(name, expr.span))
                }
            }
            ExpressionKind::Function {
                parameters,
                return_type,
                body,
            } => {
                // create a new scope
                let new_scope = self.create_scope(Some(self.cur_scope));
                self.cur_scope = new_scope;

                let mut checked_params = vec![];

                // add params as local variables in said scope
                for param in parameters {
                    match param {
                        FunctionParameter::LabeledAtCallsite {
                            internal_name,
                            external_name,
                            ty,
                        } => {
                            if let Some(type_id) = self.check_type_name(ty, self.cur_scope) {
                                checked_params.push(CheckedFunctionParameter::LabeledAtCallsite {
                                    internal_name: internal_name.clone(),
                                    external_name: external_name.clone(),
                                    type_id,
                                });
                                self.add_variable(internal_name, type_id);
                            } else {
                                return Err(Error::type_not_found(ty.clone(), expr.span));
                            }
                        }
                        FunctionParameter::UnlabeledAtCallsite { name, ty } => {
                            if let Some(type_id) = self.check_type_name(ty, self.cur_scope) {
                                checked_params.push(
                                    CheckedFunctionParameter::UnlabeledAtCallsite {
                                        name: name.clone(),
                                        type_id,
                                    },
                                );
                                self.add_variable(name, type_id);
                            } else {
                                return Err(Error::type_not_found(ty.clone(), expr.span));
                            }
                        }
                    }
                }

                // check the body
                let checked_expr_stmts = body
                    .iter()
                    .map(|s| self.check_expression_statment(s).unwrap())
                    .collect::<Vec<_>>();

                let Some(last_stmt) = checked_expr_stmts.last() else {
                    todo!("empty function body, only valid if return type is unit");
                };
                if last_stmt.discarded {
                    todo!(
                        "discarded last statement in function body, only valid if return type is unit"
                    );
                }

                // get return type
                let return_type = if let Some(ty) = return_type {
                    ty
                } else {
                    &Type::Ident("Unit".to_string())
                };
                let Some(return_type_id) = self.check_type_name(return_type, self.cur_scope) else {
                    return Err(Error::type_not_found(return_type.clone(), expr.span));
                };

                // check if the last statement is of the same type as the return type
                if last_stmt.expr.type_id != return_type_id {
                    return Err(Error::type_mismatch(
                        return_type_id,
                        last_stmt.expr.type_id,
                        expr.span,
                    ));
                }

                self.typed_expression(
                    CheckedExpressionData::Function {
                        parameters: checked_params,
                        return_type: return_type_id,
                        body: checked_expr_stmts,
                    },
                    expr.span,
                    todo!("function types"),
                    type_hint,
                )
            }
            ExpressionKind::FunctionCall { .. } => todo!(),
        }
    }

    fn typed_expression(
        &self,
        res: CheckedExpressionData,
        span: Span,
        res_type: TypeId,
        type_hint: Option<TypeId>,
    ) -> Result<CheckedExpression, Error> {
        if let Some(type_hint) = type_hint {
            if type_hint == res_type {
                Ok(CheckedExpression::new(res, res_type))
            } else {
                Err(Error::type_mismatch(type_hint, res_type, span))
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
    discarded: bool,
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
    Ident {
        name: String,
        variable_id: VariableId,
    },

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
        variable_id: VariableId,
    },

    Function {
        parameters: Vec<CheckedFunctionParameter>,
        return_type: TypeId,
        body: Vec<CheckedExpressionStatement>,
    },
}

#[derive(PartialEq, Eq, Debug)]
pub enum CheckedFunctionParameter {
    LabeledAtCallsite {
        internal_name: String,
        external_name: Option<String>,
        type_id: TypeId,
    },
    UnlabeledAtCallsite {
        name: String,
        type_id: TypeId,
    },
}
