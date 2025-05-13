#![allow(dead_code)]

use crate::{
    parser::ast::{
        Expression, ExpressionKind, ExpressionStatement,
        InfixOperator::{self, *},
        PrefixOperator::{self, *},
        Program,
    },
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
                Equal => todo!(),
                NotEqual => todo!(),
            },
            ExpressionKind::VariableDecl { .. } => todo!(),
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
struct CheckedProgram {
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
}

#[cfg(test)]
mod tests {
    use super::{BOOL_ID, Checker, INT_ID};
    use crate::{
        checker::{
            CheckedExpression, CheckedExpressionData, CheckedExpressionStatement, CheckedProgram,
            UNIT_ID,
        },
        parser::{
            Parser,
            ast::{InfixOperator, PrefixOperator},
        },
    };
    use pretty_assertions::assert_eq;

    fn test_checked(program: &str, expected: CheckedProgram) {
        let ast = Parser::new(program).parse().unwrap();
        let mut checker = Checker::new();
        let checked_program = checker.check_program(&ast);

        assert_eq!(checked_program, expected);
    }

    #[test]
    fn single_primitive_literals() {
        // Unit
        test_checked(
            "unit;",
            CheckedProgram {
                stmts: vec![CheckedExpressionStatement {
                    expr: CheckedExpression::new(CheckedExpressionData::Unit, UNIT_ID),
                }],
            },
        );

        // Bool
        test_checked(
            "true; false;",
            CheckedProgram {
                stmts: vec![
                    CheckedExpressionStatement {
                        expr: CheckedExpression::new(
                            CheckedExpressionData::BoolLiteral(true),
                            BOOL_ID,
                        ),
                    },
                    CheckedExpressionStatement {
                        expr: CheckedExpression::new(
                            CheckedExpressionData::BoolLiteral(false),
                            BOOL_ID,
                        ),
                    },
                ],
            },
        );

        // Int
        test_checked(
            "90; 3;",
            CheckedProgram {
                stmts: vec![
                    CheckedExpressionStatement {
                        expr: CheckedExpression::new(CheckedExpressionData::IntLiteral(90), INT_ID),
                    },
                    CheckedExpressionStatement {
                        expr: CheckedExpression::new(CheckedExpressionData::IntLiteral(3), INT_ID),
                    },
                ],
            },
        );
    }

    #[test]
    fn prefix_expressions() {
        test_checked(
            "!true;",
            CheckedProgram {
                stmts: vec![CheckedExpressionStatement {
                    expr: CheckedExpression::new(
                        CheckedExpressionData::Prefix {
                            operator: PrefixOperator::LogicalNot,
                            expression: Box::new(CheckedExpression::new(
                                CheckedExpressionData::BoolLiteral(true),
                                BOOL_ID,
                            )),
                        },
                        BOOL_ID,
                    ),
                }],
            },
        );

        test_checked(
            "-312;",
            CheckedProgram {
                stmts: vec![CheckedExpressionStatement {
                    expr: CheckedExpression::new(
                        CheckedExpressionData::Prefix {
                            operator: PrefixOperator::Negative,
                            expression: Box::new(CheckedExpression::new(
                                CheckedExpressionData::IntLiteral(312),
                                INT_ID,
                            )),
                        },
                        INT_ID,
                    ),
                }],
            },
        );
    }

    #[test]
    fn infix_expressions() {
        test_checked(
            "2 + 3 * 4 - 5",
            CheckedProgram {
                stmts: vec![CheckedExpressionStatement {
                    expr: CheckedExpression {
                        type_id: INT_ID,
                        data: CheckedExpressionData::Infix {
                            left: Box::new(CheckedExpression::new(
                                CheckedExpressionData::Infix {
                                    left: Box::new(CheckedExpression::new(
                                        CheckedExpressionData::IntLiteral(2),
                                        INT_ID,
                                    )),
                                    operator: InfixOperator::Plus,
                                    right: Box::new(CheckedExpression::new(
                                        CheckedExpressionData::Infix {
                                            left: Box::new(CheckedExpression::new(
                                                CheckedExpressionData::IntLiteral(3),
                                                INT_ID,
                                            )),
                                            operator: InfixOperator::Multiply,
                                            right: Box::new(CheckedExpression::new(
                                                CheckedExpressionData::IntLiteral(4),
                                                INT_ID,
                                            )),
                                        },
                                        INT_ID,
                                    )),
                                },
                                INT_ID,
                            )),
                            operator: InfixOperator::Minus,
                            right: Box::new(CheckedExpression::new(
                                CheckedExpressionData::IntLiteral(5),
                                INT_ID,
                            )),
                        },
                    },
                }],
            },
        );

        test_checked(
            "4 > 3;",
            CheckedProgram {
                stmts: vec![CheckedExpressionStatement {
                    expr: CheckedExpression::new(
                        CheckedExpressionData::Infix {
                            left: Box::new(CheckedExpression::new(
                                CheckedExpressionData::IntLiteral(4),
                                INT_ID,
                            )),
                            operator: InfixOperator::GreaterThan,
                            right: Box::new(CheckedExpression::new(
                                CheckedExpressionData::IntLiteral(3),
                                INT_ID,
                            )),
                        },
                        BOOL_ID,
                    ),
                }],
            },
        );

        test_checked(
            "4 - -3 > 2 / 7;",
            CheckedProgram {
                stmts: vec![CheckedExpressionStatement {
                    expr: CheckedExpression::new(
                        CheckedExpressionData::Infix {
                            left: Box::new(CheckedExpression::new(
                                CheckedExpressionData::Infix {
                                    left: Box::new(CheckedExpression::new(
                                        CheckedExpressionData::IntLiteral(4),
                                        INT_ID,
                                    )),
                                    operator: InfixOperator::Minus,
                                    right: Box::new(CheckedExpression::new(
                                        CheckedExpressionData::Prefix {
                                            operator: PrefixOperator::Negative,
                                            expression: Box::new(CheckedExpression::new(
                                                CheckedExpressionData::IntLiteral(3),
                                                INT_ID,
                                            )),
                                        },
                                        INT_ID,
                                    )),
                                },
                                INT_ID,
                            )),
                            operator: InfixOperator::GreaterThan,
                            right: Box::new(CheckedExpression::new(
                                CheckedExpressionData::Infix {
                                    left: Box::new(CheckedExpression::new(
                                        CheckedExpressionData::IntLiteral(2),
                                        INT_ID,
                                    )),
                                    operator: InfixOperator::Divide,
                                    right: Box::new(CheckedExpression::new(
                                        CheckedExpressionData::IntLiteral(7),
                                        INT_ID,
                                    )),
                                },
                                INT_ID,
                            )),
                        },
                        BOOL_ID,
                    ),
                }],
            },
        );
    }
}
