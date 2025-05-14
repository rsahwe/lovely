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
    use super::{CheckedProgram, Checker};
    use crate::parser::Parser;
    use insta::assert_debug_snapshot as snap;

    fn checked_program(program: &str) -> CheckedProgram {
        let ast = Parser::new(program).parse().unwrap();
        let mut checker = Checker::new();
        checker.check_program(&ast)
    }

    #[test]
    fn single_primitive_literals() {
        snap!(checked_program("unit;"), @r"
        CheckedProgram {
            stmts: [
                CheckedExpressionStatement {
                    expr: CheckedExpression {
                        type_id: 2,
                        data: Unit,
                    },
                },
            ],
        }
        ");
        snap!(checked_program("unit;"), @r"
        CheckedProgram {
            stmts: [
                CheckedExpressionStatement {
                    expr: CheckedExpression {
                        type_id: 2,
                        data: Unit,
                    },
                },
            ],
        }
        ");
        snap!(checked_program("true; false;"), @r"
        CheckedProgram {
            stmts: [
                CheckedExpressionStatement {
                    expr: CheckedExpression {
                        type_id: 1,
                        data: BoolLiteral(
                            true,
                        ),
                    },
                },
                CheckedExpressionStatement {
                    expr: CheckedExpression {
                        type_id: 1,
                        data: BoolLiteral(
                            false,
                        ),
                    },
                },
            ],
        }
        ");
        snap!(checked_program("90; 3;"), @r"
        CheckedProgram {
            stmts: [
                CheckedExpressionStatement {
                    expr: CheckedExpression {
                        type_id: 0,
                        data: IntLiteral(
                            90,
                        ),
                    },
                },
                CheckedExpressionStatement {
                    expr: CheckedExpression {
                        type_id: 0,
                        data: IntLiteral(
                            3,
                        ),
                    },
                },
            ],
        }
        ");
    }

    #[test]
    fn prefix_expressions() {
        snap!(checked_program("!true;"), @r"
        CheckedProgram {
            stmts: [
                CheckedExpressionStatement {
                    expr: CheckedExpression {
                        type_id: 1,
                        data: Prefix {
                            operator: LogicalNot,
                            expression: CheckedExpression {
                                type_id: 1,
                                data: BoolLiteral(
                                    true,
                                ),
                            },
                        },
                    },
                },
            ],
        }
        ");
        snap!(checked_program("-312;"), @r"
        CheckedProgram {
            stmts: [
                CheckedExpressionStatement {
                    expr: CheckedExpression {
                        type_id: 0,
                        data: Prefix {
                            operator: Negative,
                            expression: CheckedExpression {
                                type_id: 0,
                                data: IntLiteral(
                                    312,
                                ),
                            },
                        },
                    },
                },
            ],
        }
        ");
    }

    #[test]
    fn infix_expressions() {
        snap!(checked_program("2 + 3 * 4 - 5"), @r"
        CheckedProgram {
            stmts: [
                CheckedExpressionStatement {
                    expr: CheckedExpression {
                        type_id: 0,
                        data: Infix {
                            left: CheckedExpression {
                                type_id: 0,
                                data: Infix {
                                    left: CheckedExpression {
                                        type_id: 0,
                                        data: IntLiteral(
                                            2,
                                        ),
                                    },
                                    operator: Plus,
                                    right: CheckedExpression {
                                        type_id: 0,
                                        data: Infix {
                                            left: CheckedExpression {
                                                type_id: 0,
                                                data: IntLiteral(
                                                    3,
                                                ),
                                            },
                                            operator: Multiply,
                                            right: CheckedExpression {
                                                type_id: 0,
                                                data: IntLiteral(
                                                    4,
                                                ),
                                            },
                                        },
                                    },
                                },
                            },
                            operator: Minus,
                            right: CheckedExpression {
                                type_id: 0,
                                data: IntLiteral(
                                    5,
                                ),
                            },
                        },
                    },
                },
            ],
        }
        ");
        snap!(checked_program("4 > 3;"), @r"
        CheckedProgram {
            stmts: [
                CheckedExpressionStatement {
                    expr: CheckedExpression {
                        type_id: 1,
                        data: Infix {
                            left: CheckedExpression {
                                type_id: 0,
                                data: IntLiteral(
                                    4,
                                ),
                            },
                            operator: GreaterThan,
                            right: CheckedExpression {
                                type_id: 0,
                                data: IntLiteral(
                                    3,
                                ),
                            },
                        },
                    },
                },
            ],
        }
        ");
        snap!(checked_program("4 - -3 > 2 / 7;"), @r"
        CheckedProgram {
            stmts: [
                CheckedExpressionStatement {
                    expr: CheckedExpression {
                        type_id: 1,
                        data: Infix {
                            left: CheckedExpression {
                                type_id: 0,
                                data: Infix {
                                    left: CheckedExpression {
                                        type_id: 0,
                                        data: IntLiteral(
                                            4,
                                        ),
                                    },
                                    operator: Minus,
                                    right: CheckedExpression {
                                        type_id: 0,
                                        data: Prefix {
                                            operator: Negative,
                                            expression: CheckedExpression {
                                                type_id: 0,
                                                data: IntLiteral(
                                                    3,
                                                ),
                                            },
                                        },
                                    },
                                },
                            },
                            operator: GreaterThan,
                            right: CheckedExpression {
                                type_id: 0,
                                data: Infix {
                                    left: CheckedExpression {
                                        type_id: 0,
                                        data: IntLiteral(
                                            2,
                                        ),
                                    },
                                    operator: Divide,
                                    right: CheckedExpression {
                                        type_id: 0,
                                        data: IntLiteral(
                                            7,
                                        ),
                                    },
                                },
                            },
                        },
                    },
                },
            ],
        }
        ");
    }
}
