use crate::parser::ast::{Expression, ExpressionKind, FunctionArgument, Program, UseTailItem};
use std::error::Error;

pub struct Combiner<'a> {
    files: Vec<(&'a str, &'a Program)>,
}

impl<'a> Combiner<'a> {
    pub const fn new(programs: Vec<(&'a str, &'a Program)>) -> Self {
        Self { files: programs }
    }

    pub fn combine(self) -> Result<Program, Box<dyn Error>> {
        let Some(main_file) = self
            .files
            .iter()
            .find(|(name, _)| name.eq_ignore_ascii_case("main.lv"))
        else {
            return Err("No main.lv file found".into());
        };

        self.resolve_imports(main_file.1)
    }

    #[allow(clippy::too_many_lines)]
    fn resolve_imports(&self, main_file: &Program) -> Result<Program, Box<dyn Error>> {
        let mut new_main_file = Program(vec![]);

        for expr in &main_file.0 {
            match &expr.kind {
                ExpressionKind::Use { segments, tail } => {
                    let mut ref_program_name = segments.join("/");
                    ref_program_name.push_str(".lv");
                    let Some((_, ref_program)) = self
                        .files
                        .iter()
                        .find(|(name, _)| name == &ref_program_name)
                    else {
                        return Err(
                            format!("Couldn't find reference program {ref_program_name}").into(),
                        );
                    };

                    if tail.is_empty() {
                        let mut variables_in_main_file = vec![];
                        for expr in &main_file.0 {
                            variables_in_main_file.extend(Self::referenced_variables(expr, vec![]));
                        }
                        let Some(namespace_name) = segments.last() else {
                            return Err("Invalid use statement (no segments)".into());
                        };
                        let variables_to_import = variables_in_main_file
                            .iter()
                            .filter_map(|(name, namespace)| {
                                if namespace.as_ref() == Some(namespace_name) {
                                    Some(name)
                                } else {
                                    None
                                }
                            })
                            .collect::<Vec<_>>();

                        let mut definitions_from_ref_program = ref_program
                            .0
                            .iter()
                            .filter_map(|expr| match &expr.kind {
                                ExpressionKind::VariableDecl {
                                    name,
                                    value,
                                    mutable,
                                    ty,
                                } => {
                                    if variables_to_import.contains(&name) {
                                        Some(Expression::new(
                                            ExpressionKind::VariableDecl {
                                                // mangle import name
                                                name: format!("{namespace_name}_{name}"),
                                                value: value.clone(),
                                                mutable: *mutable,
                                                ty: ty.clone(),
                                            },
                                            expr.span,
                                        ))
                                    } else {
                                        None
                                    }
                                }
                                _ => None,
                            })
                            .collect::<Vec<_>>();

                        let vars_referenced_in_ref_program_defs = definitions_from_ref_program
                            .iter()
                            .flat_map(|expr| Self::referenced_variables(expr, vec![]))
                            .collect::<Vec<_>>();

                        let mut uses_used_in_ref_program_defs = ref_program
                            .0
                            .iter()
                            .filter_map(|expr| match &expr.kind {
                                ExpressionKind::Use { segments, tail } => {
                                    if vars_referenced_in_ref_program_defs.iter().any(
                                        |(name, namespace)| {
                                            Self::matches_import(
                                                name,
                                                namespace.as_ref(),
                                                segments,
                                                tail,
                                            )
                                        },
                                    ) {
                                        Some(expr.clone())
                                    } else {
                                        None
                                    }
                                }
                                _ => None,
                            })
                            .collect::<Vec<_>>();

                        new_main_file.0.append(&mut uses_used_in_ref_program_defs);
                        new_main_file.0.append(&mut definitions_from_ref_program);
                    } else {
                        for tail_item in tail {
                            let Some(corresponding_def) =
                                ref_program.0.iter().find(|expr| match &expr.kind {
                                    ExpressionKind::VariableDecl { name, .. } => {
                                        name == &tail_item.name
                                    }
                                    _ => false,
                                })
                            else {
                                return Err(format!(
                                    "Couldn't find corresponding definition for {tail_item:?}"
                                )
                                .into());
                            };

                            let referenced_vars =
                                Self::referenced_variables(corresponding_def, vec![]);

                            let mut uses_used_in_corresponding_def = ref_program
                                .0
                                .iter()
                                .filter_map(|expr| match &expr.kind {
                                    ExpressionKind::Use { segments, tail } => {
                                        if referenced_vars.iter().any(|(name, namespace)| {
                                            Self::matches_import(
                                                name,
                                                namespace.as_ref(),
                                                segments,
                                                tail,
                                            )
                                        }) {
                                            Some(expr.clone())
                                        } else {
                                            None
                                        }
                                    }
                                    _ => None,
                                })
                                .collect::<Vec<_>>();

                            new_main_file.0.append(&mut uses_used_in_corresponding_def);
                            new_main_file.0.push(corresponding_def.clone());
                        }
                    }
                }
                _ => {
                    new_main_file.0.push(Self::mangle_var_names(expr));
                }
            }
        }

        if new_main_file
            .0
            .iter()
            .any(|expr| matches!(&expr.kind, ExpressionKind::Use { .. }))
        {
            self.resolve_imports(&new_main_file)
        } else {
            Ok(new_main_file)
        }
    }

    fn mangle_var_names(expr: &Expression) -> Expression {
        match &expr.kind {
            ExpressionKind::Ident { name, namespace } => namespace.as_ref().map_or_else(
                || expr.clone(),
                |namespace| {
                    Expression::new(
                        ExpressionKind::Ident {
                            name: format!("{namespace}_{name}"),
                            namespace: None,
                        },
                        expr.span,
                    )
                },
            ),
            ExpressionKind::Block(expressions) => Expression::new(
                ExpressionKind::Block(expressions.iter().map(Self::mangle_var_names).collect()),
                expr.span,
            ),
            ExpressionKind::Prefix {
                operator,
                expression,
            } => Expression::new(
                ExpressionKind::Prefix {
                    operator: *operator,
                    expression: Box::new(Self::mangle_var_names(expression)),
                },
                expr.span,
            ),
            ExpressionKind::Infix {
                left,
                operator,
                right,
            } => Expression::new(
                ExpressionKind::Infix {
                    left: Box::new(Self::mangle_var_names(left)),
                    operator: *operator,
                    right: Box::new(Self::mangle_var_names(right)),
                },
                expr.span,
            ),
            ExpressionKind::VariableDecl {
                name,
                value,
                mutable,
                ty,
            } => Expression::new(
                ExpressionKind::VariableDecl {
                    name: name.clone(),
                    value: Box::new(Self::mangle_var_names(value)),
                    mutable: *mutable,
                    ty: ty.clone(),
                },
                expr.span,
            ),
            ExpressionKind::Function {
                parameters,
                return_type,
                body,
            } => Expression::new(
                ExpressionKind::Function {
                    parameters: parameters.clone(),
                    return_type: return_type.clone(),
                    body: Box::new(Self::mangle_var_names(body)),
                },
                expr.span,
            ),
            ExpressionKind::FunctionCall {
                name,
                namespace,
                arguments,
            } => Expression::new(
                ExpressionKind::FunctionCall {
                    name: name.clone(),
                    namespace: namespace.clone(),
                    arguments: arguments
                        .iter()
                        .map(|arg| FunctionArgument {
                            label: arg.label.clone(),
                            value: Self::mangle_var_names(&arg.value),
                        })
                        .collect(),
                },
                expr.span,
            ),
            ExpressionKind::BoolLiteral(_)
            | ExpressionKind::IntLiteral(_)
            | ExpressionKind::Use { .. } => expr.clone(),
        }
    }

    fn referenced_variables(
        expr: &Expression,
        base: Vec<(String, Option<String>)>,
    ) -> Vec<(String, Option<String>)> {
        match &expr.kind {
            ExpressionKind::BoolLiteral(_)
            | ExpressionKind::IntLiteral(_)
            | ExpressionKind::Use { .. } => base,
            ExpressionKind::Ident { name, namespace } => {
                let mut new_base = base;
                new_base.push((name.clone(), namespace.clone()));
                new_base
            }
            ExpressionKind::Block(expressions) => expressions
                .iter()
                .fold(base, |acc, expr| Self::referenced_variables(expr, acc)),
            ExpressionKind::Prefix { expression, .. } => {
                Self::referenced_variables(expression, base)
            }
            ExpressionKind::Infix { left, right, .. } => {
                let mut new_base = Self::referenced_variables(left, base);
                new_base = Self::referenced_variables(right, new_base);
                new_base
            }
            ExpressionKind::VariableDecl { value, .. } => Self::referenced_variables(value, base),
            ExpressionKind::Function { body, .. } => Self::referenced_variables(body, base),
            ExpressionKind::FunctionCall {
                name,
                namespace,
                arguments,
            } => {
                let mut new_base = base;
                new_base.push((name.clone(), namespace.clone()));
                arguments.iter().fold(new_base, |acc, arg| {
                    Self::referenced_variables(&arg.value, acc)
                })
            }
        }
    }

    fn matches_import(
        name: &str,
        namespace: Option<&String>,
        segments: &[String],
        tail_items: &[UseTailItem],
    ) -> bool {
        namespace.as_ref().map_or_else(
            || {
                tail_items.iter().any(|tail_item| {
                    tail_item
                        .alias
                        .as_ref()
                        .map_or_else(|| tail_item.name == name, |alias| alias == name)
                })
            },
            |namespace| tail_items.is_empty() && segments.last() == Some(namespace),
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{blush::combiner::Combiner, parser::ast::UseTailItem};

    #[test]
    fn test_matches_import() {
        // normal variable import
        assert!(Combiner::matches_import(
            "foo",
            None,
            &["lorem".to_string(), "ipsum".to_string()],
            &[UseTailItem {
                name: "foo".to_string(),
                alias: None
            }]
        ));

        // missing variable import
        assert!(!Combiner::matches_import(
            "foo",
            None,
            &["lorem".to_string(), "ipsum".to_string()],
            &[]
        ));

        // aliased variable import
        assert!(Combiner::matches_import(
            "bar",
            None,
            &["lorem".to_string(), "ipsum".to_string()],
            &[UseTailItem {
                name: "foo".to_string(),
                alias: Some("bar".to_string())
            }]
        ));

        // aliased but using non-aliased name should fail
        assert!(!Combiner::matches_import(
            "foo",
            None,
            &["lorem".to_string(), "ipsum".to_string()],
            &[UseTailItem {
                name: "foo".to_string(),
                alias: Some("bar".to_string())
            }]
        ));

        // namespaced import
        assert!(Combiner::matches_import(
            "foo",
            Some(&"ipsum".to_string()),
            &["lorem".to_string(), "ipsum".to_string()],
            &[]
        ));
    }
}
