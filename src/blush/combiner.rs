use crate::{
    blush::printer,
    parser::ast::{Expression, ExpressionKind, Program, UseTailItem},
    span::Span,
};
use std::error::Error;

pub struct Combiner<'a> {
    files: Vec<(&'a str, &'a Program)>,
}

// TODO:
// - namespace imports
// - naming conflicts (might have to mangle imported variable names)

impl<'a> Combiner<'a> {
    pub const fn new(programs: Vec<(&'a str, &'a Program)>) -> Self {
        Self { files: programs }
    }

    pub fn combine(self) -> Result<Program, Box<dyn Error>> {
        printer::info(&format!("Combining {} ASTs...", self.files.len()));
        let Some(main_file) = self
            .files
            .iter()
            .find(|(name, _)| name.eq_ignore_ascii_case("main.lv"))
        else {
            return Err("No main.lv file found".into());
        };

        self.resolve_imports(main_file.1)
    }

    fn resolve_imports(&self, main_file: &Program) -> Result<Program, Box<dyn Error>> {
        let mut new_main_file = Program(vec![]);

        for expr in &main_file.0 {
            match &expr.kind {
                crate::parser::ast::ExpressionKind::Use { segments, tail } => {
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
                        todo!("namespaced imports")
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
                                        Some((segments, tail))
                                    }
                                    _ => None,
                                })
                                .filter(|(segments, tail_items)| {
                                    referenced_vars.iter().any(|(name, namespace)| {
                                        Self::matches_import(
                                            name,
                                            namespace.as_ref(),
                                            segments,
                                            tail_items,
                                        )
                                    })
                                })
                                .map(|(segments, tail_items)| {
                                    Expression::new(
                                        ExpressionKind::Use {
                                            segments: segments.clone(),
                                            tail: tail_items.clone(),
                                        },
                                        Span::from_range(0, 0), // TODO: real span
                                    )
                                })
                                .collect::<Vec<_>>();

                            new_main_file.0.append(&mut uses_used_in_corresponding_def);
                            new_main_file.0.push(corresponding_def.clone());
                        }
                    }
                }

                ExpressionKind::BoolLiteral(_)
                | ExpressionKind::IntLiteral(_)
                | ExpressionKind::Ident { .. }
                | ExpressionKind::Block(_)
                | ExpressionKind::Prefix { .. }
                | ExpressionKind::Infix { .. }
                | ExpressionKind::VariableDecl { .. }
                | ExpressionKind::Function { .. }
                | ExpressionKind::FunctionCall { .. } => {
                    // TODO: some of these will need to be handled differently, like namespaced
                    // variable idents
                    new_main_file.0.push(expr.clone());
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

    fn referenced_variables(
        expr: &Expression,
        base: Vec<(String, Option<String>)>,
    ) -> Vec<(String, Option<String>)> {
        match &expr.kind {
            ExpressionKind::BoolLiteral(_) | ExpressionKind::IntLiteral(_) => base,
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
            ExpressionKind::Use { .. } => todo!("not sure what to do here yet"),
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
