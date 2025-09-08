use super::{ScopeId, TypeId};

#[derive(Clone, Eq, PartialEq)]
pub struct ScopedType {
    pub kind: TypeKind,
    pub scope_id: ScopeId,
}

impl ScopedType {
    pub fn named(name: &str, scope_id: ScopeId) -> Self {
        Self {
            kind: TypeKind::Name(name.to_string()),
            scope_id,
        }
    }
}

#[derive(Clone, Eq, PartialEq)]
pub enum TypeKind {
    Name(String),
    Function {
        parameters: Vec<FunctionParameterType>,
        return_type: TypeId,
    },
}

#[derive(Clone, Eq, PartialEq)]
pub struct FunctionParameterType {
    pub kind: FunctionParameterTypeKind,
    pub callsite_label: Option<String>,
    pub ty: TypeId,
}

#[derive(Clone, Eq, PartialEq)]
pub enum FunctionParameterTypeKind {
    Owned,
    ReadOnlyRef,
    MutableRef,
}
