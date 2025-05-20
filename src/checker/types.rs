use super::ScopeId;

#[derive(Clone)]
pub struct ScopedType {
    pub kind: TypeKind,
    pub scope_id: ScopeId,
}

impl ScopedType {
    pub fn named(name: &str, scope_id: ScopeId) -> Self {
        ScopedType {
            kind: TypeKind::Name(name.to_string()),
            scope_id,
        }
    }
}

#[derive(Clone)]
pub enum TypeKind {
    Name(String),
    Function {
        parameters: Vec<FunctionParameterType>,
        return_type: Box<ScopedType>,
    },
}

#[derive(Clone)]
pub struct FunctionParameterType {
    pub kind: FunctionParameterTypeKind,
    pub ty: ScopedType,
}

#[derive(Clone)]
pub enum FunctionParameterTypeKind {
    Owned,
    ReadOnlyRef,
    MutableRef,
}
