use super::TypeId;

pub type ScopeId = usize;

pub struct Scope {
    pub parent_scope: Option<ScopeId>,
}

pub struct ScopedVariable {
    pub name: String,
    pub type_id: TypeId,
    pub scope_id: ScopeId,
}
impl ScopedVariable {
    pub fn new(name: &str, scope_id: ScopeId, type_id: TypeId) -> Self {
        ScopedVariable {
            name: name.to_string(),
            scope_id,
            type_id,
        }
    }
}

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

pub enum TypeKind {
    Name(String),
    Function {
        parameters: Vec<FunctionParameterType>,
        return_type: Box<ScopedType>,
    },
}

pub struct FunctionParameterType {
    kind: FunctionParameterTypeKind,
    ty: ScopedType,
}

pub enum FunctionParameterTypeKind {
    Owned,
    ReadOnlyRef,
    MutableRef,
}
