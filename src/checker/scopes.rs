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
    pub name: String,
    pub scope_id: ScopeId,
}
impl ScopedType {
    pub fn new(name: &str, scope_id: ScopeId) -> Self {
        ScopedType {
            name: name.to_string(),
            scope_id,
        }
    }
}
