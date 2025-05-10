pub type ScopeId = usize;

pub struct Scope {
    pub parent_scope: Option<ScopeId>,
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
