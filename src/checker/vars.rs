use super::{ScopeId, TypeId};

pub struct ScopedVariable {
    pub name: String,
    pub type_id: TypeId,
    pub scope_id: ScopeId,
    pub borrow_status: BorrowStatus,
}

impl ScopedVariable {
    pub fn new(
        name: &str,
        scope_id: ScopeId,
        type_id: TypeId,
        borrow_status: BorrowStatus,
    ) -> Self {
        Self {
            name: name.to_string(),
            scope_id,
            type_id,
            borrow_status,
        }
    }
}

pub enum BorrowStatus {
    MutablyOwned,
    ImmutableOwned,
    MutablyBorrowed,
    ImmutablyBorrowed,
}
