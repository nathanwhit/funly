use std::collections::HashMap;
use thiserror::Error;

use crate::ast::{Ident, Name, Program};

use self::syntax::{ScopeSet, ScopeSets};

mod syntax;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct Binding(u64);

#[derive(Default)]
pub struct Resolver<'ast> {
    scope_sets: ScopeSets<'ast>,
    table: BindingTable,
}

impl<'ast> Resolver<'ast> {
    pub fn new(program: Program) -> Self {
        Self::default()
    }

    pub fn resolve(&self, name: &Name) -> Binding {
        todo!()
    }
}

#[derive(Default)]
struct BindingTable {
    scopes_for_ident: HashMap<Ident, Vec<ScopeSet>>,
    bindings: HashMap<ScopeSet, Binding>,
    binding_idx: u64,
}

impl BindingTable {
    fn new() -> Self {
        Self::default()
    }

    fn add_binding(&mut self, scopes: ScopeSet) -> Binding {
        let binding = Binding(self.binding_idx);
        self.binding_idx += 1;
        self.bindings.insert(scopes, binding);
        binding
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum ResolutionError {
    #[error("Name {0:?} not found in binding table")]
    UnknownName(Name),
}
