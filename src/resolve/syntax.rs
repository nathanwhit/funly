use std::{
    collections::{BTreeSet, HashMap},
    ops::Deref,
};

use by_address::ByAddress;
use duplicate::duplicate_item;

use crate::ast::context::{ExprRef, StmtRef, TypeRef};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Scope(u64);

impl Scope {
    pub(super) fn new(idx: u64) -> Scope {
        Scope(idx)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Hash)]
pub struct ScopeSet {
    scopes: BTreeSet<Scope>,
}

pub type ScopeSetMap<T> = HashMap<ByAddress<T>, ScopeSet>;

#[derive(Default, Debug)]
pub struct ScopeSets<'ast> {
    exprs: ScopeSetMap<ExprRef<'ast>>,
    stmts: ScopeSetMap<StmtRef<'ast>>,
    types: ScopeSetMap<TypeRef<'ast>>,
}

impl<'ast> ScopeSets<'ast> {
    fn syntax_change<F, R, T>(&'ast mut self, item: &T, action: F) -> R
    where
        F: FnOnce(&'ast mut ScopeSetMap<T>) -> R,
        T: HasSyntax<'ast> + Deref + 'ast,
    {
        item.syntax_change(self, action)
    }
}

#[derive(Default, Debug)]
pub struct SyntaxSets<'ast> {
    scopes: ScopeSets<'ast>,
    scope_idx: u64,
}

impl<'ast> SyntaxSets<'ast> {
    pub fn new_scope(&mut self) -> Scope {
        self.scope_idx += 1;
        Scope::new(self.scope_idx)
    }

    pub fn add_scope<T: HasSyntax<'ast> + Deref + Copy + 'ast>(
        &'ast mut self,
        scope: Scope,
        to: T,
    ) {
        self.scopes.syntax_change(&to, |map| {
            map.entry(ByAddress(to)).or_default().scopes.insert(scope)
        });
    }

    pub fn remove_scope<T: HasSyntax<'ast> + Deref + Copy + 'ast>(
        &'ast mut self,
        scope: Scope,
        from: T,
    ) {
        self.scopes.syntax_change(&from, |map| {
            map.entry(ByAddress(from))
                .or_default()
                .scopes
                .remove(&scope);
        })
    }

    pub fn flip_scope<T: HasSyntax<'ast> + Deref + Copy + 'ast>(
        &'ast mut self,
        scope: Scope,
        on: T,
    ) {
        self.scopes.syntax_change(&on, |map| {
            let scopes = &mut map.entry(ByAddress(on)).or_default().scopes;
            if scopes.contains(&scope) {
                scopes.remove(&scope);
            } else {
                scopes.insert(scope);
            }
        })
    }
}

pub trait HasSyntax<'ast>: Sized {
    fn syntax_do<F, R>(&self, sets: &'ast mut ScopeSets<'ast>, action: F) -> R
    where
        F: FnOnce(&'ast ScopeSetMap<Self>) -> R,
        Self: Deref + 'ast;

    fn syntax_change<F, R>(&self, sets: &'ast mut ScopeSets<'ast>, action: F) -> R
    where
        F: FnOnce(&'ast mut ScopeSetMap<Self>) -> R,
        Self: Deref + 'ast;
}

#[duplicate_item(
        Ty       field;
    [ExprRef]   [exprs];
    [StmtRef]   [stmts];
    [TypeRef]   [types];
)]
impl<'ast> HasSyntax<'ast> for Ty<'ast> {
    fn syntax_do<F, R>(&self, sets: &'ast mut ScopeSets<'ast>, action: F) -> R
    where
        F: FnOnce(&'ast ScopeSetMap<Self>) -> R,
        Self: Deref + 'ast,
    {
        action(&sets.field)
    }
    fn syntax_change<F, R>(&self, sets: &'ast mut ScopeSets<'ast>, action: F) -> R
    where
        F: FnOnce(&'ast mut ScopeSetMap<Self>) -> R,
        Self: Deref + 'ast,
    {
        action(&mut sets.field)
    }
}

fn fooby() {
    let mut syntaxes = SyntaxSets::default();
    let ast = crate::ast::AstContext::new();
    let expr = ast.expr(crate::ast::make::lit(1));
    let scope = syntaxes.new_scope();
    syntaxes.add_scope(scope, expr);
}
