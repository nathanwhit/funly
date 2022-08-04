use std::{
    collections::{BTreeSet, HashMap},
    marker::PhantomData,
};

use crate::ast::{
    context::NameId,
    visit::{Visit, Visitor},
    Ident, Name,
};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Scope(u64);

impl Scope {
    fn new(idx: u64) -> Scope {
        Scope(idx)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Syntax {
    ident: Ident,
    scopes: BTreeSet<Scope>,
}

impl Syntax {
    fn new(ident: Ident) -> Self {
        Self {
            ident,
            scopes: BTreeSet::new(),
        }
    }
    pub fn ident(&self) -> &Ident {
        &self.ident
    }
    pub fn is_subset(&self, other: &Syntax) -> bool {
        self.scopes.is_subset(&other.scopes)
    }
    pub fn num_scopes(&self) -> usize {
        self.scopes.len()
    }
    #[cfg(test)]
    fn contains(&self, scope: &Scope) -> bool {
        self.scopes.contains(&scope)
    }
}

#[derive(Default, Debug)]
pub struct SyntaxSets {
    scope_sets: HashMap<NameId, Syntax>,
    scope_idx: u64,
}

struct SyntaxChanger<'ast, F>
where
    F: FnMut(&'ast Name) + 'ast,
{
    action: F,
    phantom: PhantomData<&'ast ()>,
}

impl<'ast, F> SyntaxChanger<'ast, F>
where
    F: FnMut(&'ast Name) + 'ast,
{
    fn new(action: F) -> Self {
        Self {
            action,
            phantom: PhantomData,
        }
    }
}

impl<'ast, F> Visitor<'ast> for SyntaxChanger<'ast, F>
where
    F: FnMut(&'ast Name) + 'ast,
{
    fn visit_ident(&mut self, name: &'ast crate::ast::Name) {
        println!("visiting {name:?}");
        (self.action)(name)
    }
}

impl SyntaxSets {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn new_scope(&mut self) -> Scope {
        self.scope_idx += 1;
        Scope::new(self.scope_idx)
    }

    pub fn print_scopes(&self) {
        println!("{:?}", self.scope_sets);
    }

    pub fn add_scope<'me, 'ast, T: Visit<'ast> + 'ast>(&'me mut self, scope: Scope, to: &'me T)
    where
        'me: 'ast,
    {
        println!("doin it");
        let mut changer = SyntaxChanger::new(move |name| {
            let scope = scope.clone();
            self.add_scope_to_name(scope, name)
        });
        to.visit(&mut changer);
    }

    pub fn flip_scope<'me, 'ast, T: Visit<'ast> + 'ast>(&'me mut self, scope: Scope, on: &'me T)
    where
        'me: 'ast,
    {
        let mut changer = SyntaxChanger::new(move |name| self.flip_scope_for_name(scope, name));
        on.visit(&mut changer);
    }

    fn add_scope_to_name(&mut self, scope: Scope, name: &Name) {
        self.scope_sets
            .entry(name.id())
            .or_insert_with(|| Syntax::new(name.ident().clone()))
            .scopes
            .insert(scope);
    }

    fn flip_scope_for_name(&mut self, scope: Scope, name: &Name) {
        let scopes = &mut self
            .scope_sets
            .entry(name.id())
            .or_insert_with(|| Syntax::new(name.ident().clone()))
            .scopes;
        if scopes.contains(&scope) {
            scopes.remove(&scope);
        } else {
            scopes.insert(scope);
        }
    }

    pub fn get_scopes(&self, name: &Name) -> Option<&Syntax> {
        self.scope_sets.get(&name.id())
    }
}

#[cfg(test)]
mod test {
    use crate::ast::AstContext;

    use super::SyntaxSets;

    fn setup<'a>() -> (AstContext<'a>, SyntaxSets) {
        (AstContext::new(), SyntaxSets::new())
    }

    #[test]
    fn add_scope_works() {
        let (ctx, mut sets) = setup();
        let foo = ctx.name("foo");
        let expr = ctx.expr(foo.clone());
        let s = sets.new_scope();

        sets.add_scope(s, expr);
        assert!(sets.get_scopes(&foo).unwrap().contains(&s));
    }

    #[test]
    fn flip_scope_works() {
        let (ctx, mut sets) = setup();
        let foo = ctx.name("foo");
        let expr = ctx.expr(foo.clone());
        let s = sets.new_scope();

        sets.flip_scope(s, expr);
        assert!(sets.get_scopes(&foo).unwrap().contains(&s));

        sets.flip_scope(s, expr);
        assert!(!sets.get_scopes(&foo).unwrap().contains(&s));
    }
}
