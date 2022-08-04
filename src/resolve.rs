use std::{collections::HashMap, ops::Not};
use thiserror::Error;

use crate::ast::{
    visit::{self, Visit, Visitor},
    Expr, Ident, Name, Program,
};

use self::syntax::{Syntax, SyntaxSets};

mod syntax;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct Binding(u64);

#[derive(Default)]
pub struct Resolver {
    scope_sets: SyntaxSets,
    table: BindingTable,
}

impl<'ast> Visitor<'ast> for Resolver {
    fn visit_stmt(&mut self, stmt: &'ast crate::ast::Stmt) {
        match stmt {
            crate::ast::Stmt::Assign { lhs, rhs } => {
                if let Expr::Ident(name) = lhs {
                    let scope = self.scope_sets.new_scope();
                    self.scope_sets.add_scope(scope, name);
                    self.scope_sets.add_scope(scope, *rhs);
                    self.add_binding(name);
                } else {
                    visit::walk_expr(self, lhs);
                    visit::walk_expr(self, rhs);
                }
            }
            s => visit::walk_stmt(self, s),
        }
    }

    fn visit_fun(&mut self, fun: &'ast crate::ast::Fun) {
        println!("HERE");
        for arg in &fun.args {
            println!("yarrr matey {arg:?}");
            let scope = self.scope_sets.new_scope();
            self.scope_sets.add_scope(scope, arg);
            self.scope_sets.add_scope(scope, fun.body);
            self.add_binding(&arg.name);
            self.scope_sets.print_scopes();
            self.visit_arg(arg);
        }
        self.visit_type(&fun.ret);
        self.visit_expr(&fun.body);
    }
}

impl Resolver {
    fn add_binding(&mut self, name: &Name) {
        let scopes = self.scope_sets.get_scopes(name).unwrap();
        self.table.add_binding(scopes.clone());
        self.table.add_syntax(name.ident().clone(), scopes.clone());
    }

    fn build_table<'ast, T: Visit<'ast> + 'ast>(&mut self, ast: &'ast T) {
        ast.visit(self);
    }

    pub fn new<'ast, T: Visit<'ast> + 'ast>(ast: &'ast T) -> Self {
        let mut resolver = Self::default();
        resolver.build_table(ast);
        resolver
    }

    pub fn resolve(&self, name: &Name) -> Result<Binding, ResolutionError> {
        let syntax = self.scope_sets.get_scopes(name).expect("unknown name!");
        let candidates: Vec<_> = self
            .table
            .bindings
            .keys()
            .filter(|cand| {
                println!("candidate");
                (cand.ident() == syntax.ident()) && cand.is_subset(&syntax)
            })
            .collect();

        let best = if let Some(&best) = candidates.iter().max_by_key(|syn| syn.num_scopes()) {
            best
        } else {
            return Err(ResolutionError::NoneFound(name.clone()));
        };

        if candidates.into_iter().any(|c| c.is_subset(best).not()) {
            return Err(ResolutionError::Ambiguous(name.clone()));
        }

        Ok(self
            .table
            .bindings
            .get(best)
            .expect("we just got it from the binding table!")
            .clone())
    }
}

#[derive(Default)]
struct BindingTable {
    scopes_for_ident: HashMap<Ident, Vec<Syntax>>,
    bindings: HashMap<Syntax, Binding>,
    binding_idx: u64,
}

impl BindingTable {
    fn add_syntax(&mut self, ident: Ident, syntax: Syntax) {
        self.scopes_for_ident.entry(ident).or_default().push(syntax);
    }
    fn add_binding(&mut self, scopes: Syntax) -> Binding {
        let binding = Binding(self.binding_idx);
        self.binding_idx += 1;
        self.bindings.insert(scopes, binding);
        println!("bindings {:?}", self.bindings);
        binding
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum ResolutionError {
    #[error("Name {0:?} not found in binding table")]
    UnknownName(Name),

    #[error("No candidate bindings found for {0:?}")]
    NoneFound(Name),

    #[error("Resolution was ambigious for {0:?}")]
    Ambiguous(Name),
}

#[cfg(test)]
mod test {
    use crate::{
        ast::{Arg, AstContext, Fun, Type},
        resolve::Binding,
    };

    use super::Resolver;

    #[test]
    fn fun_body_resolution() {
        let ctx = AstContext::new();
        let arg_foo = ctx.name("foo");
        let body_foo = ctx.name("foo");
        let fun = ctx.expr(Fun {
            args: vec![Arg {
                name: arg_foo.clone(),
                ty: ctx.ty(Type::Int),
            }],
            ret: ctx.ty(Type::Int),
            body: ctx.expr(body_foo.clone()),
        });

        let resolver = Resolver::new(fun);

        let binding = resolver.resolve(&body_foo).unwrap();
        assert_eq!(binding, Binding(0));
        assert_eq!(binding, resolver.resolve(&arg_foo).unwrap());
    }

    // #[test]
    // fn block_resolution() {
    //     let ctx = AstContext::new();
    // }
}
