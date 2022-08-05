//! Name resolution
//!
//! We use the [Binding as Sets of Scopes](https://www.cs.utah.edu/plt/scope-sets/) model
//! for its simplicity and to set us up well for macro support in the future.
use std::{collections::HashMap, ops::Not};
use thiserror::Error;

use crate::ast::{
    visit::{self, Visit, Visitor},
    Expr, Ident, Name,
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
    fn visit_expr(&mut self, expr: &'ast Expr) {
        match expr {
            Expr::Block(stmts) => {
                let mut pending_scopes = Vec::new();
                for &stmt in stmts {
                    self.scope_sets.add_scopes(&pending_scopes, stmt);
                    match stmt {
                        crate::ast::Stmt::Assign {
                            lhs: Expr::Ident(name),
                            rhs,
                        } => {
                            let scope = self.scope_sets.new_scope();
                            self.scope_sets.add_scope(scope, name);
                            self.scope_sets.add_scope(scope, *rhs);
                            self.add_binding(name);
                            pending_scopes.push(scope);
                        }
                        s => visit::walk_stmt(self, s),
                    }
                }
            }
            expr => visit::walk_expr(self, expr),
        }
    }

    fn visit_fun(&mut self, fun: &'ast crate::ast::Fun) {
        for arg in &fun.args {
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
            .filter(|cand| (cand.ident() == syntax.ident()) && cand.is_subset(&syntax))
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
        ast::{make, Arg, AstContext, Fun, Type},
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

    #[test]
    fn block_resolution() {
        let ctx = AstContext::new();
        let assign_foo = ctx.name("foo");
        let block_foo = ctx.name("foo");
        let nested_block_foo = ctx.name("foo");

        let block = ctx.expr(vec![
            ctx.assign(assign_foo.clone(), make::lit(1)),
            ctx.assign(ctx.name("bar"), block_foo.clone()),
            ctx.semi_stmt(vec![ctx.expr_stmt(nested_block_foo.clone())]),
        ]);

        let resolver = Resolver::new(block);

        let binding = resolver.resolve(&assign_foo).unwrap();
        let block_binding = resolver.resolve(&block_foo).unwrap();
        let nested_block_binding = resolver.resolve(&nested_block_foo).unwrap();

        assert_eq!(binding, Binding(0));
        assert_eq!(binding, block_binding);
        assert_eq!(binding, nested_block_binding);
    }

    #[test]
    fn shadowing() {
        let ctx = AstContext::new();
        let assign_foo = ctx.name("foo");
        let block_foo = ctx.name("foo");
        let fun_foo = ctx.name("foo");
        let body_foo = ctx.name("foo");

        let block = ctx.expr(vec![
            ctx.assign(assign_foo.clone(), make::lit(1)),
            ctx.semi_stmt(block_foo.clone()),
            ctx.expr_stmt(Fun {
                args: vec![Arg {
                    name: fun_foo.clone(),
                    ty: ctx.ty(Type::Int),
                }],
                ret: ctx.ty(Type::Int),
                body: ctx.expr(body_foo.clone()),
            }),
        ]);

        let resolver = Resolver::new(block);

        let binding = resolver.resolve(&assign_foo).unwrap();
        let block_binding = resolver.resolve(&block_foo).unwrap();
        let fun_binding = resolver.resolve(&fun_foo).unwrap();
        let body_binding = resolver.resolve(&body_foo).unwrap();

        assert_eq!(binding, block_binding);
        assert_eq!(fun_binding, body_binding);
        assert_ne!(binding, fun_binding);
    }
}
