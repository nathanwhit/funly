use by_address::ByAddress;
use dashmap::DashMap;
use thiserror::Error;
use tracing::instrument;

use crate::{
    ast::{
        context::{ExprRef, StmtRef, TypeRef},
        visit::{Visit, Visitor},
        AstCtx, Name, Op, Type,
    },
    resolve::{Binding, ResolutionError, Resolver},
};

pub struct TypeCtx<'a> {
    binding_types: DashMap<Binding, TypeRef<'a>>,
    expr_types: DashMap<ByAddress<ExprRef<'a>>, TypeRef<'a>>,
    stmt_types: DashMap<ByAddress<StmtRef<'a>>, TypeRef<'a>>,
    ast: &'a AstCtx<'a>,
    resolver: &'a Resolver,
}

impl<'a> TypeCtx<'a> {
    pub fn new(ast: &'a AstCtx<'a>, resolver: &'a Resolver) -> Self {
        Self {
            binding_types: DashMap::new(),
            expr_types: DashMap::new(),
            stmt_types: DashMap::new(),
            ast,
            resolver,
        }
    }

    pub fn resolve(&self, name: &Name) -> Result<Binding, TypeError<'a>> {
        self.resolver.resolve(name).map_err(Into::into)
    }

    pub fn type_of_name(&self, name: &Name) -> Result<Option<TypeRef<'a>>, TypeError<'a>> {
        let binding = self.resolver.resolve(name)?;

        Ok(self.type_of_binding(binding))
    }

    pub fn type_of_binding(&self, binding: Binding) -> Option<TypeRef<'a>> {
        self.binding_types.get(&binding).map(|e| *e.value())
    }

    #[instrument(skip(self))]
    pub fn type_of_stmt(&self, stmt: StmtRef<'a>) -> Result<TypeRef<'a>, TypeError<'a>> {
        if let Some(ty) = self.stmt_types.get(&ByAddress(stmt)) {
            return Ok(ty.value());
        }

        let ty = match stmt {
            crate::ast::Stmt::Assign { lhs, rhs } => {
                let lhs = self.type_of(lhs)?;
                let rhs = self.type_of(rhs)?;
                type_eq(lhs, rhs)?;
                self.ast.ty(Type::Unit)
            }
            crate::ast::Stmt::Expr(expr) => self.type_of(expr)?,
            crate::ast::Stmt::Semi(expr) => {
                let _ = self.type_of(expr)?;
                self.ast.ty(Type::Unit)
            }
            crate::ast::Stmt::Bind { name, rhs } => {
                let binding = self.resolver.resolve(name)?;
                if let Some(ty) = self.type_of_binding(binding) {
                    eprintln!("Shadowing {name:?} : {ty:?}");
                }

                let rhs_ty = self.type_of(rhs)?;

                self.binding_types.insert(binding, rhs_ty);

                self.ast.ty(Type::Unit)
            }
        };

        self.stmt_types.insert(ByAddress(stmt), ty);

        Ok(ty)
    }

    #[instrument(skip(self))]
    pub fn type_of(&self, expr: ExprRef<'a>) -> Result<TypeRef<'a>, TypeError<'a>> {
        if let Some(ty) = self.expr_types.get(&ByAddress(expr)) {
            return Ok(ty.value());
        }

        let ty = match expr {
            crate::ast::Expr::Block(stmts) => {
                for s in stmts {
                    let _ = self.type_of_stmt(s);
                }
                if let Some(stmt) = stmts.last() {
                    self.type_of_stmt(stmt)?
                } else {
                    self.ast.ty(Type::Unit)
                }
            }
            crate::ast::Expr::Fun(fun) => {
                for arg in &fun.args {
                    let binding = self.resolve(&arg.name)?;
                    self.binding_types.insert(binding, arg.ty);
                }
                self.ast.ty(Type::Fun {
                    args: fun.args.iter().map(|a| a.ty).collect(),
                    ret: fun.ret,
                })
            }
            crate::ast::Expr::Call(call) => {
                let fun_ty = self
                    .type_of_name(&call.fun)?
                    .ok_or_else(|| TypeError::Unknown(format!("{:?}", call.fun)))?;

                match fun_ty {
                    Type::Fun { args, ret } => {
                        let arg_tys: Vec<_> = call
                            .args
                            .iter()
                            .map(|e| self.type_of(e))
                            .collect::<Result<_, _>>()?;
                        args_eq(&*args, &arg_tys)?;

                        ret
                    }
                    Type::Int | Type::Unit | Type::Bool => {
                        return Err(TypeError::NotCallable(fun_ty))
                    }
                }
            }
            crate::ast::Expr::Literal(lit) => match lit {
                crate::ast::Literal::Int(_) => self.ast.ty(Type::Int),
                crate::ast::Literal::Bool(_) => self.ast.ty(Type::Bool),
            },
            crate::ast::Expr::If(cond, then, else_body) => {
                let cond_ty = self.type_of(cond)?;
                let then_ty = self.type_of(then)?;
                let else_body_ty = else_body.map(|b| self.type_of(b)).transpose()?;

                type_eq(&Type::Bool, cond_ty)?;

                if let Some(else_body_ty) = else_body_ty {
                    type_eq(then_ty, else_body_ty)?;
                    then_ty
                } else {
                    // type_eq(then_ty, &Type::Unit)?;
                    &Type::Unit
                }
            }
            crate::ast::Expr::Ident(name) => self
                .type_of_name(name)?
                .ok_or(TypeError::Unknown(format!("name {name:?}")))?,
            crate::ast::Expr::BinOp(a, op, b) => {
                let a_ty = self.type_of(a)?;
                let b_ty = self.type_of(b)?;

                tracing::debug!("{a_ty:?} , {b_ty:?}");
                type_eq(a_ty, b_ty)?;

                match op {
                    Op::Add | Op::Sub | Op::Mul | Op::Div => {
                        type_eq(&Type::Int, a_ty)?;
                        &Type::Int
                    }
                    Op::Lt | Op::Gt | Op::LtEq | Op::GtEq => {
                        type_eq(&Type::Int, a_ty)?;
                        &Type::Bool
                    }
                    Op::Eq | Op::NotEq => match a_ty {
                        Type::Bool | Type::Int => &Type::Bool,
                        _ => return Err(TypeError::Mismatch(&Type::Int, a_ty)),
                    },
                }
            }
        };

        self.expr_types.insert(ByAddress(expr), ty);

        Ok(ty)
    }
}

fn args_eq<'a>(a: &[TypeRef<'a>], b: &[TypeRef<'a>]) -> Result<(), TypeError<'a>> {
    if a.len() != b.len() {
        return Err(TypeError::ArityMismatch(a.len(), b.len()));
    }

    for (arg_a, arg_b) in a.iter().zip(b.iter()) {
        type_eq(arg_a, arg_b)?;
    }

    Ok(())
}

fn type_eq<'a>(a: TypeRef<'a>, b: TypeRef<'a>) -> Result<(), TypeError<'a>> {
    match (a, b) {
        (Type::Int, Type::Int) | (Type::Unit, Type::Unit) | (Type::Bool, Type::Bool) => Ok(()),
        (
            Type::Fun { args, ret },
            Type::Fun {
                args: args_b,
                ret: ret_b,
            },
        ) => {
            args_eq(args, args_b)?;
            type_eq(ret, ret_b)?;
            Ok(())
        }
        (a, b) => Err(TypeError::Mismatch(a, b)),
    }
}

pub struct TypeChecker<'a> {
    ctx: TypeCtx<'a>,
    errors: Vec<String>,
}

impl<'a> TypeChecker<'a> {
    pub fn check<V: Visit<'a> + 'a>(mut self, node: &'a V) -> Result<TypeCtx<'a>, Vec<String>> {
        node.visit(&mut self);

        let errors = self.errors;
        if errors.is_empty() {
            Ok(self.ctx)
        } else {
            Err(errors)
        }
    }

    pub fn new(ctx: TypeCtx<'a>) -> Self {
        Self {
            ctx,
            errors: Vec::new(),
        }
    }
}

macro_rules! push_err {
    ($s: ident, $e: expr) => {
        if let Err(err) = $e {
            $s.errors.push(err.to_string());
        }
    };
}

impl<'a> Visitor<'a> for TypeChecker<'a> {
    fn visit_stmt(&mut self, stmt: &'a crate::ast::Stmt<'a>) {
        push_err!(self, self.ctx.type_of_stmt(stmt));
        crate::ast::visit::walk_stmt(self, stmt)
    }

    fn visit_expr(&mut self, expr: &'a crate::ast::Expr) {
        push_err!(self, self.ctx.type_of(expr));
        crate::ast::visit::walk_expr(self, expr)
    }

    fn visit_ident(&mut self, ident: &'a Name) {
        push_err!(self, self.ctx.type_of_name(ident));
    }
}

#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum TypeError<'a> {
    #[error("Type mismatch: expected {:?} but found {:?}", .0, .1)]
    Mismatch(TypeRef<'a>, TypeRef<'a>),

    #[error("Resolution failure: {0}")]
    Unresolved(#[from] ResolutionError),

    #[error("Unknown {0}")]
    Unknown(String),

    #[error("The type {0:?} is not callable")]
    NotCallable(TypeRef<'a>),

    #[error("Wrong number of arguments, expected {0} but found {1}")]
    ArityMismatch(usize, usize),
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! test_type {
        (@ast = $ast: ident, @func = $f: ident, @parse = $p: ident, @input = $i: expr, @expect = $t: expr) => {{
            let $ast = $crate::ast::AstCtx::new();
            let res = $crate::parse::parser::$p($i, &$ast).unwrap();
            let resolver = $crate::resolve::Resolver::new(&res);
            let ty_ctx = $crate::typeck::TypeCtx::new(&$ast, &resolver);
            let ty = ty_ctx.$f(&res);
            assert_eq!(ty, $t);
        }};
        (expr($ast: ident) : $input: expr => $t: expr) => {
            test_type!(@ast = $ast, @func = type_of, @parse = expr, @input = $input, @expect = $t)
        };
        (expr : $input: expr => $t: expr) => {
            test_type!(@ast = ast, @func = type_of, @parse = expr, @input = $input, @expect = $t)
        };
        (stmt: $input: expr => $t: expr) => {
            test_type!(@func = type_of_stmt, @parse = stmt, @input = $input, @expect = $t)
        };
    }

    #[test]
    fn type_of_expr() {
        test_type!(expr : "1" => Ok(&Type::Int));
        test_type!(expr : "{
            foo = 1;
            foo
        }" => Ok(&Type::Int));
        test_type!(
            expr(ast) : "fun(a: int, b: int) -> int { a }" => Ok(&Type::Fun {
                args: vec![ast.ty(Type::Int), ast.ty(Type::Int)],
                ret: ast.ty(Type::Int)
            })
        );
    }

    #[test]
    fn call_expr() {
        test_type!(
            expr(ast) : "{
                id = fun(a: int) -> int { a };
                id(1)
            }" => Ok(&Type::Int)
        );
    }

    #[test]
    fn call_expr_bad() {
        test_type!(
            expr(ast) : "{
                id = fun(a: int) -> int { a };
                id(fun(b: int) -> int { b })
            }" => Err(TypeError::Mismatch(ast.ty(Type::Int), ast.ty(Type::Fun { args: vec![ast.ty(Type::Int)], ret: ast.ty(Type::Int) })))
        )
    }

    #[test]
    fn if_expr() {
        test_type!(
            expr(ast) : "if true then 1 else 2" => Ok(&Type::Int)
        );
    }

    #[test]
    fn if_non_bool_cond() {
        test_type!(
            expr(ast) : "if 1 then 1 else 2" => Err(TypeError::Mismatch(ast.ty(Type::Bool), ast.ty(Type::Int)))
        )
    }
}
