use by_address::ByAddress;
use dashmap::DashMap;
use thiserror::Error;

use crate::{
    ast::{
        context::{ExprRef, StmtRef, TypeRef},
        AstCtx, Name, Type,
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

    pub fn type_of_name(&self, name: &Name) -> Result<Option<TypeRef<'a>>, TypeError> {
        let binding = self.resolver.resolve(name)?;

        Ok(self.type_of_binding(binding))
    }

    pub fn type_of_binding(&self, binding: Binding) -> Option<TypeRef<'a>> {
        self.binding_types.get(&binding).map(|e| *e.value())
    }

    pub fn type_of_stmt(&self, stmt: StmtRef<'a>) -> Result<TypeRef<'a>, TypeError> {
        if let Some(ty) = self.stmt_types.get(&ByAddress(stmt)) {
            return Ok(ty.value());
        }

        let ty = match stmt {
            crate::ast::Stmt::Assign { lhs, rhs } => {
                let _ = self.type_of(lhs)?;
                let _ = self.type_of(rhs)?;
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

    pub fn type_of(&self, expr: ExprRef<'a>) -> Result<TypeRef<'a>, TypeError> {
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
            crate::ast::Expr::Fun(fun) => self.ast.ty(Type::Fun {
                args: fun.args.iter().map(|a| a.ty).collect(),
                ret: fun.ret,
            }),
            crate::ast::Expr::Call(call) => {
                let fun_ty = self.type_of_name(&call.fun)?.ok_or(TypeError::Unknown)?;

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
                    Type::Int | Type::Unit => return Err(TypeError::NotCallable(fun_ty)),
                }
            }
            crate::ast::Expr::Literal(lit) => match lit {
                crate::ast::Literal::Int(_) => self.ast.ty(Type::Int),
            },
            crate::ast::Expr::Ident(name) => self.type_of_name(name)?.ok_or(TypeError::Unknown)?,
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
        (Type::Int, Type::Int) | (Type::Unit, Type::Unit) => Ok(()),
        (Type::Int, Type::Fun { .. })
        | (Type::Int, Type::Unit)
        | (Type::Fun { .. }, Type::Int)
        | (Type::Fun { .. }, Type::Unit)
        | (Type::Unit, Type::Int)
        | (Type::Unit, Type::Fun { .. }) => Err(TypeError::Mismatch(a, b)),
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
    }
}

#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum TypeError<'a> {
    #[error("Type mismatch")]
    Mismatch(TypeRef<'a>, TypeRef<'a>),

    #[error("Resolution failure: {0}")]
    Unresolved(#[from] ResolutionError),

    #[error("Unknown")]
    Unknown,

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
}
