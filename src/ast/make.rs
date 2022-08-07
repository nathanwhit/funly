use super::{
    context::{ExprRef, StmtRef, TypeRef},
    Arg, AstCtx, Expr, Literal, Name, Op, Stmt, Type,
};

pub fn lit(val: impl Into<Literal>) -> Literal {
    val.into()
}

pub fn expr<'a>(val: impl Into<Expr<'a>>) -> Expr<'a> {
    val.into()
}

pub fn alloc_expr<'a>(ctx: &'a AstCtx<'a>, val: impl Into<Expr<'a>>) -> ExprRef<'a> {
    ctx.alloc(val.into())
}

pub fn bin_op<'a>(
    ctx: &'a AstCtx<'a>,
    a: impl Into<Expr<'a>>,
    op: Op,
    b: impl Into<Expr<'a>>,
) -> Expr<'a> {
    Expr::BinOp(ctx.expr(a), op, ctx.expr(b))
}

impl<'a> AstCtx<'a> {
    pub fn expr(&'a self, val: impl Into<Expr<'a>>) -> ExprRef<'a> {
        alloc_expr(self, val)
    }

    pub fn expr_block(&'a self, stmts: impl IntoIterator<Item = Stmt<'a>>) -> ExprRef<'a> {
        let stmts = stmts.into_iter().map(|s| self.alloc(s)).collect();
        self.alloc(Expr::Block(stmts))
    }

    pub fn ty(&'a self, ty: impl Into<Type<'a>>) -> TypeRef<'a> {
        self.alloc(ty.into())
    }

    pub fn arg(&'a self, name: impl AsRef<str>, ty: impl Into<Type<'a>>) -> Arg {
        Arg {
            name: self.name(name.as_ref()),
            ty: self.ty(ty),
        }
    }

    pub fn expr_stmt(&'a self, val: impl Into<Expr<'a>>) -> StmtRef<'a> {
        let e = self.expr(val);
        self.alloc(Stmt::Expr(e))
    }

    pub fn semi_stmt(&'a self, val: impl Into<Expr<'a>>) -> StmtRef<'a> {
        let e = self.expr(val);
        self.alloc(Stmt::Semi(e))
    }

    pub fn assign(&'a self, lhs: impl Into<Expr<'a>>, rhs: impl Into<Expr<'a>>) -> StmtRef<'a> {
        self.alloc(Stmt::Assign {
            lhs: self.expr(lhs),
            rhs: self.expr(rhs),
        })
    }

    pub fn bind(&'a self, name: Name, rhs: impl Into<Expr<'a>>) -> StmtRef<'a> {
        self.alloc(Stmt::Bind {
            name,
            rhs: self.alloc(rhs.into()),
        })
    }
}

#[allow(unused_macros)]
macro_rules! exprs {
    ($ctx: ident, [$($e: expr),*]) => {
        vec![
            $(
                $ctx.expr($e)
            ),*
        ]
    };
}

#[allow(unused_imports)]
pub(crate) use exprs;

#[cfg(test)]
#[duplicate::duplicate_item(
    Ty      Ref;
    [Expr]  [ExprRef];
    [Stmt]  [StmtRef];
)]
impl<'a> From<Ref<'a>> for Ty<'a> {
    fn from(value: Ref<'a>) -> Self {
        value.clone()
    }
}
