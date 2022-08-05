use super::{
    context::{ExprRef, StmtRef, TypeRef},
    Arg, AstContext, Expr, Literal, Stmt, Type,
};

pub fn lit(val: impl Into<Literal>) -> Literal {
    val.into()
}

pub fn expr<'a>(val: impl Into<Expr<'a>>) -> Expr<'a> {
    val.into()
}

pub fn alloc_expr<'a>(ctx: &'a AstContext<'a>, val: impl Into<Expr<'a>>) -> ExprRef<'a> {
    ctx.alloc(val.into())
}

impl<'a> AstContext<'a> {
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
}

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
