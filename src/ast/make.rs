use super::{AstContext, Expr, ExprId, Ident, Literal, Stmt, StmtId, Type, TypeId};

pub fn lit(val: impl Into<Literal>) -> Literal {
    val.into()
}

pub fn expr(val: impl Into<Expr>) -> Expr {
    val.into()
}

pub fn alloc_expr(ctx: &mut AstContext, val: impl Into<Expr>) -> ExprId {
    ctx.alloc(val.into())
}

pub fn id(ident: &str) -> Ident {
    Ident::new(ident)
}

impl AstContext {
    pub fn expr(&mut self, val: impl Into<Expr>) -> ExprId {
        alloc_expr(self, val)
    }

    pub fn expr_block(&mut self, stmts: impl FnOnce(&mut Self) -> Vec<Stmt>) -> ExprId {
        let stmts = stmts(self).into_iter().map(|s| self.alloc(s)).collect();
        self.alloc(Expr::Block(stmts))
    }

    pub fn ty(&mut self, ty: impl Into<Type>) -> TypeId {
        self.alloc(ty.into())
    }

    pub fn expr_stmt(&mut self, val: impl Into<Expr>) -> StmtId {
        let e = self.expr(val);
        self.alloc(Stmt::Expr(e))
    }

    pub fn semi_stmt(&mut self, val: impl Into<Expr>) -> StmtId {
        let e = self.expr(val);
        self.alloc(Stmt::Semi(e))
    }
}
