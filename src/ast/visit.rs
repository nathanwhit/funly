use duplicate::duplicate_item;

use super::{Arg, AstContext, Expr, Fun, Ident, Literal, Program, Stmt, Type};

pub trait Visitor<'ast>: Sized {
    fn visit_stmt(&mut self, ctx: &'ast AstContext, stmt: &'ast Stmt) {
        walk_stmt(self, ctx, stmt)
    }

    fn visit_expr(&mut self, ctx: &'ast AstContext, expr: &'ast Expr) {
        walk_expr(self, ctx, expr)
    }

    fn visit_fun(&mut self, ctx: &'ast AstContext, fun: &'ast Fun) {
        walk_fun(self, ctx, fun)
    }

    fn visit_arg(&mut self, ctx: &'ast AstContext, arg: &'ast Arg) {
        walk_arg(self, ctx, arg)
    }

    fn visit_ident(&mut self, _ctx: &'ast AstContext, _ident: &'ast Ident) {}

    fn visit_type(&mut self, _ctx: &'ast AstContext, _ty: &'ast Type) {}

    fn visit_literal(&mut self, _ctx: &'ast AstContext, _lit: &'ast Literal) {}

    fn visit_program(&mut self, ctx: &'ast AstContext, program: &'ast Program) {
        walk_program(self, ctx, program)
    }
}

macro_rules! walk_list {
    ($visitor: expr, $ctx: ident, $method: ident, index $list: expr) => {
        for &elem in $list {
            $visitor.$method($ctx, &$ctx[elem])
        }
    };
    ($visitor: expr, $ctx: ident, $method: ident, $list: expr) => {
        for elem in $list {
            $visitor.$method($ctx, elem)
        }
    };
}

pub fn walk_stmt<'a, V: Visitor<'a>>(visitor: &mut V, ctx: &'a AstContext, stmt: &'a Stmt) {
    match stmt {
        Stmt::Assign { lhs, rhs } => {
            visitor.visit_expr(ctx, &ctx[lhs]);
            visitor.visit_expr(ctx, &ctx[rhs]);
        }
        Stmt::Expr(expr) | Stmt::Semi(expr) => visitor.visit_expr(ctx, &ctx[expr]),
    }
}

pub fn walk_expr<'a, V: Visitor<'a>>(visitor: &mut V, ctx: &'a AstContext, expr: &'a Expr) {
    match expr {
        Expr::Block(stmts) => {
            walk_list!(visitor, ctx, visit_stmt, index stmts);
        }
        Expr::Fun(fun) => visitor.visit_fun(ctx, fun),
        Expr::Literal(lit) => visitor.visit_literal(ctx, lit),
        Expr::Ident(ident) => visitor.visit_ident(ctx, ident),
    }
}

pub fn walk_fun<'a, V: Visitor<'a>>(visitor: &mut V, ctx: &'a AstContext, fun: &'a Fun) {
    let Fun { args, ret, body } = fun;
    walk_list!(visitor, ctx, visit_arg, args);
    visitor.visit_type(ctx, &ctx[ret]);
    visitor.visit_expr(ctx, &ctx[body]);
}

pub fn walk_arg<'a, V: Visitor<'a>>(visitor: &mut V, ctx: &'a AstContext, arg: &'a Arg) {
    let Arg { name, ty } = arg;
    visitor.visit_ident(ctx, name);
    visitor.visit_type(ctx, &ctx[ty]);
}

pub fn walk_program<'a, V: Visitor<'a>>(
    visitor: &mut V,
    ctx: &'a AstContext,
    program: &'a Program,
) {
    walk_list!(visitor, ctx, visit_stmt, index & program.stmts);
}

pub trait Visit {
    fn visit<'ast, V: Visitor<'ast>>(&'ast self, visitor: &'ast mut V, ctx: &'ast AstContext);
}

#[duplicate_item(
        Ty                method         ;
    [   Expr     ]      [ visit_expr ]   ;
    [   Stmt     ]      [ visit_stmt ]   ;
    [   Type     ]      [ visit_type ]   ;
    [   Program  ]      [ visit_program ];
    [   Arg      ]      [ visit_arg  ]   ;
    [   Ident    ]      [ visit_ident]   ;
    [   Literal  ]      [ visit_literal ];
    [   Fun      ]      [ visit_fun  ]   ;

)]
impl Visit for Ty {
    fn visit<'ast, V: Visitor<'ast>>(&'ast self, visitor: &mut V, ctx: &'ast AstContext) {
        visitor.method(ctx, self);
    }
}
