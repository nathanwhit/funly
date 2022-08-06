use duplicate::duplicate_item;

use super::{Arg, Call, Expr, Fun, Literal, Name, Program, Stmt, Type};

pub trait Visitor<'ast>: Sized {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        walk_stmt(self, stmt)
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        walk_expr(self, expr)
    }

    fn visit_fun(&mut self, fun: &'ast Fun) {
        walk_fun(self, fun)
    }

    fn visit_call(&mut self, call: &'ast Call) {
        walk_call(self, call)
    }

    fn visit_arg(&mut self, arg: &'ast Arg) {
        walk_arg(self, arg)
    }

    fn visit_ident(&mut self, _ident: &'ast Name) {}

    fn visit_type(&mut self, _ty: &'ast Type) {}

    fn visit_literal(&mut self, _lit: &'ast Literal) {}

    fn visit_program(&mut self, program: &'ast Program) {
        walk_program(self, program)
    }
}

macro_rules! walk_list {
    ($visitor: expr, $method: ident, $list: expr) => {
        for elem in $list {
            $visitor.$method(elem)
        }
    };
}

pub fn walk_stmt<'a, V: Visitor<'a>>(visitor: &mut V, stmt: &'a Stmt) {
    match stmt {
        Stmt::Assign { lhs, rhs } => {
            visitor.visit_expr(lhs);
            visitor.visit_expr(rhs);
        }
        Stmt::Expr(expr) | Stmt::Semi(expr) => visitor.visit_expr(expr),
        Stmt::Bind { name: lhs, rhs } => {
            visitor.visit_ident(lhs);
            visitor.visit_expr(rhs);
        }
    }
}

pub fn walk_expr<'a, V: Visitor<'a>>(visitor: &mut V, expr: &'a Expr) {
    match expr {
        Expr::Block(stmts) => {
            walk_list!(visitor, visit_stmt, stmts);
        }
        Expr::Fun(fun) => visitor.visit_fun(fun),
        Expr::Literal(lit) => visitor.visit_literal(lit),
        Expr::Ident(ident) => visitor.visit_ident(ident),
        Expr::Call(call) => visitor.visit_call(call),
    }
}

pub fn walk_fun<'a, V: Visitor<'a>>(visitor: &mut V, fun: &'a Fun) {
    let Fun { args, ret, body } = fun;
    walk_list!(visitor, visit_arg, args);
    visitor.visit_type(ret);
    visitor.visit_expr(body);
}

pub fn walk_arg<'a, V: Visitor<'a>>(visitor: &mut V, arg: &'a Arg) {
    let Arg { name, ty } = arg;
    visitor.visit_ident(name);
    visitor.visit_type(ty);
}

pub fn walk_call<'a, V: Visitor<'a>>(visitor: &mut V, call: &'a Call) {
    let Call { fun, args } = call;
    visitor.visit_ident(fun);
    walk_list!(visitor, visit_expr, args);
}

pub fn walk_program<'a, V: Visitor<'a>>(visitor: &mut V, program: &'a Program<'a>) {
    walk_list!(visitor, visit_stmt, &program.stmts);
}

pub fn walk_type<'a, V: Visitor<'a>>(_visitor: &mut V, _ty: &'a Type) {}

pub trait Visit<'ast> {
    fn visit<V: Visitor<'ast>>(&'ast self, visitor: &mut V);
}

#[duplicate_item(
        Ty                method         ;
    [   Expr     ]      [ visit_expr ]   ;
    [   Stmt     ]      [ visit_stmt ]   ;
    [   Program  ]      [ visit_program ];
    [   Arg      ]      [ visit_arg  ]   ;
    [   Fun      ]      [ visit_fun  ]   ;

)]
impl<'ast> Visit<'ast> for Ty<'ast> {
    fn visit<V: Visitor<'ast>>(&'ast self, visitor: &mut V) {
        visitor.method(self);
    }
}

impl<'ast> Visit<'ast> for Name {
    fn visit<V: Visitor<'ast>>(&'ast self, visitor: &mut V) {
        visitor.visit_ident(self);
    }
}
