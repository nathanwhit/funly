pub mod context;
pub mod make;
pub mod visit;

use core::fmt;

pub use context::{ArenaAllocated, AstContext, ExprId, StmtId, TypeId};
use derive_more::From;
use duplicate::duplicate_item;

use self::visit::{Visit, Visitor};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Program {
    pub stmts: Vec<StmtId>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, From)]
pub struct Ident(String);

impl Ident {
    pub fn new(s: &str) -> Self {
        Self(s.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Stmt {
    Assign { lhs: ExprId, rhs: ExprId },
    Expr(ExprId),
    Semi(ExprId),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, From)]
pub enum Expr {
    Block(Vec<StmtId>),
    Fun(Fun),
    Literal(Literal),
    Ident(Ident),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, From)]
pub enum Literal {
    Int(i64),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Fun {
    pub args: Vec<Arg>,
    pub ret: TypeId,
    pub body: ExprId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Arg {
    pub name: Ident,
    pub ty: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Int,
}

pub struct DeepEqual<'a, T>(pub &'a AstContext, pub T);
macro_rules! pat {
    ($var: ident, $a: pat, $b: pat) => {
        (Expr::$var($a), Expr::$var($b))
    };
}

#[duplicate_item(
        E;
    [ &Expr ];
    [  Expr ];
)]
impl<'a> PartialEq<DeepEqual<'a, E>> for DeepEqual<'a, E> {
    fn eq(&self, other: &DeepEqual<'a, E>) -> bool {
        let DeepEqual(ctx, expr) = self;
        let DeepEqual(_, other) = other;
        match (expr, other) {
            pat!(Ident, a, b) => a == b,
            pat!(Fun, a, b) => {
                a.args
                    .iter()
                    .zip(b.args.iter())
                    .all(|(a, b)| DeepEqual::<'a, &Arg>(ctx, a) == ctx.deep_eq(b))
                    && DeepEqual(ctx, &ctx[a.body]) == ctx.deep_eq(&ctx[b.body])
                    && DeepEqual(ctx, &ctx[a.ret]) == ctx.deep_eq(&ctx[b.ret])
            }
            pat!(Literal, a, b) => a == b,
            pat!(Block, a, b) => {
                a.iter()
                    .zip(b.iter())
                    .all(|(a, b)| DeepEqual(ctx, &ctx[a]) == ctx.deep_eq(&ctx[b]))
                    && a.len() == b.len()
            }
            _ => false,
        }
    }
}

#[duplicate_item(
        S;
    [ &Stmt ];
    [  Stmt ];
)]
impl<'a> PartialEq<DeepEqual<'a, S>> for DeepEqual<'a, S> {
    fn eq(&self, other: &DeepEqual<'a, S>) -> bool {
        let DeepEqual(ctx, expr) = self;
        let DeepEqual(_, other) = other;
        match (expr, other) {
            (Stmt::Expr(a), Stmt::Expr(b)) | (Stmt::Semi(a), Stmt::Semi(b)) => {
                DeepEqual(ctx, &ctx[a]) == ctx.deep_eq(&ctx[b])
            }
            (
                Stmt::Assign { lhs, rhs },
                Stmt::Assign {
                    lhs: lhs2,
                    rhs: rhs2,
                },
            ) => {
                DeepEqual(ctx, &ctx[lhs]) == ctx.deep_eq(&ctx[lhs2])
                    && DeepEqual(ctx, &ctx[rhs]) == ctx.deep_eq(&ctx[rhs2])
            }
            _ => false,
        }
    }
}

#[duplicate_item(
        T;
    [ &Type ];
    [  Type ];
)]
impl<'a> PartialEq<DeepEqual<'a, T>> for DeepEqual<'a, T> {
    fn eq(&self, other: &DeepEqual<'a, T>) -> bool {
        self.1.eq(&other.1)
    }
}

#[duplicate_item(
        A;
    [ &Arg ];
    [  Arg ];
)]
impl<'a> PartialEq<DeepEqual<'a, A>> for DeepEqual<'a, A> {
    fn eq(&self, other: &DeepEqual<'a, A>) -> bool {
        let DeepEqual(ctx, arg) = self;
        let DeepEqual(_, other) = other;
        arg.name == other.name && DeepEqual(ctx, &ctx[arg.ty]) == ctx.deep_eq(&ctx[other.ty])
    }
}

#[duplicate_item(
        F;
    [ &Fun ];
    [  Fun ];
)]
impl<'a> PartialEq<DeepEqual<'a, F>> for DeepEqual<'a, F> {
    fn eq(&self, other: &DeepEqual<'a, F>) -> bool {
        let DeepEqual(ctx, fun) = self;
        let DeepEqual(_, other) = other;
        fun.args
            .iter()
            .zip(other.args.iter())
            .all(|(a, b)| DeepEqual(ctx, a) == ctx.deep_eq(b))
            && DeepEqual(ctx, &ctx[fun.ret]) == ctx.deep_eq(&ctx[other.ret])
            && DeepEqual(ctx, &ctx[fun.body]) == ctx.deep_eq(&ctx[other.body])
    }
}

#[duplicate_item(
        I;
    [ &Ident ];
    [  Ident ];
)]
impl<'a> PartialEq<I> for DeepEqual<'a, I> {
    fn eq(&self, other: &I) -> bool {
        self.1.eq(other)
    }
}

impl<'a, T> fmt::Debug for DeepEqual<'a, T>
where
    T: fmt::Debug + Clone + Visit,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.1.print(&self.0))
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "")
    }
}

pub struct Printer<'a, T> {
    ctx: &'a AstContext,
    item: T,
}

pub trait Print: Sized {
    fn print<'a>(&self, ctx: &'a AstContext) -> Printer<'a, Self>;
}

impl<T> Print for T
where
    T: Visit + Clone,
{
    fn print<'a>(&self, ctx: &'a AstContext) -> Printer<'a, Self> {
        Printer {
            ctx,
            item: self.clone(),
        }
    }
}

impl<'a, T> fmt::Debug for Printer<'a, T>
where
    T: Visit,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut printer = DebugPrinter { f, stack: vec![] };
        self.item.visit(&mut printer, self.ctx);
        Ok(())
    }
}

struct DisplayPrinter<'p, 'f> {
    f: &'p mut fmt::Formatter<'f>,
}

impl<'p, 'f, 'a> Visitor<'a> for DisplayPrinter<'p, 'f> {
    fn visit_expr(&mut self, ctx: &'a AstContext, expr: &'a Expr) {
        match expr {
            Expr::Block(stmts) => {
                write!(&mut self.f, "{{").unwrap();
                for &stmt in stmts {
                    self.visit_stmt(ctx, &ctx[stmt]);
                }
                write!(&mut self.f, "}}")
            }
            Expr::Fun(fun) => {
                write!(&mut self.f, "Fun {{ args: [").unwrap();
                for arg in &fun.args {
                    self.visit_arg(ctx, arg);
                }
                write!(&mut self.f, "],").unwrap();
                write!(&mut self.f, "ret: ").unwrap();
                self.visit_type(ctx, &ctx[fun.ret]);
                write!(&mut self.f, ", body: ").unwrap();
                self.visit_expr(ctx, &ctx[fun.body]);
                write!(&mut self.f, "}}")
            }
            Expr::Literal(_) => todo!(),
            Expr::Ident(_) => todo!(),
        }
        .unwrap();
    }
}

struct DebugPrinter<'p, 'f> {
    f: &'p mut fmt::Formatter<'f>,
    stack: Vec<String>,
}

impl<'p, 'f, 'a> Visitor<'a> for DebugPrinter<'p, 'f> {
    fn visit_expr(&mut self, ctx: &'a AstContext, expr: &'a Expr) {
        match expr {
            Expr::Ident(id) => write!(&mut self.f, "{id:?}"),
            Expr::Block(stmts) => {
                write!(&mut self.f, "Block([").unwrap();
                let mut debug = self.f.debug_struct("Block");
                for &stmt in stmts {
                    self.visit_stmt(ctx, &ctx[stmt]);
                    write!(&mut self.f, ", ").unwrap();
                }
                write!(&mut self.f, "])")
            }
            Expr::Fun(fun) => {
                write!(&mut self.f, "Fun {{ args: [").unwrap();
                for arg in &fun.args {
                    self.visit_arg(ctx, arg);
                }
                write!(&mut self.f, "],").unwrap();
                write!(&mut self.f, "ret: ").unwrap();
                self.visit_type(ctx, &ctx[fun.ret]);
                write!(&mut self.f, ", body: ").unwrap();
                self.visit_expr(ctx, &ctx[fun.body]);
                write!(&mut self.f, "}}")
            }
            Expr::Literal(literal) => write!(&mut self.f, "{literal:?}"),
        }
        .unwrap();
    }
}
