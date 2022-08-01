pub mod context;
pub mod make;

use core::fmt;

pub use context::{ArenaAllocated, AstContext, ExprId, StmtId, TypeId};
use derive_more::From;
use duplicate::duplicate_item;

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
impl<'a> PartialEq<E> for DeepEqual<'a, E> {
    fn eq(&self, other: &E) -> bool {
        let DeepEqual(ctx, expr) = self;
        match (expr, other) {
            pat!(Ident, a, b) => a == b,
            pat!(Fun, a, b) => {
                a.args
                    .iter()
                    .zip(b.args.iter())
                    .all(|(a, b)| DeepEqual::<'a, &Arg>(ctx, a) == b)
                    && DeepEqual(ctx, &ctx[a.body]) == &ctx[b.body]
                    && DeepEqual(ctx, &ctx[a.ret]) == &ctx[b.ret]
            }
            pat!(Literal, a, b) => a == b,
            pat!(Block, a, b) => a
                .iter()
                .zip(b.iter())
                .all(|(a, b)| DeepEqual(ctx, &ctx[a]) == &ctx[b]),
            _ => false,
        }
    }
}

#[duplicate_item(
        S;
    [ &Stmt ];
    [  Stmt ];
)]
impl<'a> PartialEq<S> for DeepEqual<'a, S> {
    fn eq(&self, other: &S) -> bool {
        let DeepEqual(ctx, expr) = self;
        match (expr, other) {
            (Stmt::Expr(a), Stmt::Expr(b)) | (Stmt::Semi(a), Stmt::Semi(b)) => {
                DeepEqual(ctx, &ctx[a]) == &ctx[b]
            }
            (
                Stmt::Assign { lhs, rhs },
                Stmt::Assign {
                    lhs: lhs2,
                    rhs: rhs2,
                },
            ) => DeepEqual(ctx, &ctx[lhs]) == &ctx[lhs2] && DeepEqual(ctx, &ctx[rhs]) == &ctx[rhs2],
            _ => false,
        }
    }
}

#[duplicate_item(
        T;
    [ &Type ];
    [  Type ];
)]
impl<'a> PartialEq<T> for DeepEqual<'a, T> {
    fn eq(&self, other: &T) -> bool {
        self.1.eq(other)
    }
}

#[duplicate_item(
        A;
    [ &Arg ];
    [  Arg ];
)]
impl<'a> PartialEq<A> for DeepEqual<'a, A> {
    fn eq(&self, other: &A) -> bool {
        let DeepEqual(ctx, arg) = self;
        arg.name == other.name && DeepEqual(ctx, &ctx[arg.ty]) == &ctx[other.ty]
    }
}

#[duplicate_item(
        F;
    [ &Fun ];
    [  Fun ];
)]
impl<'a> PartialEq<F> for DeepEqual<'a, F> {
    fn eq(&self, other: &F) -> bool {
        let DeepEqual(ctx, fun) = self;
        fun.args
            .iter()
            .zip(other.args.iter())
            .all(|(a, b)| DeepEqual(ctx, a) == b)
            && DeepEqual(ctx, &ctx[fun.ret]) == &ctx[other.ret]
            && DeepEqual(ctx, &ctx[fun.body]) == &ctx[other.body]
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
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("DeepEqual").field(&self.1).finish()
    }
}
