pub mod context;
pub mod make;
pub mod visit;

pub use context::{ArenaAllocated, AstContext};
use derive_more::From;

use self::context::{ExprRef, StmtRef, TypeRef};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Program<'a> {
    pub stmts: Vec<StmtRef<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, From)]
pub struct Ident(String);

impl Ident {
    pub fn new(s: &str) -> Self {
        Self(s.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Stmt<'a> {
    Assign { lhs: ExprRef<'a>, rhs: ExprRef<'a> },
    Expr(ExprRef<'a>),
    Semi(ExprRef<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, From)]
pub enum Expr<'a> {
    Block(Vec<StmtRef<'a>>),
    Fun(Fun<'a>),
    Literal(Literal),
    Ident(Ident),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, From)]
pub enum Literal {
    Int(i64),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Fun<'a> {
    pub args: Vec<Arg<'a>>,
    pub ret: TypeRef<'a>,
    pub body: ExprRef<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Arg<'a> {
    pub name: Ident,
    pub ty: TypeRef<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Int,
}
