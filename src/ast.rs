pub mod context;
pub mod make;
pub mod visit;

pub use context::{ArenaAllocated, AstCtx};
use derive_more::From;

use self::context::{ExprRef, NameId, StmtRef, TypeRef};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Program<'a> {
    pub stmts: Vec<StmtRef<'a>>,
}

#[derive(Debug, Clone, Eq, PartialOrd, Ord)]
pub struct Name {
    text: Ident,
    id: NameId,
}

impl Name {
    pub fn ident(&self) -> &Ident {
        &self.text
    }

    pub fn id(&self) -> NameId {
        self.id
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
pub struct Ident(String);

impl PartialEq for Name {
    fn eq(&self, other: &Self) -> bool {
        self.text == other.text
    }
}

impl Name {
    pub fn new(s: &str, id: NameId) -> Self {
        Self {
            text: Ident(s.into()),
            id,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Stmt<'a> {
    Bind { name: Name, rhs: ExprRef<'a> },
    Assign { lhs: ExprRef<'a>, rhs: ExprRef<'a> },
    Expr(ExprRef<'a>),
    Semi(ExprRef<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, From)]
pub enum Expr<'a> {
    Block(Vec<StmtRef<'a>>),
    Fun(Fun<'a>),
    Call(Call<'a>),
    Literal(Literal),
    Ident(Name),
    BinOp(ExprRef<'a>, Op, ExprRef<'a>),
    If(ExprRef<'a>, ExprRef<'a>, Option<ExprRef<'a>>),
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Call<'a> {
    pub fun: Name,
    pub args: Vec<ExprRef<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, From)]
pub enum Literal {
    Int(i64),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Fun<'a> {
    pub args: Vec<Arg<'a>>,
    pub ret: TypeRef<'a>,
    pub body: ExprRef<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Arg<'a> {
    pub name: Name,
    pub ty: TypeRef<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type<'a> {
    Int,
    Bool,
    Fun {
        args: Vec<TypeRef<'a>>,
        ret: TypeRef<'a>,
    },
    Unit,
}

#[cfg(test)]
mod test_impls {
    use super::{Expr, Literal};

    impl<'a> From<i64> for Expr<'a> {
        fn from(val: i64) -> Self {
            Self::Literal(Literal::Int(val))
        }
    }
}
