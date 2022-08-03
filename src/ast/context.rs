use std::sync::atomic::AtomicU64;

use super::{Expr, Name, Stmt, Type};

use paste::paste;

macro_rules! declare_arena {
    ($t: ident <$a: lifetime>) => {
        paste! {
            pub type [<$t Arena>]<$a> = ::typed_arena::Arena<$t<$a>>;
            pub type [<$t Ref>]<$a> = &$a $t<$a>;
            impl<'a> ArenaAllocated<'a> for $t<'a> {
                fn alloc(self, context: &'a AstContext<'a>) -> &'a $t<'a> where Self: 'a {
                    context. [<$t:lower s>].alloc(self)
                }
            }
        }
    };
    ($t: ident) => {
        paste! {
            pub type [<$t Arena>] = ::typed_arena::Arena<$t>;
            pub type [<$t Ref>]<'a> = &'a $t;
            impl<'a> ArenaAllocated<'a> for $t {
                fn alloc(self, context: &'a AstContext) -> &'a $t where Self: 'a {
                    context. [<$t:lower s>].alloc(self)
                }
            }
        }
    };
    ($($t: ident $(< $a: lifetime >)?),+) => {
        $(
            declare_arena!($t $(<$a>)?);
        )+
    };
}

pub trait ArenaAllocated<'a> {
    fn alloc(self, context: &'a AstContext<'a>) -> &'a Self
    where
        Self: 'a;
}

declare_arena!(Stmt<'a>, Expr<'a>, Type);

pub struct AstContext<'a> {
    stmts: StmtArena<'a>,
    exprs: ExprArena<'a>,
    types: TypeArena,
    name_idx: AtomicU64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct NameId(u64);

impl<'a> AstContext<'a> {
    pub fn alloc<T>(&'a self, t: T) -> &'a T
    where
        T: ArenaAllocated<'a>,
    {
        &*t.alloc(self)
    }

    pub fn name(&self, s: &str) -> Name {
        Name::new(
            s,
            NameId(
                self.name_idx
                    .fetch_add(1, std::sync::atomic::Ordering::SeqCst),
            ),
        )
    }

    pub fn new() -> Self {
        Self {
            stmts: StmtArena::new(),
            exprs: ExprArena::new(),
            types: TypeArena::new(),
            name_idx: AtomicU64::new(0),
        }
    }
}
