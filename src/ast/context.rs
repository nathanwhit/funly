use std::ops::Index;

use super::{Expr, Stmt, Type};
use duplicate::duplicate_item;
use id_arena::Id;
use paste::paste;

macro_rules! declare_arena {
    ($t: ident) => {
        paste! {
            pub type [<$t Id>] = ::id_arena::Id<$t>;
            pub type [<$t Arena>] = ::id_arena::Arena<$t>;
            impl ArenaAllocated for $t {
                type Id = [<$t Id>];

                fn alloc(self, context: &mut AstContext) -> Self::Id {
                    context. [<$t:lower s>].alloc(self)
                }

                fn get(id: Self::Id, context: &AstContext) -> Option<&Self> {
                    context. [<$t:lower s>].get(id)
                }
            }
        }
    };
    ($($t: ident),+) => {
        $(
            declare_arena!($t);
        )+
    };
}

pub trait ArenaAllocated {
    type Id;

    fn alloc(self, context: &mut AstContext) -> Self::Id;
    fn get(id: Self::Id, context: &AstContext) -> Option<&Self>;
}

declare_arena!(Stmt, Expr, Type);

pub struct AstContext {
    stmts: StmtArena,
    exprs: ExprArena,
    types: TypeArena,
}

impl AstContext {
    pub fn alloc<T>(&mut self, t: T) -> T::Id
    where
        T: ArenaAllocated,
    {
        t.alloc(self)
    }

    pub fn get<T, I>(&self, id: I) -> Option<&T>
    where
        T: ArenaAllocated<Id = I>,
    {
        <T as ArenaAllocated>::get(id, self)
    }

    pub fn get_ref<T, I>(&self, id: &I) -> Option<&T>
    where
        T: ArenaAllocated<Id = I>,
        I: Copy,
    {
        <T as ArenaAllocated>::get(*id, self)
    }

    pub fn new() -> Self {
        Self {
            stmts: StmtArena::new(),
            exprs: ExprArena::new(),
            types: TypeArena::new(),
        }
    }

    pub(super) fn deep_eq<'a, T>(&'a self, thing: T) -> super::DeepEqual<'a, T> {
        super::DeepEqual(self, thing)
    }
}

#[duplicate_item(
        Idx       method;
    [ &Id<T> ]  [ get_ref ];
    [  Id<T> ]  [ get     ];
)]
impl<'a, T> Index<Idx> for AstContext
where
    T: ArenaAllocated<Id = Id<T>>,
{
    type Output = T;

    fn index(&self, index: Idx) -> &Self::Output {
        self.method(index).unwrap()
    }
}
