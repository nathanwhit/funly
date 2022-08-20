use chumsky::prelude::*;

use crate::ast::{Arg, AstCtx, Call, Expr, Fun, Literal, Name, Op, Program, Stmt, Type};

macro_rules! parser {
    ($t: ty) => {
        impl Parser<char, $t, Error = Simple<char>> + Clone
    };
}

macro_rules! parser_todo {
    () => {
        ::chumsky::prelude::just('0').map(|_| todo!())
    };
}

fn _literal<'a>(_ctx: &'a AstCtx) -> parser!(Literal) {
    let int = text::int(10)
        .map(|s: String| Literal::Int(s.parse().unwrap()))
        .padded();
    int
}

fn _expr<'a>(_ctx: &'a AstCtx) -> parser!(Expr<'a>) {
    parser_todo!()
}

peg::parser! {
    pub grammar parser<'a>(ctx: &'a AstCtx<'a>) for str {
        rule number() -> i64
            = n:$(['0'..='9']+) {? n.parse().or(Err("i64"))}

        pub rule literal() -> Literal
            = l:number() { Literal::Int(l) }

        rule _
            = ([' ' | '\t' | '\n'] / ("#" [^'\n']*))*

        pub rule ident() -> Name
            = id:$(['a'..='z' | 'A'..='Z'] ['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { ctx.name(id) }

        pub rule type_() -> Type<'a>
            = "int" { Type::Int }
                / "unit" { Type::Unit }
                / "fun" _ "(" _ args:type_() ** ("," _) ")" _ "->" _ ret:type_()
                    { Type::Fun { args: args.into_iter().map(|t| ctx.alloc(t)).collect(), ret: ctx.alloc(ret) } }

        pub rule arg() -> Arg<'a>
            = name:ident() _ ":" _ ty:type_() { Arg { name, ty: ctx.alloc(ty) } }

        pub rule expr() -> Expr<'a>
            = f:fun() { Expr::Fun(f) }
                / call:call() { Expr::Call(call) }
                / bin_op()
                / lit:literal() { Expr::Literal(lit) }
                / id:ident() { Expr::Ident(id) }
                / "{" _ stmts:stmt()* _ "}" { Expr::Block(stmts.into_iter().map(|s| ctx.alloc(s)).collect()) }
                / "(" _ e:expr() _ ")" { e }

        pub rule bin_op() -> Expr<'a>
            = precedence! {
                a:(@) _ "+" _ b:@ { Expr::BinOp(ctx.expr(a), Op::Add, ctx.expr(b)) }
                a:(@) _ "-" _ b:@ { Expr::BinOp(ctx.expr(a), Op::Sub, ctx.expr(b)) }
                --
                a:(@) _ "*" _ b:@ { Expr::BinOp(ctx.expr(a), Op::Mul, ctx.expr(b)) }
                a:(@) _ "/" _ b:@ { Expr::BinOp(ctx.expr(a), Op::Div, ctx.expr(b)) }
                --
                n:number() { Expr::Literal(Literal::Int(n)) }
                id:ident() { Expr::Ident(id) }
                "(" _ e:bin_op() _ ")" { e }
            }

        pub rule fun() -> Fun<'a>
            = "fun" _ "(" _ args:arg() ** ("," _) ")" _ "->" _ ret:type_() _ body:expr()
        {
            Fun { args, ret: ctx.alloc(ret), body: ctx.alloc(body) }
        }

        pub rule standalone_fun() -> Fun<'a>
            = f:fun() _ { f }

        pub rule call() -> Call<'a>
            = fun:ident() _ "(" _ args:expr() ** ("," _) _ ")"
        {
            Call {
                fun,
                args: args.into_iter().map(|e| ctx.alloc(e)).collect()
            }
        }

        pub rule stmt() -> Stmt<'a>
            = _ lhs:expr() _ ":" _ "=" _ rhs:expr() _ ";" { Stmt::Assign { lhs: ctx.alloc(lhs), rhs: ctx.alloc(rhs) } }
                / name:ident() _ "=" _ rhs:expr() _ ";" { Stmt::Bind { name, rhs: ctx.alloc(rhs) } }
                / _ e:expr() _ ";" _ { Stmt::Semi(ctx.alloc(e)) }
                / _ e:expr() _ { Stmt::Expr(ctx.alloc(e)) }

        pub rule program() -> Program<'a>
            = stmts:stmt() ** _ _ { Program { stmts: stmts.into_iter().map(|s| ctx.alloc(s)).collect() } }
    }
}

#[cfg(test)]
mod test {
    use crate::ast::make::{self, exprs};

    use super::*;
    use pretty_assertions::assert_eq;
    use Type::Int;

    macro_rules! parse_test {
        ($name: ident : $parser: ident | $input: expr => $output: expr) => {
            #[test]
            fn $name() {
                let mut ctx = $crate::ast::AstCtx::new();
                ::pretty_assertions::assert_eq!(
                    $parser(&mut ctx).parse($input).map_err(|_| ()),
                    $output
                );
            }
        };
        ($($name: ident : $parser: ident | $input: expr => $output: expr),+ $(,)?) => {
            $(
                parse_test!(
                    $name : $parser | $input => $output
                );
            )+
        };
    }

    // Basically the format is
    // <test name> : <parser func> (<ctx ident if needed>) | <input> => <output>,
    macro_rules! peg_parse_test_ {
        ($name: ident : $parser: ident ($ctx: ident) | $input: expr => $output: expr) => {
            paste::paste! {
                #[test]
                #[allow(non_snake_case)]
                fn [<peg_ $name>]() {
                    let $ctx = $crate::ast::AstCtx::new();
                    let actual = parser::$parser($input, &$ctx).map_err(|e| eprintln!("{e}"));
                    let expected: Result<_, ()> = $output;
                    match (actual, expected) {
                        (Ok(actual), Ok(expected)) => ::pretty_assertions::assert_eq!(
                            actual,
                            expected,
                        ),
                        (Err(_), Err(_)) => {},
                        (a, e) => panic!("expected {e:#?}, found {a:#?}")
                    }

                }
            }
        };
        ($name: ident : $parser: ident ($ctx: ident) | $input: expr => $output: expr) => {
            paste::paste! {
                #[test]
                fn [<peg_ $name>]() {
                    let mut $ctx = $crate::ast::AstContext::new();
                    let actual = parser::$parser($input, &$ctx).map_err(|_| ());
                    let expected: Result<_, ()> = $output;
                    match (actual, expected) {
                        (Ok(actual), Ok(expected)) => ::pretty_assertions::assert_eq!(
                            actual,
                            expected,
                        ),
                        (Err(_), Err(_)) => {},
                        (a, e) => panic!("expected {a:?}, found {e:?}")
                    }

                }
            }
        };
        ($name: ident : $parser: ident | $input: expr =>  $output: expr) => {
            paste::paste! {
                #[test]
                fn [<peg_ $name>]() {
                    let ctx = $crate::ast::AstCtx::new();
                    ::pretty_assertions::assert_eq!(parser::$parser($input, &ctx), $output);
                }
            }
        };
        ($($name: ident : $parser: ident $( ($ctx: ident) )? | $input: expr => $output: expr),+ $(,)?) => {
            $(
                peg_parse_test!($name : $parser $(($ctx))? | $input => $output);
            )+
        };
    }

    macro_rules! peg_parse_test {
        ($parser: ident ($ctx: ident) $(-)+ $($name: ident : $input: expr => $output: expr),+ $(,)?) => {
            $(
                paste::paste! {
                    peg_parse_test_!([<$parser _ $name>] : $parser ($ctx) | $input => $output);
                }
            )+
        };
        ($parser: ident $(-)+ $($name: ident : $input: expr => $output: expr),+ $(,)?) => {
            $(
                paste::paste! {
                    peg_parse_test_!([<$parser _ $name>] : $parser | $input => $output);
                }
            )+
        };
        ($($parser: ident $( ($ctx: ident) )? $(-)+ $($name: ident : $input: expr => $output: expr),+ $(,)?);+) => {
            $(
                peg_parse_test!($parser $( ($ctx) )? ---- $($name : $input => $output),+);
            )+
        };
    }

    #[test]
    fn peg_literal() {
        let mut ctx = AstCtx::new();
        assert_eq!(parser::literal("50", &mut ctx).unwrap(), Literal::Int(50));
    }

    #[test]
    fn peg_fun() {
        let ctx = AstCtx::new();
        let result = parser::fun("fun() -> int { 1 }", &ctx).unwrap();
        let expected = Fun {
            args: vec![],
            ret: ctx.alloc(Type::Int),
            body: {
                let inner = Expr::Block(vec![{
                    let e = ctx.alloc(Expr::Literal(Literal::Int(1)));
                    ctx.alloc(Stmt::Expr(e))
                }]);
                ctx.alloc(inner)
            },
        };
        assert_eq!(result, expected);
    }

    peg_parse_test! {
        literal
        --------
        int : "50" => Ok(make::lit(50));

        fun(ctx)
        --------
        works : "fun() -> int { 1 }" => Ok(
            Fun {
                args: vec![],
                ret: ctx.ty(Type::Int),
                body: ctx.expr_block(
                    vec![
                        Stmt::Expr(ctx.expr(make::lit(1)))
                    ]
                )
            }
        ),
        with_args : "fun(a: int, b: int) -> int { a }" => Ok(
            Fun {
                args: vec![ctx.arg("a", Int), ctx.arg("b", Int)],
                ret: ctx.ty(Type::Int),
                body: ctx.expr_block(
                    vec![
                        Stmt::Expr(ctx.expr(ctx.name("a")))
                    ]
                )
            }
        );

        call(ctx)
        ---------
        no_args     : "foo()" => Ok(Call {
            fun: ctx.name("foo"),
            args: vec![]
        }),
        with_args   : "foo(bar, 1, 2)" => Ok(Call {
            fun: ctx.name("foo"),
            args: exprs!(ctx, [ctx.name("bar"), make::lit(1), make::lit(2)])
        });

        stmt(ctx)
        ---------
        semi   : "1;"           => Ok(Stmt::Semi(ctx.expr(make::lit(1)))),
        expr   : "1"            => Ok(Stmt::Expr(ctx.expr(make::lit(1)))),
        assign : "foo := 1;"    => Ok(Stmt::Assign { lhs: ctx.expr(ctx.name("foo")), rhs: ctx.expr(make::lit(1)) }),
        bind   : "foo = 1;"     => Ok(Stmt::Bind { name: ctx.name("foo"), rhs: ctx.expr(make::lit(1)) }),
        block  : "{ 1; 2; 3; 4; 5};" => Ok(Stmt::Semi(ctx.expr_block( {
            vec![
                Stmt::Semi(ctx.expr(make::lit(1))),
                Stmt::Semi(ctx.expr(make::lit(2))),
                Stmt::Semi(ctx.expr(make::lit(3))),
                Stmt::Semi(ctx.expr(make::lit(4))),
                Stmt::Expr(ctx.expr(make::lit(5))),
            ]
        })));

        type_(ctx)
        ----------
        int     : "int" => Ok(Type::Int),
        fun     : "fun (int, int) -> int" => Ok(Type::Fun { args: vec![ctx.ty(Int), ctx.ty(Int)], ret: ctx.ty(Int)}),
        fun_fun : "fun (fun (int) -> int, int) -> fun (int) -> int" => Ok(Type::Fun {
            args: vec![
                ctx.ty(Type::Fun {
                    args: vec![ctx.ty(Int)],
                    ret: ctx.ty(Int),
                }),
                ctx.ty(Int),
            ],
            ret: ctx.ty(Type::Fun {
                args: vec![ctx.ty(Int)],
                ret: ctx.ty(Int),
            }),
        });

        expr(ctx)
        ---------
        block_with_stuff : "{
            id = fun(a: int) -> int { a };
            id ( fun(b: int) -> int { b } )
        }" => Ok(Expr::Block(vec![ctx.bind(ctx.name("id"), ctx.expr(Fun {
            args: vec![ctx.arg("a", Type::Int)],
            ret: ctx.ty(Type::Int),
            body: ctx.expr(vec![ctx.expr_stmt(ctx.name("a"))])
        })), ctx.expr_stmt(Call { fun: ctx.name("id"), args: vec![ctx.expr(Fun {
            args: vec![ctx.arg("b", Type::Int)],
            ret: ctx.ty(Type::Int),
            body: ctx.expr(vec![ctx.expr_stmt(ctx.name("b"))])
        })]})]));

        bin_op(ctx)
        -----------
        add : "1 + 2" => Ok(make::bin_op(&ctx, 1, Op::Add, 2)),
        add_assoc : "1 + 2 + 3" => Ok(make::bin_op(&ctx, ctx.expr(make::bin_op(&ctx, 1, Op::Add, 2)), Op::Add, 3)),
        add_mul_prec : "1 + 2 * 3" => Ok(make::bin_op(&ctx, ctx.expr(1), Op::Add, ctx.expr(make::bin_op(&ctx, 2, Op::Mul, 3))))
    }

    parse_test! {
        literal_works : _literal | "50" => Ok(Literal::Int(50)),
        literal_bad   : _literal | "j"  => Err(()),
    }
}
