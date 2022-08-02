use chumsky::prelude::*;

use crate::ast::{Arg, AstContext, Expr, Fun, Ident, Literal, Program, Stmt, Type};

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

fn _literal(_ctx: &mut AstContext) -> parser!(Literal) {
    let int = text::int(10)
        .map(|s: String| Literal::Int(s.parse().unwrap()))
        .padded();
    int
}

fn _expr(_ctx: &mut AstContext) -> parser!(Expr) {
    parser_todo!()
}

peg::parser! {
    pub grammar parser(ctx: &mut AstContext) for str {
        rule number() -> i64
            = n:$(['0'..='9']+) {? n.parse().or(Err("i64"))}

        pub rule literal() -> Literal
            = l:number() { Literal::Int(l) }

        rule _
            = [' ' | '\t' | '\n']*

        pub rule ident() -> Ident
            = id:$(['a'..='z' | 'A'..='Z'] ['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { Ident::new(id) }

        pub rule type_() -> Type
            = "int" { Type::Int }

        pub rule arg() -> Arg
            = name:ident() _ ":" _ ty:type_() { Arg { name, ty: ctx.alloc(ty) } }

        pub rule expr() -> Expr
            = id:ident() { Expr::Ident(id) }
                / lit:literal() { Expr::Literal(lit) }
                / f:fun() { Expr::Fun(f) }
                / "{" _ stmts:stmt()* _ "}" { Expr::Block(stmts.into_iter().map(|s| ctx.alloc(s)).collect()) }

        pub rule fun() -> Fun
            = "fun" _ "(" _ args:arg() ** "," _ ")" _ "->" _ ret:type_() _ body:expr()
        {
            Fun { args, ret: ctx.alloc(ret), body: ctx.alloc(body) }
        }

        pub rule stmt() -> Stmt
            = _ lhs:expr() _ "=" _ rhs:expr() _ ";" { Stmt::Assign { lhs: ctx.alloc(lhs), rhs: ctx.alloc(rhs) } }
                / _ e:expr() _ ";" _ { Stmt::Semi(ctx.alloc(e)) }
                / _ e:expr() _ { Stmt::Expr(ctx.alloc(e)) }

        pub rule program() -> Program
            = stmts:stmt() ** _ { Program { stmts: stmts.into_iter().map(|s| ctx.alloc(s)).collect() } }
    }
}

#[cfg(test)]
mod test {
    use crate::ast::{make, DeepEqual};

    use super::*;
    use pretty_assertions::assert_eq;

    macro_rules! parse_test {
        ($name: ident : $parser: ident | $input: expr => $output: expr) => {
            #[test]
            fn $name() {
                let mut ctx = $crate::ast::AstContext::new();
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
                fn [<peg_ $name>]() {
                    let mut $ctx = $crate::ast::AstContext::new();
                    let actual = parser::$parser($input, &mut $ctx).map_err(|_| ());
                    let expected: Result<_, ()> = $output;
                    match (actual, expected) {
                        (Ok(actual), Ok(expected)) => ::pretty_assertions::assert_eq!(
                            DeepEqual(&$ctx, actual),
                            DeepEqual(&$ctx, expected),
                        ),
                        (Err(_), Err(_)) => {},
                        (a, e) => panic!("expected {a:?}, found {e:?}")
                    }

                }
            }
        };
        ($name: ident : $parser: ident ($ctx: ident) | $input: expr => $output: expr) => {
            paste::paste! {
                #[test]
                fn [<peg_ $name>]() {
                    let mut $ctx = $crate::ast::AstContext::new();
                    let actual = parser::$parser($input, &mut $ctx).map_err(|_| ());
                    let expected: Result<_, ()> = $output;
                    match (actual, expected) {
                        (Ok(actual), Ok(expected)) => ::pretty_assertions::assert_eq!(
                            DeepEqual(&$ctx, actual),
                            DeepEqual(&$ctx, expected),
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
                    let mut ctx = $crate::ast::AstContext::new();
                    ::pretty_assertions::assert_eq!(parser::$parser($input, &mut ctx), $output);
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
        let mut ctx = AstContext::new();
        assert_eq!(parser::literal("50", &mut ctx).unwrap(), Literal::Int(50));
    }

    #[test]
    fn peg_fun() {
        let mut ctx = AstContext::new();
        let result = parser::fun("fun() -> int { 1 }", &mut ctx).unwrap();
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
        assert_eq!(DeepEqual(&ctx, result), DeepEqual(&ctx, expected));
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
                body: ctx.expr_block(|ctx|
                    vec![
                        Stmt::Expr(ctx.expr(make::lit(1)))
                    ]
                )
            }
        ),
        with_args : "fun(a: int) -> int { a }" => Ok(
            Fun {
                args: vec![Arg { name: make::id("a"), ty: ctx.ty(Type::Int) }],
                ret: ctx.ty(Type::Int),
                body: ctx.expr_block(|ctx|
                    vec![
                        Stmt::Expr(ctx.expr(make::id("a")))
                    ]
                )
            }
        );

        stmt(ctx)
        ---------
        semi   : "1;"       => Ok(Stmt::Semi(ctx.expr(make::lit(1)))),
        expr   : "1"        => Ok(Stmt::Expr(ctx.expr(make::lit(1)))),
        assign : "foo = 1;"  => Ok(Stmt::Assign { lhs: ctx.expr(make::id("foo")), rhs: ctx.expr(make::lit(1)) }),

        ambig  : "{ 1; 2; 3; 4; };" => Ok(Stmt::Semi(ctx.expr_block(|ctx| {
            vec![
                Stmt::Semi(ctx.expr(make::lit(1))),
                Stmt::Semi(ctx.expr(make::lit(2)))
            ]
        }))),
    }

    parse_test! {
        literal_works : _literal | "50" => Ok(Literal::Int(50)),
        literal_bad   : _literal | "j"  => Err(()),

    }
}
