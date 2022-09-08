use std::sync::Once;

use indoc::indoc;

use super::*;

fn init_logging() {
    use tracing_subscriber::layer::SubscriberExt;
    static INIT: Once = Once::new();
    INIT.call_once(|| {
        tracing::subscriber::set_global_default(
            tracing_subscriber::registry()
                .with(tracing_error::ErrorLayer::default())
                .with(tracing_subscriber::EnvFilter::from_default_env())
                .with(tracing_tree::HierarchicalLayer::default().with_indent_lines(true)),
        )
        .unwrap();
    });
}

fn compile_fun<I, O>(src: &str) -> fn(I) -> O {
    init_logging();
    let fun = compile_standalone_fun(src).unwrap();
    unsafe { mem::transmute(fun) }
}

#[allow(unused_macros)]
macro_rules! compile_test {
    ({$($t: tt)*} as $typ: ty, $inp: expr => $exp: expr) => {
        let fun: $typ = compile_fun(stringify!($($t)*));
        assert_eq!(fun($inp), $exp);
    };
    ({$($t: tt)*} as $typ: ty) => {
        {
            let fun: $typ = compile_fun(stringify!($($t)*));
            fun
        }
    };
}

#[test]
fn id_works() {
    let id: fn(i64) -> i64 = compile_fun(indoc! {"
        fun(a: int) -> int { a }
    "});

    assert_eq!(id(1), 1);
}

#[test]
fn math_works() {
    let add: fn((i64, i64)) -> i64 = compile_fun(indoc! {"
        fun(a: int, b: int) -> int { a + b }
    "});

    assert_eq!(add((3, 2)), 5);

    let sub: fn((i64, i64)) -> i64 = compile_fun(indoc! {"
        fun(a: int, b: int) -> int { a - b }
    "});

    assert_eq!(sub((3, 2)), 1);

    let mul: fn((i64, i64)) -> i64 = compile_fun(indoc! {"
        fun(a: int, b: int) -> int { a * b }
    "});

    assert_eq!(mul((3, 2)), 6);

    let div: fn((i64, i64)) -> i64 = compile_fun(indoc! {"
        fun(a: int, b: int) -> int { a / b }
    "});

    assert_eq!(div((3, 2)), 1);
}

#[test]
fn if_else() {
    let booly: fn(bool) -> i64 = compile_fun(indoc! {"
        fun(do_thing: bool) -> int {
            if do_thing then 1 else 2
        }
    "});

    assert_eq!(booly(true), 1);
    assert_eq!(booly(false), 2);
}

macro_rules! cmp {
    (int $toks: tt) => {{
        let fun: fn((i64, i64)) -> bool = compile_fun(concat!(
            "fun(a: int, b: int) -> bool { a ",
            stringify!($toks),
            " b }"
        ));

        fun
    }};
    (bool $toks: tt) => {{
        let fun: fn((bool, bool)) -> bool = compile_fun(concat!(
            "fun(a: bool, b: bool) -> bool { a ",
            stringify!($toks),
            " b }"
        ));

        fun
    }};
}

#[test]
fn compare() {
    let eq = cmp!(int ==);

    assert!(eq((1, 1)));
    assert!(!eq((1, 2)));

    let eq_bool = cmp!(bool ==);

    assert!(eq_bool((true, true)));
    assert!(eq_bool((false, false)));
    assert!(!eq_bool((true, false)));
    assert!(!eq_bool((false, true)));

    let neq = cmp!(int !=);

    assert!(neq((1, 2)));
    assert!(!neq((1, 1)));

    let neq_bool = cmp!(bool !=);

    assert!(neq_bool((true, false)));
    assert!(!neq_bool((true, true)));

    let gt = cmp!(int >);

    assert!(gt((2, 1)));
    assert!(!gt((1, 2)));

    let lt = cmp!(int <);

    assert!(lt((1, 2)));
    assert!(!lt((2, 1)));
    assert!(!lt((2, 2)));

    let gte = cmp!(int >=);

    assert!(gte((2, 1)));
    assert!(gte((2, 2)));
    assert!(!gte((1, 2)));

    let lte = cmp!(int <=);

    assert!(lte((1, 2)));
    assert!(lte((2, 2)));
    assert!(!lte((2, 1)));
}

#[test]
fn fib() {
    let fib: fn(i64) -> i64 = compile_fun(indoc! {"
        fun(n: int) -> int {
            fib = fun(a: int) -> int {
                if a == 0 then 0 else {
                    if a <= 2 then 1 else {
                        fib(a-1) + fib(a-2)
                    }
                }
            };
            fib(n)
        }
    "});

    assert_eq!(fib(1), 1);
    assert_eq!(fib(2), 1);
    assert_eq!(fib(3), 2);
    assert_eq!(fib(4), 3);
}
