use indoc::indoc;

use super::*;

fn compile_fun<I, O>(src: &str) -> fn(I) -> O {
    let fun = compile_standalone_fun(src).unwrap();
    unsafe { mem::transmute(fun) }
}

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
