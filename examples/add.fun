add = fun(a: int, b: int) -> int { a + b };
fooby = fun(a: int, b: int) -> int { add(a, b) };

# nooby = fun(a: int) -> int { fooby(a, 10) };
# nooby(3)

# (fun(a: int, b: int) -> int { a + b })(1, 2)

fooby(1, 2);

sub = fun(a: int, b: int) -> int { b - a };

sub(10, 10);

mul = fun(a: int, b: int) -> int { a * b };

mul(12, 105)
