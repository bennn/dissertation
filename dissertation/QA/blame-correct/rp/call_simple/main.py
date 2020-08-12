from Timer import Timer

"""Microbenchmark for function call overhead.

This measures simple function calls that are not methods, do not use varargs or
kwargs, and do not use tuple unpacking.

bg:
- annotated all parameters + return types
  the first parameter of ALL functions was untyped
- using Timer
- fixed num iterations (see bottom of file)
- removed command-line parsing
"""

def foo(a:int, b:int, c:int, d:int)->Void:
    # 20 calls
    bar(a, b, c)
    bar(a, b, c)
    bar(a, b, c)
    bar(a, b, c)
    bar(a, b, c)
    bar(a, b, c)
    bar(a, b, c)
    bar(a, b, c)
    bar(a, b, c)
    bar(a, b, c)
    bar(a, b, c)
    bar(a, b, c)
    bar(a, b, c)
    bar(a, b, c)
    bar(a, b, c)
    bar(a, b, c)
    bar(a, b, c)
    bar(a, b, c)
    bar(a, b, c)
    bar(a, b, c)


def bar(a:int, b:int, c:int)->Void:
    # 20 calls
    baz(a, b)
    baz(a, b)
    baz(a, b)
    baz(a, b)
    baz(a, b)
    baz(a, b)
    baz(a, b)
    baz(a, b)
    baz(a, b)
    baz(a, b)
    baz(a, b)
    baz(a, b)
    baz(a, b)
    baz(a, b)
    baz(a, b)
    baz(a, b)
    baz(a, b)
    baz(a, b)
    baz(a, b)
    baz(a, b)


def baz(a:int, b:int)->Void:
    # 20 calls
    quux(a)
    quux(a)
    quux(a)
    quux(a)
    quux(a)
    quux(a)
    quux(a)
    quux(a)
    quux(a)
    quux(a)
    quux(a)
    quux(a)
    quux(a)
    quux(a)
    quux(a)
    quux(a)
    quux(a)
    quux(a)
    quux(a)
    quux(a)


def quux(a:int)->Void:
    # 20 calls
    qux()
    qux()
    qux()
    qux()
    qux()
    qux()
    qux()
    qux()
    qux()
    qux()
    qux()
    qux()
    qux()
    qux()
    qux()
    qux()
    qux()
    qux()
    qux()
    qux()


def qux()->Void:
    pass


def test_calls()->Void:
    # 20 calls
    foo(1, 2, 3, 4)
    foo(1, 2, 3, 4)
    foo(1, 2, 3, 4)
    foo(1, 2, 3, 4)
    foo(1, 2, 3, 4)
    foo(1, 2, 3, 4)
    foo(1, 2, 3, 4)
    foo(1, 2, 3, 4)
    foo(1, 2, 3, 4)
    foo(1, 2, 3, 4)
    foo(1, 2, 3, 4)
    foo(1, 2, 3, 4)
    foo(1, 2, 3, 4)
    foo(1, 2, 3, 4)
    foo(1, 2, 3, 4)
    foo(1, 2, 3, 4)
    foo(1, 2, 3, 4)
    foo(1, 2, 3, 4)
    foo(1, 2, 3, 4)
    foo(1, 2, 3, 4)
    return


if __name__ == "__main__":
    t = Timer()
    with t:
        test_calls()
