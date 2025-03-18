
def f[A, B, C](a: => A, b: => B, g: (A, B) => C): C =
  g(a, b)

f(sys.error("a"), sys.error("b"), (a, b) => a)
f(12, sys.error("b"), (a, b) => a)

def f2[A, B, C](a: => A, b: => B, g: (A, => B) => C): C =
  g(a, b)

f2(sys.error("a"), sys.error("b"), (a, b) => a)
f2(12, sys.error("b"), (a, b) => a)

def f3[A, B, C](a: => A, b: => B, g: (=> A, => B) => C): C =
  g(a, b)

f3(sys.error("a"), sys.error("b"), (a, b) => 42)
