def f(n: Int) =
  println(s"f $n")
  n

def g(acc: Int)(h:(Int, Int) => Int) =
  println("g")
  h(acc, acc)

g(f(1))(_ + _)

def g2(acc: => Int)(h:(Int, Int) => Int) =
  println("g2")
  h(acc, acc)

g2(f(1))(_ + _)

g2(f(1)) { (a, b) =>
  println("lambda")
  a + b
}

/*

This works but fals inside a worksheet inside IntelliJ (I suppose it is
the magic that makes worksheets possible):

def g3(acc: => Int)(h: (=>Int, =>Int) => Int) =
  println("g3")
  h(acc, acc)

g3(f(1)) { (a, b) =>
  println("lambda")
  a + b
}

And it prints:

g3
lambda
f 1
f 1

*/
