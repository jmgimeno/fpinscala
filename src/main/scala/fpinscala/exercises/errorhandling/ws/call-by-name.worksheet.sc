def computeN(): Int =
  println("computing n")
  5

def byValue(n: Int): Int =
  println("in byValue")
  n * n

def byThunk(n: () => Int): Int =
  println("in byThunk")
  n() * n()

def byName(n: => Int): Int =
  println("in byName")
  n * n

println("PASSING BY VALUE")
byValue(computeN())

println("PASSING BY THUNK")
byThunk(() => computeN())

println("PASSING BY NAME")
byName(computeN())
