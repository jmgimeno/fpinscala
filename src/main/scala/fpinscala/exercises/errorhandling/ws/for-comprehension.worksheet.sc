val l1 = List(1, 2, 3)
val l2 = List("a", "b")

for {
  n <- l1
  s <- l2
} yield s * n

