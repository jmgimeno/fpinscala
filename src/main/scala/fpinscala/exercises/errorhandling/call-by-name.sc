def funcio_by_value(n: Int): Unit = {
  println("entering function")
  println(s"n = $n")
  println(s"n = $n")
}

funcio_by_value({
  println("evaluating parameter")
  42
})

def funcio_by_name(n: => Int): Unit = {
  println("entering function")
  println(s"n = $n")
  println(s"n = $n")
}

funcio_by_name({
  println("evaluating parameter")
  42
})

Some(1).getOrElse(Some(4))
Some(1).orElse(Some(4))
