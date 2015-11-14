import collection.immutable.List

val l = List(1, 2, 3, 4)

// Must be done with our List implementation

def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
  if (sup.length < sub.length) false
  else sup.take(sub.length) == sub || hasSubsequence(sup.tail, sub)

hasSubsequence(List(1,2,3,4), List(2,3))
