package fpinscala.exercises.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match {
    case Leaf(value) => 0
    case Branch(left, right) => math.max(left.depth, right.depth) + 1
  }

  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(left.map(f), right.map(f))
  }

  /*
  List[+A]
  def map[B](f: A => B): List[B] = this match
    case Nil => Nil
    case Cons(x,xs) => Cons(f(x),xs.map(f))
  */
  
  // equivalent to foldRight on lists
  def fold[B](f: A => B, g: (B,B) => B): B = this match {
    case Tree.Leaf(value) => f(value)
    case Tree.Branch(left, right) => g(left.fold(f, g), right.fold(f, g))  
  }
  
  /* enum List[+A]:
       case Nil
       case Cons(x: A,xs: List[A]) 
                                             B
      // Nil -> B                  ----------------------
      // (h: A, t: List[A]) -> (A, res crida rec sobre t) -> B
       def foldRight(z: B, f: (A,B) => B): B = this match {
         case Nil => z
         case Cons(x,xs) => f(x,xs.foldRight(z,f))
       }
  
     enum Tree[+A]:
       case Leaf(value: A)
       case Branch(left: Tree[A], right: Tree[A])
                                                         B                         B
       // A -> B                              ------------------------  -------------------------
       // (left: Tree[A], right: Tree[A]) -> (res crida rec sobre left, res crida rec sobre right) -> B
   */
  
  def sizeViaFold: Int =
    this.fold(_ => 1, 1 + _ + _)
  
  def depthViaFold: Int =
    this.fold(_ => 0, (d1,d2) => math.max(d1,d2) + 1)
  
  def mapViaFold[B](f: A => B): Tree[B] =
    this.fold(a => Leaf(f(a)), Branch(_,_))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)

  // This function IMHO has no sense: its return type
  // is `Int` but it should be `Option[Int]` because we
  // cannot guarantee that the tree contains a positive
  // integer.
  extension (t: Tree[Int]) def firstPositive: Int = ???

  extension (t: Tree[Int]) def maximum: Int = t match {
    case Tree.Leaf(value) => value
    case Tree.Branch(left, right) => math.max(left.maximum, right.maximum)
  }

  extension (t: Tree[Int]) def maximumViaFold: Int =
    t.fold(identity, math.max)
