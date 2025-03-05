package fpinscala.exercises.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(_) => 0
    case Branch(l, r) => 1 + math.max(l.depth, r.depth)

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  def fold[B](f: A => B, g: (B, B) => B): B = this match
    case Leaf(a) => f(a)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))

  def sizeViaFold: Int =
    this.fold(_ => 1, (size_of_l, size_of_r) => 1 + size_of_l + size_of_r)

  def depthViaFold: Int =
    this.fold(_ => 0, (depth_of_l, depth_of_r) => 1 + math.max(depth_of_l, depth_of_r))

  def mapViaFold[B](f: A => B): Tree[B] =
    this.fold(a => Leaf(f(a)), (map_of_l, map_of_r) => Branch(map_of_l, map_of_r))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Option[Int] =
    t match
      case Leaf(i) => if i > 0 then Some(i) else None
      case Branch(l, r) => l.firstPositive orElse r.firstPositive

  extension (t: Tree[Int]) def maximum: Int = t match
    case Leaf(a) => a
    case Branch(l,r) => math.max(l.maximum, r.maximum)

  extension (t: Tree[Int]) def maximumViaFold: Int =
    t.fold(a => a, (max_of_l, max_of_r) => math.max(max_of_l, max_of_r))
    //t.fold(identity, math.max)
