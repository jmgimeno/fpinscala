package fpinscala.exercises.datastructures

import scala.annotation.tailrec
import scala.math.max

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int =
    this match
      case Leaf(_)             => 0 // The test defines the depth of a leaf as 0
      case Branch(left, right) => 1 + max(left.depth, right.depth)

  def map[B](f: A => B): Tree[B] =
    this match
      case Leaf(a)             => Leaf(f(a))
      case Branch(left, right) => Branch(left.map(f), right.map(f))

  def fold[B](f: A => B, g: (B, B) => B): B =
    this match
      case Leaf(a)             => f(a)
      case Branch(left, right) => g(left.fold(f, g), right.fold(f, g))

  def foldTailRec[B](f: A => B, g: (B, B) => B): B = {
    enum Context {
      case Call(tree: Tree[A])
      case AfterLeft(leftFold: B)
      case Result(result: B)
    }
    import Context.*
    import scala.collection.immutable.List
    @tailrec
    def go(stack: List[Context]): B = (stack : @unchecked) match {
      case Call(tree @ Leaf(a)) :: rest => go(Result(f(a)) :: rest)
      case Call(tree @ Branch(left, right)) :: rest =>
        go(Call(left) :: Call(right) :: rest)
      case Result(result) :: Nil => result
      case Result(leftFold) :: Call(right) :: rest =>
        go(Call(right) :: AfterLeft(leftFold) :: rest)
      case Result(rightFold) :: AfterLeft(leftFold) :: rest =>
        go(Result(g(leftFold, rightFold)) :: rest)
    }
    go(List(Call(this)))
  }

  def sizeViaFold: Int =
    this.fold(_ => 1, (leftSize, rightSize) => 1 + leftSize + rightSize)

  def depthViaFold: Int =
    fold(_ => 0, (leftDepth, rightDepth) => 1 + (leftDepth max rightDepth))

  def mapViaFold[B](f: A => B): Tree[B] =
    fold(
      leafValue => Leaf(f(leafValue)),
      (leftMap, rightMap) => Branch(leftMap, rightMap)
    )

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + size(l) + size(r)

  // here we have problems with this method
  // what is the value to return when there is no positive value?
  // NOTE: A much better solution will be presented in the next chapter !!
  extension (t: Tree[Int])
    def firstPositive: Int = ???
    def maximum: Int = ???

  // extension (t: Tree[Int]) def maximum: Int = ???

  extension (t: Tree[Int]) def maximumViaFold: Int = ???
