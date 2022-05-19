import fpinscala.exercises.applicative.Applicative
import Applicative.ZipList
import ZipList.*

import fpinscala.exercises.monads.Monad

val ll1 = LazyList(1, 2, 3)
val ll2 = LazyList("a", "b", "c")

// LazyListApplicative (comes from Monad and lacks some combinators)
val lazyListApplicative = summon[Monad[LazyList]]

lazyListApplicative
  .map2(ll1)(ll2)((i, s) => s"$i -> $s")
  .toList

// ZipList Applicative (w/o a Monad)
val zipListApplicative = summon[Applicative[ZipList]]
val zl1 = ZipList.fromLazyList(ll1)
val zl2 = ZipList.fromLazyList(ll2)

zipListApplicative.map2(zl1)(zl2)((i, s) => s"$i -> $s")
  .toLazyList
  .toList

// -----

lazyListApplicative
  .map2(lazyListApplicative
          .unit((i: Int) => i + 1))(ll1)((f, a) => f(a))
  .toList

zipListApplicative
  .map2(zipListApplicative
          .unit((i: Int) => i + 1))(zl1)((f, a) => f(a))
  .toLazyList
  .toList

// -----

val step0: ZipList[Int => String => String] =
  zipListApplicative.unit(i => s => s"$i -> $s")
val step1: ZipList[String => String] =
  zipListApplicative.apply(step0)(zl1)
val step2: ZipList[String] =
  zipListApplicative.apply(step1)(zl2)
val result = step2.toLazyList.toList
