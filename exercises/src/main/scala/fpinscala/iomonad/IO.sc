import fpinscala.iomonad.IO2a
import fpinscala.iomonad.IO2aTests

val io42 = IO2aTests.g(42)
IO2a.run(io42)

val ioHello = IO2a.printLine("Hello IO !!!!")
IO2a.run(ioHello)
val ioHelloBad = IO2a.printLine_orig("Hello IO !!!!")
IO2a.run(ioHelloBad)
import fpinscala.iomonad.IO2b
def tailRecFact(n: Int): IO2b.TailRec[Int] =
  if (n == 0) IO2b.Return(1)
  else IO2b.TailRec.suspend(tailRecFact(n-1)
        flatMap (f =>
          IO2b.TailRec.suspend(
            IO2b.Return(f * n ))))
val trFact5 = tailRecFact(5)
IO2b.run(trFact5)
def tailRectFibo(n: Int): IO2b.TailRec[Int] =
  if (n <= 1) IO2b.Return(n)
  else IO2b.TailRec.suspend(
    tailRectFibo(n-1) flatMap (f1 =>
      IO2b.TailRec.suspend(
        tailRectFibo(n-2) flatMap (f2 =>
          IO2b.TailRec.suspend(
            IO2b.Return(f1 + f2))))))

val tFibo6 = tailRectFibo(2)

IO2b.run(tFibo6)

import fpinscala.iomonad.IO2c
import fpinscala.parallelism.Nonblocking._
import java.util.concurrent.Executors
val ES = Executors.newFixedThreadPool(4)
def asyncFact(n: Int): IO2c.Async[Int] =
  if (n == 0) IO2c.Suspend(Par.unit(1))
  else asyncFact(n-1) flatMap (f =>
        IO2c.Suspend(Par.lazyUnit(f * n)))

val aFact5 = asyncFact(5)

val aFact5P = IO2c.run(aFact5)

Par.run(ES)(aFact5P)

def asyncFibo(n: Int): IO2c.Async[Int] =
  if (n <= 1) IO2c.Suspend(Par.lazyUnit(n))
  else asyncFibo(n-1) flatMap ( f1 =>
        asyncFibo(n-2) flatMap (f2 =>
          IO2c.Suspend(Par.lazyUnit((f1 + f2)))))

val aFibo6 = asyncFibo(6)

val aFibo6P = IO2c.run(aFibo6)

Par.run(ES)(aFibo6P)

