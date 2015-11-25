import java.util.concurrent.Executors

import fpinscala.parallelism.Nonblocking.Par._

val es = Executors.newFixedThreadPool(4)

val ints = List.range(1, 1000)

val map = parMap(ints)(_ * 2)

run(es)(map)

val filter = parFilter(ints)(_ % 2 == 0)

run(es)(filter)

