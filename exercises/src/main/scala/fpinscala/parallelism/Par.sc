import java.util.concurrent.Executors

import fpinscala.parallelism.Par._

val executorService = Executors.newFixedThreadPool(4)

val ints = List.range(1, 50)

val map = parMap(ints)(_ * 2)

map(executorService).get

val filter = parFilter(ints)(_ % 2 == 0)

filter(executorService).get

val filter2 = parFilter2(ints)(_ % 2 == 0)

filter2(executorService).get
