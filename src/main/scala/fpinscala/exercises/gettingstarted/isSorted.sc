import fpinscala.exercises.gettingstarted.PolymorphicFunctions.*

val sorted = Array(1, 2, 3, 4, 5)

isSorted(sorted, (x, y) => (x <= y))

isSorted(sorted, _ <= _)

val unsorted = Array(1, 2, 8, 4, 5)

isSorted(unsorted, _ <= _)
