import fpinscala.exercises.monads.Monad

def even(n: Int): Boolean = n % 2 == 0

val l = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

// filter requereix una funció pura
l.filter(even)

def evenNoFallaMai(n: Int): Option[Boolean] =
  Some(n % 2 == 0)

// F=Option indica que pot fallar
val monadOption = summon[Monad[Option]]

monadOption.filterM(l)(evenNoFallaMai)

def evenFallaDeVegades(n: Int): Option[Boolean] =
  if n != 5 then Some(n % 2 == 0) else None

monadOption.filterM(l)(evenFallaDeVegades)

// F=List indica que hi ha vàries possibilitats

val monadList = summon[Monad[List]]

def evenDeterminista(n: Int): List[Boolean] =
  List(n % 2 == 0)

monadList.filterM(l)(evenDeterminista)

def evenOddCombined(n: Int): List[Boolean] =
  if n % 2 == 0 then List(true)
  else List(false, true)

monadList.filterM(l)(evenOddCombined)

monadList.filterM(List())(evenOddCombined)
monadList.filterM(List(0))(evenOddCombined)
monadList.filterM(List(1))(evenOddCombined)
monadList.filterM(List(1, 3))(evenOddCombined)

monadList.filterM(List(1,2,3))(_ => List(true, false))


