def if_strict[A](cond: Boolean, onTrue: A, onFalse: A): A =
  if cond then onTrue else onFalse

def if_lazy[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
  if cond then onTrue() else onFalse()

val a = 25

if_strict(a < 2, println("Ok strict"), println("No strict"))

if_lazy(a < 2, () => println("Ok lazy"), () => println("No lazy"))
