
def secondByValue[A, B](a: A, b: B): A = a

def secondByName[A, B](a: A, b: => B): A = a

def paramLambdaByValue[A, B, C](a: => A, b: => B, g: (A, B) => A): A =
  // the type of g is what makes whether the arguments are passed by value or by name
  // when calling g: both a and b are evaluated by value,
  g(a, b)

// First argument of g is passed always by value, so this always gets an error

// paramLambdaByValue(sys.error("a"), sys.error("b"), (a, b) => a)

// Here we pass a lambda (anonymous function) which does not use the second argument
// but when passing it to paramLambdaByValue this second argument is always evaluated
// when calling it

// paramLambdaByValue(12, sys.error("b"), (a, b) => a)

// When passing a method, we are really passing a lambda which calls the method. And
// in this case, both arguments of the lambda are evaluated before calling the method so,
// irrespectively to the type in the method, the second argument is evaluated.

// paramLambdaByValue(12, sys.error("b"), secondByValue)
// paramLambdaByValue(12, sys.error("b"), (a, b) => secondByValue(a, b))
// paramLambdaByValue(12, sys.error("b"), secondByName)
// paramLambdaByValue(12, sys.error("b"), (a, b) => secondByName(a, b))

def paramLambdaByName[A, B, C](a: => A, b: => B, g: (A, => B) => C): C =
  // the type of g is what makes whether the arguments are passed by value or by name
  // when calling g: a is evaluated by value (before calling g), b by name (inside g and
  // only when g uses it)
  g(a, b)

// The same as before

//paramLambdaByName(sys.error("a"), sys.error("b"), (a, b) => a)

// Here we pass a lambda (anonymous function) which does not use the second argument
// and when passing it to paramLambdaByValue this second argument is passed by name, so
// there is no error

paramLambdaByName(12, sys.error("b"), (a, b) => a)

// This case is tricky because the lambda that really is passed does not evaluate its second
// argument (by g), but when calling secondByValue we evaluate it before the call, so the
// error is produced.

//paramLambdaByName(12, sys.error("b"), secondByValue)
//paramLambdaByName(12, sys.error("b"), (a, b) => secondByValue(a, b))

// As before, but now as secondByName does not use the second parameter, b is never evaluated.

paramLambdaByName(12, sys.error("b"), secondByName)
paramLambdaByName(12, sys.error("b"), (a, b) => secondByName(a, b))




