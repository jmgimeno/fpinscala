
def secondByValue[A, B](a: A, b: B): A = a

def secondByName[A, B](a: A, b: => B): A = a

def paramLambdaByValue[A, B, C](a: => A, b: => B, g: (A, B) => A): A =
  g(a, b)

//paramLambdaByValue(sys.error("a"), sys.error("b"), (a, b) => a)
//paramLambdaByValue(12, sys.error("b"), (a, b) => a)
//paramLambdaByValue(12, sys.error("b"), secondByValue)
//paramLambdaByValue(12, sys.error("b"), secondByName)

def paramLambdaByName[A, B, C](a: => A, b: => B, g: (A, => B) => C): C =
  g(a, b)

//paramLambdaByName(sys.error("a"), sys.error("b"), (a, b) => a)
//paramLambdaByName(12, sys.error("b"), (a, b) => a)
//paramLambdaByName(12, sys.error("b"), secondByValue)
//paramLambdaByName(12, sys.error("b"), secondByName)

// Polimorphic lambdas (advanced)
val lambda = [A, B] => (a: A, b: B) => a

//paramLambdaByValue(12, sys.error("b"), lambda[Int, Any])
//paramLambdaByName(12, sys.error("b"), lambda[Int, Any])


