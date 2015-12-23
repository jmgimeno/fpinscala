import fpinscala.iomonad.IO3._
import fpinscala.iomonad.IO3.Console._

val f1: ConsoleIO[Option[String]] = for {
  _  <- printLn("I can only interact with the console.")
  ln <- readLn
} yield ln

val thunk = runConsoleFunction0(f1)
val par = runConsolePar(f1)

//thunk()

