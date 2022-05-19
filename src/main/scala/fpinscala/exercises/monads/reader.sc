import fpinscala.exercises.monads.Monad
import Monad.{*, given}
import fpinscala.exercises.monads.Reader
import Reader.*

case class Configuration(host: String, port: Int, dbname: String)
case class Webserver(host: String, port: Int)
case class DBserver(dbname: String)

case class Application(web: Webserver, db: DBserver):
  def start(): Unit = println("running app")

val webserver: Reader[Configuration,Webserver] =
  Reader( (conf: Configuration) =>
    println(s"serving on ${conf.host} at ${conf.port}")
    Webserver(conf.host, conf.port)
  )

val dbserver: Reader[Configuration, DBserver] =
  Reader((conf: Configuration) =>
    println(s"data defined at ${conf.dbname}")
    DBserver(conf.dbname)
  )

val maker: Reader[Configuration, Application] =
  for
    web <- webserver
    db <- dbserver
  yield Application(web, db)

val app = maker.run(Configuration("patata.com", 42, "pomadb"))
app.start()

// Another way to do it

def webserver2(conf: Configuration) =
    println(s"serving on ${conf.host} at ${conf.port}")
    Webserver(conf.host, conf.port)

def dbserver2(conf: Configuration) =
    println(s"data defined at ${conf.dbname}")
    DBserver(conf.dbname)

val maker2: Reader[Configuration, Application] =
  for
    web <- Reader(webserver2)
    db  <- Reader(dbserver2)
  yield Application(web, db)

val app2 = maker2.run(Configuration("patata.com", 42, "pomadb"))
app2.start()
