import fpinscala.exercises.laziness.LazyList
import LazyList.*

val constructors =
  Cons(
    () => {
      println("primer"); 42
    },
    () => Cons(
      () => {
        println("segon"); 23
      },
      () => Empty))

println("lazy list creada")

// Cada vegada s'avalua el "crear" el primer element
constructors.headOption
constructors.headOption

val bad_smart_constuctors =
  bad_cons(
    {
      println("primer"); 42
    },
    bad_cons({
      println("segon"); 23
    },
      empty))

// Torna a passar el mateix
bad_smart_constuctors.headOption
bad_smart_constuctors.headOption

val good_smart_constuctors =
  cons(
    {
      println("primer"); 42
    },
    cons({
      println("segon"); 23
    },
      empty))

// Ara només s'avalúa la primera vegada
good_smart_constuctors.headOption
good_smart_constuctors.headOption

// --------------------------------

val costly_lazy_list =
  cons(
    {
      println("costly computing first element"); 1
    },
    {
      println("costly computing first tail")
      cons({
        println("costly computing second element"); 2
      },
        {
          println("costly computing second tail")
          cons({
            println("costly computing third element"); 3
          },
            empty)
        })
    })

costly_lazy_list.bad_take(1).toList

val costly_lazy_list2 =
  cons(
    {
      println("costly computing first element"); 1
    },
    {
      println("costly computing first tail")
      cons({
        println("costly computing second element"); 2
      },
        {
          println("costly computing second tail")
          cons({
            println("costly computing third element"); 3
          },
            empty)
        })
    })

// Si només cridem al take, NO s'avalúa cap part !!
costly_lazy_list2.take(1)

// Només quan realment es força l'avaluació (p.e. al crear una llista)
// sortim de la "laziness" i avaluem.
costly_lazy_list2.take(1).toList

// takewhile only evaluates each element once !!!

val costly_lazy_list3 =
  cons(
    {
      println("costly computing first element"); 1
    },
    {
      println("costly computing first tail")
      cons({
        println("costly computing second element"); 2
      },
        {
          println("costly computing second tail")
          cons({
            println("costly computing third element"); 3
          },
            empty)
        })
    })

costly_lazy_list3.takeWhile(_ < 3).toList

fibs.take(10)




