// Classe / Subclasse / Superclasse

class Animal
class Dog extends Animal
class Cat extends Animal

def save(a: Animal) =
  42

save(new Animal)
save(new Dog)

// Genèrics

class Container[A]

def saveAll(c: Container[Animal]) =
  54

val containerAnimal = new Container[Animal]

saveAll(containerAnimal)

val containerDog = new Container[Dog]

// Per defecte els tipus són INVARIANTS respecte els paràmetres genérics
// Dog subclasse Animal =/=> Container[Dog] subclasse Container[Animal]

// saveAll(containerDog)

class CovariantContainer[+A]

// Amb el + indiquem que el tipus es COVARIANT respecte del paràmetre
// Dog subclasse Animal ==> Container[Dog] subclasse Container[Animal]

def saveAll2(c: CovariantContainer[Animal]) =
  33

val dogContainer2 = new CovariantContainer[Dog]

saveAll2(dogContainer2)

/*
int saveAll(Container<Animal> c) { }

int saveAll2(Container<? extends Animal> c) { }
 */

// Col·leccions IMMUTABLES
class Collection[+E]:
  def add[E1 >: E](e: E1): Collection[E1] =
    this

val col = new Collection[Dog]
col.add(new Dog)

val dogs: Collection[Dog] = new Collection[Dog].add(new Dog).add(new Dog)
val dogs2: Collection[Animal] = new Collection[Dog].add(new Dog).add(new Dog)
val animals: Collection[Animal] = new Collection[Dog].add(new Dog).add(new Cat)
// val dogs3: Collection[Dog] = new Collection[Dog].add(new Dog).add(new Cat)
