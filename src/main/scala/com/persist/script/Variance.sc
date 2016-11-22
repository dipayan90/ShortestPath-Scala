import scala.collection.mutable
import scala.collection.immutable

sealed trait Animal {
  val name: String
}

case class Cat(name: String) extends Animal

case class Dog(name: String) extends Animal

case class Cow(name: String)

val fluffy = new Cat("fluffy")
val rover = new Dog("rover")
val bess = new Cow("bess")

// Simple parameter (<= parameter, >= result


def fa(a: Animal) = {println(a.name);a}
def fc(a: Cat) = {println(a.name);a}

fa(fluffy)  // to wider
//fa(bess)

val a1:Animal= fa(fluffy)
val a2:Animal = fc(fluffy)// to wider

// Mutable sequence (== parameter and result)

def fsa(a: mutable.Seq[Animal]) = {a.map(println(_)); a}
def fsc(a: mutable.Seq[Cat]) = {a.map(println(_)); a}

val sanimal = mutable.Seq[Animal](fluffy)
val scat = mutable.Seq[Cat](fluffy)

fsa(sanimal)
//fsa(scat)
fsc(scat)
val as1:mutable.Seq[Animal] = fsa(sanimal)
//val as2:mutable.Seq[Animal] = fsc(scat)

// Immutable sequence

def ifsa(a: immutable.Seq[Animal]) = {a.map(println(_)); a}
def ifsc(a: immutable.Seq[Cat]) = {a.map(println(_)); a}

val isanimal = immutable.Seq[Animal](fluffy)
val iscat = immutable.Seq[Cat](fluffy)

ifsa(isanimal)
ifsa(iscat)
val ias1:immutable.Seq[Animal] = ifsa(isanimal)
val ias2:immutable.Seq[Animal] = ifsc(iscat)

// Bounds

def vfsa[T<:Animal](a: immutable.Seq[T]) = {a.map(println(_)); a}

vfsa(isanimal)
vfsa(iscat)  // Type is Seq[Cat] not Seq[Animal]

// Covariance(+)  and Contravariance (-)

case class W[+T,-U]() {
  def f(x:U):T = x.asInstanceOf[T]
  //def g(x:T):T
  //def h(x:U):U
}

// W[Cat,Animal] < W[Animal,Cat]

def w1:W[Animal,Cat] = W[Cat,Animal]()
def W2:W[Cat,Animal] = W[Animal,Cat]()




