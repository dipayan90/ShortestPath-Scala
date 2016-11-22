// Iterable has Iterator with hasNext next

val s = List(1,2,3,4)

val it = s.iterator

it.hasNext
it.next()
it.hasNext
it.next()

case class L(v:String,next:Option[L]) extends Iterable[String] {
  class I(init:L) extends Iterator[String] {
    var curr:Option[L] = Some(init)

    override def hasNext: Boolean = curr match {
      case Some(l) => true
      case None => false
    }

    override def next(): String = curr match {
      case Some(l) =>
        val s = l.v
        curr = l.next
        s
      case None => throw new Exception("no next")
    }
  }
  override def iterator: Iterator[String] = new I(this)
}

val l = L("abc",Some(L("def",None)))

val lit = l.iterator
lit.hasNext
lit.next
it.hasNext
lit.next
lit.hasNext


// Traversable has foreach

s.foreach(println(_))

sealed trait Tree extends Traversable[String]

case class Inner(left:Tree,right:Tree) extends Tree {
  override def foreach[U](f: (String) => U): Unit = {
    for (s<-left) f(s)
    for (s<-right) f(s)
  }
}

case class Leaf(s:String) extends Tree {
  override def foreach[U](f: (String) => U): Unit = f(s)
}

// Iterable is a subtrait of Traversable
// anything that is iterable is traversable
// but not vis-versa

l.foreach(println(_))

// go to source of foreach to see details


// Streams

val t = Inner(Inner(Leaf("ABC"),Leaf("X")),Leaf("??"))
for (i<-t) println(i)

val s1 = Stream.from(1)
val x = s1.filter(_%5==0).drop(3).take(5).toList

val  s2:Stream[Int] = 2 #::  s2.map(2*_)
s2.take(5).toList

