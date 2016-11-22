// Simple type

def a[T](x:T):String = {
  x match {
    case i:Int => s"Int:$i"
    case s:String => s"String:$s"
    case y:Any => s"???:$y"
  }
}

val a1 = a(3)
val a2 = a("abc")
val a3 = a(false)

// List

def b[T](x:List[Any]):String = {
  x match {
    case i:List[Int] => s"Int:$i"
    case s:List[String@unchecked] => s"String:$s"
    case y:List[Any] => s"???:$y"
  }
}

val b1 = b(List(3))
val b2 = b(List("abc"))
val b3 = b(List(false))

// Case class

trait C
case class C1(s:List[Int]) extends C
case class C2(s:List[String]) extends C
case class C3(s:List[Any]) extends C

def c(x:C):String = {
  x match {
    case C1(i) => s"Int:$i"
    case C2(s) => s"String:$s"
    case C3(y) => s"???:$y"
  }
}

val c1 = c(C1(List(3)))
val c2 = c(C2(List("abc")))
val c3 = c(C3(List[Any](false)))

// TypeTag

import scala.reflect.runtime.universe._

def d[T](x:List[T])(implicit tag:TypeTag[T]):String = {
  val Ti = typeTag[Int]
  val Ts = typeTag[java.lang.String]
  tag match {
    case Ti =>
      val i = x.asInstanceOf[List[Int]]
      s"Int:$i"
    case Ts =>
      val s = x.asInstanceOf[List[String]]
      s"String:$s"
    case _ => s"???:$x"
  }
}

val d1 = d(List(3))
val d2 = d(List("abc"))
val d3 = d(List(false))


