abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero = true
  def predecessor = throw new Error("0.predecessor")
  def +(that: Nat) = that
  def -(that: Nat) = if (that.isZero) this else throw new Error("negative number")
}

class Succ(n: Nat) extends Nat {
  def isZero = false
  def predecessor = n
  def +(that: Nat) = new Succ(n + that)
  def -(that: Nat) = if (that.isZero) this else n - that.predecessor
}

//trait List[+T] {
//  def isEmpty: Boolean
//  def head: T
//  def tail: List[T]
//  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
//}
class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}
object Nil extends List[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object test {
  val x: List[String] = Nil
//  def f(xs: List[NonEmpty], x: Empty) = xs prepend x
}

//object List {
//  // List(1, 2) = List.apply(1, 2)
//  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))
//  def apply[T]() = new Nil
//}

trait Expr {
//  def isNumber: Boolean
//  def isSum: Boolean
//  def numValue: Int
//  def leftOp: Expr
//  def rightOp: Expr
//  def eval: Int
}

case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr

//class Number(n: Int) extends Expr {
//  def isNumber: Boolean = true
//  def isSum: Boolean = false
//  def numValue: Int = n
//  def leftOp: Expr = throw new Error("Number.leftOp")
//  def rightOp: Expr = throw new Error("Number.rightOp")
//  def eval: Int = n
//}
//
//class Sum(e1: Expr, e2: Expr) extends Expr {
//  def isNumber: Boolean = false
//  def isSum: Boolean = true
//  def numValue: Int = throw new Error("Sum.numValue")
//  def leftOp: Expr = e1
//  def rightOp: Expr = e2
//  def eval: Int = e1.eval + e2.eval
//}

object exprs {
  def show(e: Expr): String = e match {
    case Number(x) => x.toString
    case Sum(l, r) => show(l) + " + " + show(r)
  }
}

def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
}

//def eval(e: Expr): Int = e match {
//  case Number(n) => n
////  else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
////  else throw new Error("Unknown expression " + e)
//}
