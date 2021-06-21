def last[T](xs: List[T]): T = xs match {
  case List() => throw new Error("last of empty list")
  case List(x) => x
  case y :: ys => last(ys)
}

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => xs
  case y :: ys => reverse(ys) ++ List(y)
}

def removeAt[T](xs: List[T], n: Int): List[T] = (xs take n) ::: (xs drop n + 1)

def flatten[T](xs: List[T]): List[T] = xs match {
  case Nil => Nil
  case y :: tail => y match {
    case z :: zs => flatten(List(z)) ++ flatten(tail)
    case z => z :: flatten(tail)
  }
}

def  msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
//def  msort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
  val n = xs.length/2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if (ord.lt(x, y)) x :: merge(xs1, ys)
//        if (lt(x, y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }
    val (fst, snd)= xs splitAt n
    merge(msort(fst), msort(snd))
//    merge(msort(fst)(lt), msort(snd)(lt))
  }
}

val nums = List(1, -4, 5, 7, 1)
msort(nums)
//msort(nums)((x: Int, y: Int) => x < y)

val fruits = List("apple", "pineapple", "orange", "banana")
msort(fruits)
//msort(fruits)((x: String, y: String) => x.compareTo(y) < 0)

val pair = ("answer", 42)
val (label, value) = pair

case class Tuple2[T1, T2](_1:  T1, _2: T2) {
  override def toString = "(" +_1 + "," +_2 + ")"
}

val label = pair._1
val value = pair._2

def scaleList(xs: List[Double], factor: Double): List[Double] = xs match {
  case Nil => xs
  case y ::  ys => y * factor :: scaleList(ys, factor)
}

//abstract class List[T] {
//  def map[U](f: T => U): List[U] = this match {
//    case Nil => this
//    case x :: xs => f(x) :: xs.map(f)
//  }
//
//  def filter(p: T => Boolean): List[T] = this match {
//    case Nil=> this
//    case x :: xs => if (p(x)) x :: xs.filter(p) else xs.filter(p)
//  }
//
//  def reduceLeft(op: (T, T) => T): T = this match {
//    case Nil => throw new Error("Nil.reduceleft")
//    case x :: xs => (xs foldLeft x)(op)
//  }
//
//  def foldLeft[U](z: U)(op: (U, T) => U): U = this match {
//    case Nil => z
//    case x :: xs => (xs foldLeft op(z, x))(op)
//  }
//
//  def reduceRight(op: (T, T) => T): T = this match {
//    case Nil => throw new Error("Nil.reduceRight")
//    case x :: Nil => x
//    case x :: xs => op(x, xs.reduceRight(op))
//  }
//
//  def foldRight[U](z: U)(op: (T, U) => U): U = this match {
//    case Nil => z
//    case x :: xs => op(x, xs foldRight z)(op))
//  }
//}

def scaleList2(xs: List[Double], factor: Double): List[Double] = xs.map(x => x * factor)

def squareList(xs: List[Int]): List[Int] = xs match {
  case Nil => xs
  case y :: ys => y * y :: squareList(ys)
}

def squareList2(xs: List[Int]): List[Int] = xs.map(x => x * x)

def posElems(xs: List[Int]): List[Int] = xs match {
  case Nil => xs
  case y :: ys => if (y > 0) y ::  posElems(ys) else posElems(ys)
}

def posElems2(xs: List[Int]): List[Int] = xs.filter(x => x > 0)

object listfun {
  val nums = List(2, -4, 5, 7, 1)
  val fruits = List("apple", "pineapple", "orange", "banana")

  nums filter(x => x > 0)
  nums filterNot(x => x > 0)
  nums partition(x => x > 0)

  nums takeWhile(x => x > 0)
  nums dropWhile(x => x > 0)
  nums span(x => x > 0)

  val data = List("a", "a", "a", "b", "c", "c", "a")

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 => {
      val (first, rest) = xs span (y => y == x)
      first :: pack(rest)
    }
  }

  pack(data)

  def encode[T](xs: List[T]): List[(T, Int)] = {
    pack(xs).map(ys => (ys.head, ys.length))
  }

  encode(data)
}

def sum(xs: List[Int]): Int = xs match {
  case Nil => 0
  case y :: ys => y + sum(ys)
}

def product(xs: List[Int]): Int = xs match {
  case Nil => 0
  case y :: ys => y * product(ys)
}

def sum2(xs: List[Int]) = (0 :: xs) reduceLeft ((x, y) => x + y)
def sum3(xs: List[Int]) = (0 :: xs) reduceLeft (_ + _)
def product2(xs: List[Int]) = (1 :: xs) reduceLeft ((x, y) => x * y)
def product3(xs: List[Int]) = (1 :: xs) reduceLeft (_ * _)

def sum4(xs: List[Int]) = (xs foldLeft 0) (_ + _)
def product4(xs: List[Int]) = (xs foldLeft 1) (_ * _)

def concat[T](xs: List[T], ys: List[T]): List[T] = (xs foldRight ys) (_ :: _)

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())((h: T,t: List[U]) => { (f(h) :: t)})

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((_, acc) => acc + 1)

object test {
  val xs = Array(1, 2, 3, 44)
  xs map (x => x * 2)

  val s = "Hello World"
  s filter (c => c.isUpper) // "HW"
  s exists (c => c.isUpper) // true
  s forall (c => c.isUpper) // false

  val pairs = List(1, 2, 3) zip s // List((1, H), (2, e), (3, l)
  pairs.unzip // (List(1, 2, 3), List(H, e, l))

  s flatMap  (c => List('.', c)) // .H.e.l.l.o. .W.o.r.l.d
  xs.sum // 50
  xs.product // 264
  xs.max // 44
  xs.min // 1

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
//    (xs zip ys).map(xy => xy._1 * xy._2).sum
//    (xs zip ys).map({ case (x, y) => x * y }).sum
    (for ((x, y)  <- xs zip ys) yield x * y).sum

  val r: Range = 1 until 5
  val r2: Range = 1 to 5
  1 to  10 by 3
  6 to 1 by -2
}

def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)

object pairs {
  val n = 7

  (1 until n) flatMap (i =>
    (1 until i) map (j => (i, j))) filter (pair => isPrime(pair._1 + pair._2))

  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i, j)
}

case class Person(name: String, age: Int)
//for (p <- persons if  p.age > 20) yield p.name
//persons.filter(p => p.age > 20) map (p => p.name)

val fruit = Set("apple", "banana", "pear")
val s = (1 to 6).toSet

s map (_ + 2) // Set(3, 4, 5, 6, 7, 8)
fruit filter (_.startsWith("app")) // Set("apple")
s.nonEmpty // true
s map (_ / 2) // Set(2, 0, 3, 1)
s contains 5 // true

object nqueens {
  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens
    }
    placeQueens(n)
  }

  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val queensWithRow = (row - 1 to 0 by -1) zip queens
    queensWithRow forall {
      case (r, c) => col != c && math.abs(col - c) != row - r
    }
  }

  def show(queens: List[Int]) = {
    val lines =
      for (col <- queens.reverse)
        yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
    "\n" + (lines mkString "\n")
  }

  queens(8) take 3 map show
}

object maps {
  val romanNumerals = Map("I" -> 1, "V" -> "X" -> 10)
  val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")

  capitalOfCountry("US")
  capitalOfCountry("andorra") // throws NoSuchElementException

  capitalOfCountry get "andorra" // None
  capitalOfCountry get "US" // Some(Washington)

  def showCapital(country: String) = capitalOfCountry.get(country) match {
    case Some(capital) => capital
    case None => "missing data"
  }

  val cap1 = capitalOfCountry withDefaultValue "<unknown>"
  cap1("Andorra") // "<unknown>"

  val fruit = List("apple", "pear", "orange", "pineapple")
  fruit sortWith (_.length < _.length) // List("pear", "orange", "apple", "pineapple")
  fruit.sorted // List("apple", "orange", "pear", "pineapple")
  fruit.groupBy (_.head) // Map(p -> List("pear", "pineapple"), a -> List("apple"), o -> List("orange"))
}

object polynomials {
  class Poly(val terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    val terms = terms0 withDefaultValue 0.0
//    def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
    def + (other: Poly) = {
      new Poly((other.terms foldLeft terms)(addTerm))
      def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
        val (exp, coeff) = term
        terms + (exp -> (coeff + terms(exp)))
      }
    }

//    def adjust(term: (Int, Double)): (Int, Double) = {
//      val (exp, coeff) = term
//      exp -> (coeff + terms(exp))
//    }
    override def toString =
      (for ((exp,  coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString " + "

  }

  val p1 = new Poly(1 -> 2.0, 3 -> 4.0,  5 -> 6.2)
  val p2 = new Poly(0 -> 3.0, 3 -> 7.0)
  p1 + p2
}

import scala.io.Source
object x {
  val in = Source.fromFile("./src/main/resources/forcomp/linuxwords.txt")

  val words = in.getLines

//  val words = List("Java", "Scala", "is", "fun")

  val mnem = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI",
    '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS",
    '8' -> "TUV", '9' -> "WXYZ"
  )

  val charCode: Map[Char, Char] =
    for ((digit, str) <- mnem; ltr <- str) yield ltr -> digit

  def wordCode(word: String): String =
    word.toUpperCase map charCode

  wordCode("JAVA")
  wordCode("Java")

  val wordsForNum: Map[String, Seq[String]] =
    words groupBy wordCode withDefaultValue Seq()

  def encode(number: String): Set[List[String]] = {
    if (number.isEmpty) Set(List())
    else {
      for {
        split <- 1 to number.length
        word <- wordsForNum(number take split)
        rest <- encode(number drop split)
      } yield word :: rest
    }.toSet
  }

  encode("7225247386")

  def translate(number: String): Set[String] =
    encode(number) map (_ mkString " ")

  translate("7225247386")

  def decode(phrase: String): String =
    phrase.replace(" ", "").toUpperCase.map(charCode)
}



