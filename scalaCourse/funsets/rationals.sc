class Rational(x: Int, y: Int) {
  require(y != 0, "demoninator must be nonzero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = {
    if (b == 0)
      a
    else
      gcd(b, a % b)
  }

  def numer = x
  def denom = y


  def + (that: Rational) = {
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )
  }

  def unary_- : Rational = new Rational(-numer, denom)
  def - (that: Rational) = this + -that
  def < (that: Rational) = numer * that.denom < that.numer * denom
  def max(that: Rational) = if (this < that) that else this

  override def toString() = {
    val g = gcd(numer, denom)
    numer / g + "/" + denom / g
  }
}


//val x = new Rational(1, 2)
//x.numer
//x.denom
//
//val y = new Rational(2, 3)
//x.add(y)

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
x - y - z
y + y
x < y
x max y
new Rational(2)
