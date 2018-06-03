case class Rational(x: Int, y: Int) {
  require(y != 0, "Denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  private def gdc(a: Int, b: Int): Int = if (b == 0) a else gdc(b, a % b)

  val numerator = x / gdc(x, y)
  val denominator = y / gdc(x, y)

  def +(s: Rational): Rational =
    Rational(numerator * s.denominator + s.numerator * denominator,
      denominator * s.denominator)

  def -(s: Rational): Rational = this + -s

  def multiplicate(s: Rational): Rational =
    Rational(numerator * s.numerator, denominator * s.denominator)

  def divide(s: Rational): Rational =
    Rational(numerator * s.denominator, denominator * s.numerator)

  def unary_- : Rational = Rational(-numerator, denominator)

  def <(s: Rational): Boolean = numerator * s.denominator < s.numerator * denominator

  def max(s: Rational): Rational = if (this < s) s else this

  override def toString = s"$numerator/$denominator"

}

val x = Rational(1, 3)
val y = Rational(5, 7)
val z = Rational(3, 2)

x + y
x - y
x - y - z

x < y
x max y

// ((a + b) ^? (c ?^ d)) less ((a ==> b) | c)