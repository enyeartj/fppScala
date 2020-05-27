class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  def numer = x
  def denom = y

  /* Alternative Constructor 1:
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  def numer = x / gcd(x, y)
  def denom = y / gcd(x, y)

  drop the g val and calculate directly. Could be slow if numer and denom
  are called frequently
   */

  /* Alternative Constructor 2:
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  val numer = x / gcd(x, y)
  val denom = y / gcd(x, y)

  numer and denom only get calculated once. Good if used frequently.
   */

  def < (that: Rational) = numer * that.denom < that.numer * denom

  def max(that: Rational) = if (this < that) that else this

  // add takes one argument, the other Rational that is being added is itself
  def + (that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  // also:
  //   def neg: Rational = new Rational(-numer, denom)
  def unary_- = new Rational(-numer, denom)

  def - (that: Rational) = this + -that

  def * (that: Rational) =
    new Rational(
      numer * that.numer,
      denom * that.denom
    )

  def / (that: Rational) =
    new Rational(
      numer * that.denom,
      denom * that.numer
    )

  override def toString = {
    def g = gcd(numer, denom)
    numer / g + "/" + denom / g
  }
}

val x = new Rational(1, 2)
x.numer
x.denom

val y = new Rational(2, 3)
x + y
-x

y - x
val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

// x - y - z
x - y - z

x + y * z
y + y
x < y
x max y
//val strange = new Rational(1, 0)
val five = new Rational(5)