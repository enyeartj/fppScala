class Rational(x: Int, y: Int) {
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)
  def numer = x / g
  def denom = y / g

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

  // add takes one argument, the other Rational that is being added is itself
  def add(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  // also:
  //   def neg: Rational = new Rational(-numer, denom)
  def neg = new Rational(-numer, denom)

  def sub(that: Rational) = add(that.neg)

  def mul(that: Rational) =
    new Rational(
      numer * that.numer,
      denom * that.denom
    )

  def div(that: Rational) =
    new Rational(
      numer * that.denom,
      denom * that.numer
    )

  override def toString = numer + "/" + denom
}

val x = new Rational(1, 2)
x.numer
x.denom

val y = new Rational(2, 3)
x.add(y)

x.neg

y.sub(x)

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

// x - y - z
x.sub(y).sub(z)

x.add(y).mul(z)
y.add(y)