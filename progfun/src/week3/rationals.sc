class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y

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