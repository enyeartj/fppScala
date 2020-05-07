class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y

  // add takes one argument, the other Rational that is being added is itself
  def add(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  override def toString = numer + "/" + denom
}

val x = new Rational(1, 2)
x.numer
x.denom

val y = new Rational(2, 3)
x.add(y)