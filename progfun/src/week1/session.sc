1+3

def abs(x: Double) = if (x < 0) -x else x


def isGoodEnough(guess: Double, x: Double) =
  abs(guess * guess - x) < 0.001

def improve(guess: Double, x: Double) =
  (x / guess + guess) / 2

// Recursive functions require you to specify return type
def sqrtIter(guess: Double, x: Double): Double =
  if (isGoodEnough(guess, x)) guess
  else sqrtIter(improve(guess, x), x)

// Other functions don't need to return type specified
// because they can calculate RHS without going into self loop
def sqrt0(x: Double) =
  sqrtIter(1.0, x)

sqrt0(2)
sqrt0(4)
sqrt0(1e-6)  // not precise because the good enough threshold is bigger than the number
//sqrt0(1e60)  // doesn't terminate because good enough threshold is always smaller than diff
            // because floating point mantissa for very large numbers will always be
            // larger than the threshold


/* Fixed version, make good enough threshold proportional to x */
def isGoodEnoughFixed(guess: Double, x: Double) =
  abs(guess * guess - x) < x * 0.00001

def sqrtIterFixed(guess: Double, x: Double): Double =
  if (isGoodEnoughFixed(guess, x)) guess
  else sqrtIterFixed(improve(guess, x), x)

def sqrtFixed(x: Double) =
  sqrtIterFixed(1.0, x)

sqrtFixed(2)
sqrtFixed(4)
sqrtFixed(1e-6)
sqrtFixed(1e60)

/* clean up with function nesting */

def sqrt(x: Double) = {
  def abs(x: Double) = if (x < 0) -x else x
  def isGoodEnough(guess: Double, x: Double) =
    abs(guess * guess - x) / x < 0.0001
  def improve(guess: Double, x: Double) =
    (guess + x / guess) / 2
  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)

  sqrtIter(1.0, x)
}

sqrt(2)
sqrt(4)
sqrt(1e-6)
sqrt(1e60)