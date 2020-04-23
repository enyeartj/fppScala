1+3
def abs(x: Double) = if (x < 0) -x else x

/*
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
def sqrt(x: Double) =
  sqrtIter(1.0, x)

sqrt(2)
sqrt(4)
sqrt(1e-6)  // not precise because the good enough threshold is bigger than the number
sqrt(1e60)  // doesn't terminate because good enough threshold is always smaller than diff
*/

/* Fixed version, make good enough threshold proportional to x */
def isGoodEnough(guess: Double, x: Double) =
  abs(guess * guess - x) < x * 0.00001

def improve(guess: Double, x: Double) =
  (x / guess + guess) / 2

def sqrtIter(guess: Double, x: Double): Double =
  if (isGoodEnough(guess, x)) guess
  else sqrtIter(improve(guess, x), x)

def sqrt(x: Double) =
  sqrtIter(1.0, x)

sqrt(2)
sqrt(4)
sqrt(1e-6)
sqrt(1e60)