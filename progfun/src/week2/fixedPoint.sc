import math.abs
import scala.annotation.tailrec

val tolerance = 0.0001
def isCloseEnough(x: Double, y: Double) =
  abs((x - y) / x) / x < tolerance
def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  @tailrec
  def iter(guess: Double): Double = {
    println("guess = " + guess)
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else (iter(next))
  }
  iter(firstGuess)
}

fixedPoint(x => 1 + x/2)(1)

def sqrt(x: Double) =
  fixedPoint(y => (y + x / y) / 2)(1.0)

sqrt(2)

def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

def sqrtDamped(x: Double) =
  fixedPoint(averageDamp(y => x / y))(1.0)

sqrtDamped(2)