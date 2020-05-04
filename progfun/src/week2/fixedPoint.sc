import math.abs
import scala.annotation.tailrec

val tolerance = 0.0001
def isCloseEnough(x: Double, y: Double) =
  abs((x - y) / x) / x < tolerance
def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  @tailrec
  def iter(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else (iter(next))
  }
  iter(firstGuess)
}

fixedPoint(x => 1 + x/2)(1)