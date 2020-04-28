import scala.annotation.tailrec

def sum(f: Int => Int, a: Int, b: Int) = {
  @tailrec
  def loop(a: Int, acc: Int): Int =
    if (a > b) acc
    else loop(a + 1, acc + f(a))
  loop(a, 0)
}

sum(x => x, 0, 5) // 1 + 2 + 3 + 4 + 5 = 15
sum(x => x * x, 0, 3) // 1*1 + 2*2 + 3*3 = 14
sum(x => x * x * x, 0, 3) // 1^3 + 2^3 + 3^3 = 1+8+27 = 36