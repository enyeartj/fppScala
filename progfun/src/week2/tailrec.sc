import scala.annotation.tailrec

1+1
def factorial(n: Int) = {
  @tailrec
  def factorialIter(n: Int, prod: Int): Int =
    if (n == 0) prod else factorialIter(n - 1, n * prod)
  factorialIter(n, 1)
}

factorial(0)
factorial(1)
factorial(2)
factorial(3)
factorial(4)
factorial(5)
factorial(6)

def add(n: Int): Int =
  if (n == 0) 0 else n + add(n-1)

add(5)
add(100)
add(500)
add(1000)
add(5000)
add(10000)
//add(50000)  // stack overflow!
//add(100000) // stack overflow!

def addTail(n: Int): Int = {
  @tailrec
  def loop(acc: Int, n: Int): Int =
    if (n == 0) acc else loop(acc + n, n - 1)
  loop(0, n)
}

addTail(5)
addTail(100)
addTail((500))
addTail(1000)
addTail(5000)
addTail(100000)
addTail(500000)
addTail(1000000)
addTail(5000000)  // integer overflow
addTail(10000000) // integer overflow