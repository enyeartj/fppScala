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

def square(x: Int) = x * x
def cube(x: Int) = x * x * x
def id(x: Int) = x
def fact(x: Int): Int =
  if (x == 0) 1 else x * fact(x - 1)

def sum(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 0 else f(a) + sum(f)(a + 1, b)

sum(id)(0, 5)
sum(x => x)(0, 5)
def sumInts = sum(id)(_,_)
sumInts(0, 5)
sum(cube)(0, 3)

def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1 else f(a) * product(f)(a + 1, b)

product(id)(1, 3)
product(x => x)(1, 3)
product(id)(0, 3)
product(id)(-1, 2)
product(square)(1, 3)  // 1*1*2*2*3*3 = 36
product(square)(2, 3)  // 36
product(cube)(2, 5)    // 1,728,000

// Write factorial in terms of product fn
def factorial(n: Int) =
  product(x => x)(1, n)

factorial(3)
factorial(4)
factorial(0)