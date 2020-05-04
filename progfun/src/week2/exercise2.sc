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
def sumInts = sum(id) _
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

// Write a function that generalizes sum and product
def operate(g: (Int, Int) => Int, f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 0 - g(a - b, b - a) else g(f(a), operate(g, f)(a + 1, b))

operate((x, y) => x + y, id)(1, 3)
operate((x, y) => x * y, id)(1, 3)
def add(x: Int, y: Int) = x + y
def mult(x: Int, y: Int) = x * y
operate(add, id)(1, 3)
operate(mult, id)(1, 3)
operate(add, id)(0, 5)
operate(add, square)(0, 3)
operate(add, cube)(0, 3)
operate(mult, id)(0, 3)
operate(mult, id)(-1, 2)
operate(mult, square)(1, 3)
operate(mult, square)(2, 3)
operate(mult, cube)(2, 5)

// Try again with currying chained all the way down
def op(g: (Int, Int) => Int)(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 0 - g(a - b, b - a)
  else g(f(a), op(g)(f)(a + 1, b))

op(add)(id)(1, 3)
op(mult)(id)(1, 3)
op(add)(id)(0, 5)
op(add)(square)(0, 3)
op(add)(cube)(0, 3)
op(mult)(id)(0, 3)
op(mult)(id)(-1, 2)
op(mult)(square)(1, 3)
op(mult)(square)(2, 3)
op(mult)(cube)(2, 5)

def genSum = op(add) _
def genProd = op(mult) _
def sInts = genSum(id)
def genFact(n: Int) = genProd(id)(1, n)
sInts(0, 5)
sInts(-2, 3)
genFact(3)
genFact(4)
genFact(0)

// Solution from the lecture: mapReduce... similar to my operate fn
def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

def prod(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)

prod(square)(1, 3)

def mrFact(n: Int) = prod(x => x)(1, n)

mrFact(5)