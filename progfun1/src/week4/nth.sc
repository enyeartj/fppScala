trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

def nth[T](n: Int, list: List[T]): T = {
  if (list.isEmpty) throw new IndexOutOfBoundsException()
  else if (n == 0) list.head
  else nth(n - 1, list.tail)
}

val list = new Cons(1, new Cons(2, new Cons(3, new Cons(4, new Nil))))

nth(0, list)
nth(3, list)
//nth(8, list) // IndexOutOfBoundsException

object List {
  def apply[T](): List[T] = new Nil
  def apply[T](x: T): List[T] = new Cons(x, new Nil)
  def apply[T](x: T, y: T): List[T] = new Cons(x, new Cons(y, new Nil))
}

val l0 = List()
val l1 = List(1)
val l2 = List(2, 3)

l0.isEmpty
l1.isEmpty

nth(0, l2)
nth(1, l2)

val l_type = List
val l_func = List()