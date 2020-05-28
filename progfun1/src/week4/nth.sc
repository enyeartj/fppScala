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
  else if (n == 0) return list.head
  else return nth(n - 1, list.tail)
}

val list = new Cons(1, new Cons(2, new Cons(3, new Cons(4, new Nil))))

nth(0, list)
nth(3, list)
nth(8, list)