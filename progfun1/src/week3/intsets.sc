abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }
  def incl(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  }
  def union(other: IntSet) = ((left union right) union other) incl elem
  override def toString = "{" + left + elem + right + "}"
}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def union(other: IntSet) = other
  override def toString = "."
}

/* ------------------------------------------------------------------------------- */
val t1 = new NonEmpty(3, Empty, Empty)
val t2 = t1 incl 4 incl 5 incl 1 incl 7

val s1 = new NonEmpty(8, Empty, Empty)
val s2 = s1 incl 2 incl 4 incl 15 incl 10 incl 5

val x = t2 union s2

val X = new NonEmpty(2, Empty, Empty) incl 1 incl 3
val Y = new NonEmpty(5, Empty, Empty) incl 4 incl 6
val Z = X union Y