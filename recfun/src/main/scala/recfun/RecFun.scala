package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (r < 2 || c <= 0 || c >= r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def balanceIter(chars: List[Char], unclosedCount: Int): Boolean ={
      if (chars.isEmpty) unclosedCount == 0
      else if (chars.head == '(') balanceIter(chars.tail, unclosedCount + 1)
      else if (chars.head == ')') {
        if (unclosedCount > 0) balanceIter(chars.tail, unclosedCount - 1)
        else false
      }
      else balanceIter(chars.tail, unclosedCount)
    }
    balanceIter(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
