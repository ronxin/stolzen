package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(chars: List[Char], opens: Int): Boolean =
      if (chars.isEmpty)
        opens == 0
      else if (chars.head == '(')
        balanceIter(chars.tail, opens + 1)
      else if (chars.head == ')') {
        if (opens > 0)
          balanceIter(chars.tail, opens - 1)
        else
          false
      } else 
        balanceIter(chars.tail, opens)

    balanceIter(chars, 0)
  }

  /**
   * Exercise 3
   *
   * Write a recursive function that counts how many different ways you can make change
   * for an amount, given a list of coin denominations. For example, there are 3 ways
   * to give change for 4 if you have coins with denomiation 1 and 2: 1+1+1+1, 1+1+2,
   * 2+2.
   *
   * [SICP 2.19]
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) +
      countChange(money, coins.tail)
  }
}
