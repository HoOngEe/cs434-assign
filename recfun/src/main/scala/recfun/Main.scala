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
  // Invariant: c, r >= 0
    if (c == 0) 1
    else if (c > r) 0
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance_aux(chars: List[Char], left_depth: Int): Boolean =
      if (left_depth < 0) false
      else if (chars.isEmpty) left_depth == 0
      else if (chars.head == '(') balance_aux(chars.tail, left_depth + 1)
      else if (chars.head == ')') balance_aux(chars.tail, left_depth - 1)
      else balance_aux(chars.tail, left_depth)

    balance_aux(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
  // Invariant: money >= 0
    if (money == 0) 1
    else {
      if (coins.isEmpty) 0
      else if (coins.head > money) countChange(money, coins.tail)
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
}
