package recfun
import common._
import annotation.tailrec

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

  def pascal(c: Int, r: Int): Int = {

    if (c < 0 ) throw new IllegalArgumentException

    if (r < 0) throw new IllegalArgumentException

    if (c > r) throw new IllegalArgumentException

    if (c == 0 || r == 0 || r == c) 1 else (pascal(c-1 , r -1) + pascal(c, r -1))
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def b(chars: List[Char], count: Int): Int =
      if(chars.isEmpty || count < 0) count else if (chars.head == '(') b(chars.tail, count + 1) else if (chars.head == ')') b(chars.tail, count -1) else b(chars.tail, count)

    b(chars, 0) == 0;
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def countChangeInner(money: Int, coins: List[Int]): Int = {
      if(money == 0) 1
      else if (money < 0) 0
      else if (coins.isEmpty) 0
      else {
        countChangeInner(money - coins.head, coins) +
          countChangeInner(money, coins.tail)
      }
    }

    if(money == 0) 0 else countChangeInner(money, coins);
  }
}
