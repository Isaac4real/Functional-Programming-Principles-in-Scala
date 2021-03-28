package recfun

import java.util.Dictionary
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
    if ((c == 0) || (c == r)) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def helper(acc: List[Char], iter_cars: List[Char]): Boolean = {
      if (iter_cars.isEmpty) acc.isEmpty
      else if (iter_cars.head == '(') {
        helper(acc.appended('('), iter_cars.tail)
      }
      else if (iter_cars.head.equals(')') && acc.isEmpty) false
      else if (iter_cars.head.equals(')')) {
        helper(acc.dropRight(1), iter_cars.tail)
      }
      else helper(acc, iter_cars.tail)
    }

    helper(List(), chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money.equals(0)) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
