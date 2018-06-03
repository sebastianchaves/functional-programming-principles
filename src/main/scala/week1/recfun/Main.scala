package week1.recfun

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
  def pascal(column: Int, row: Int): Int =
    if (column == 0 || column == row) 1
    else pascal(column - 1, row - 1) + pascal(column, row - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def isBalanced(chars: List[Char], count: Int): Boolean = {
      if (chars.isEmpty) count == 0
      else if ('('.equals(chars.head)) isBalanced(chars.tail, count + 1)
      else if (')'.equals(chars.head)) isBalanced(chars.tail, count - 1)
      else isBalanced(chars.tail, count)
    }

    // )())(()(

    if (chars.isEmpty) true
    else if (chars.head == chars.last) false
    else isBalanced(chars, 0)

  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def count(money: Int, coins: List[Int]): Int = {

      if (money < 0 || coins.isEmpty) 0
      else if (money == 0) 1
      else count(money, coins.tail) + count(money - coins.head, coins)

    }

    count(money, coins)

  }

}


