package recfun

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
    if (c < 0 || r < 0) {
      0
    } else if (c == 0) {
      1
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceHelper(chars: List[Char]): List[Char] = {
      if (chars.isEmpty) {
        chars
      } else if (chars.length == 1) {
        if (chars.head != '(' && chars.head != ')') {
          List()
        } else {
          chars
        }
      } else if ((chars.head == '(' && chars.last == ')') || (chars.head != '(' && chars.last != ')')) {
        balanceHelper(chars.slice(1, chars.length - 1))
      } else if (chars.head == '(') {
        balanceHelper(chars.slice(0, chars.length - 1))
      } else if (chars.last == ')') {
        balanceHelper(chars.tail)
      } else {
        chars
      }
    }

    balanceHelper(chars).isEmpty
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty || money == 0) {
      0
    } else {
      val next = coins.head
      if (next > money) {
        countChange(money, coins.tail)
      } else if (money - next == 0) {
        1 + countChange(money, coins.tail)
      } else {
        countChange(money, coins.tail) + countChange(money - next, coins)
      }
    }
  }
}
