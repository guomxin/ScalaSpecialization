package recfun

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
      if ((c == 0) || (c == r)) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def detect(leftCount:Int, rightCount:Int, chars: List[Char]): Boolean =
        if (chars.isEmpty) leftCount == rightCount
        else if (chars.head == '(') detect(leftCount + 1, rightCount, chars.tail)
        else if (chars.head == ')') {
          if (leftCount <= rightCount) false
          else detect(leftCount, rightCount + 1, chars.tail)
        }
        else detect(leftCount, rightCount, chars.tail)
      
      detect(0, 0, chars)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = 
      if (money == 0) 1
      else if (money < 0) 0
      else if (coins.isEmpty) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail) 
    
  }
