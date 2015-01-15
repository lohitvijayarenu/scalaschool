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
    val res = countChange(10, List(4, 2))
    println("Value of countChange"  + res)
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) 1
    else if (c == r) 1
    else pascal(c-1, r -1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def check(count: Int, remaining: List[Char]): Int = {
      if (count < 0) -1000
      else if (remaining.length == 0) count
      else {
        if (remaining.head == '(')
          check(count + 1, remaining.tail)
        else if (remaining.head == ')')
          check(count - 1, remaining.tail)
        else
          check(count, remaining.tail)
      }
    }
    if(check(0, chars) == 0) true else false
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def degen(capacity: Int, changes: List[Int]): Int = {
      if(capacity == 0) 
    	  1
      else if(capacity < 0) 
    	  0
      else if(changes.isEmpty && capacity>=1 )
    	  0
      else
    	  degen(capacity, changes.tail) + 
    	  	degen(capacity - changes.head, changes)
    }

    degen(money, coins.sortWith(_.compareTo(_) < 0))
  }
  
}
