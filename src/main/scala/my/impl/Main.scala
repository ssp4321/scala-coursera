package my.impl

/**
 * Created by Sandeep Patil on 08/05/2014.
 */
class Main {

  /**
   *
   * The following pattern of numbers is called Pascal’s triangle
   *     1
   *    1 1
   *   1 2 1
   *  1 3 3 1
   * 1 4 6 4 1
   * The numbers at the edge of the triangle are all 1, and each number inside the triangle is the sum of the two numbers above it. Write a function that computes the elements of Pascal’s triangle by means of a recursive process.
   * Do this exercise by implementing the pascal function in Main.scala, which takes a column c and a row r, counting from 0 and returns the number at that spot in the triangle. For example, pascal(0,2)=1, pascal(1,2)=2 and pascal(1,3)=3
   *
   * @param cIndx
   * @param rIndx
   * @return
   */
  def pascal(cIndx: Int, rIndx: Int): Int = {
    val maxColIndx = rIndx
    if (cIndx > maxColIndx)
      throw new IllegalArgumentException(s"cIndx $cIndx cannot be greater than $maxColIndx")
    else if (cIndx == 0 || cIndx == maxColIndx)
      1
    else
      pascal(cIndx - 1, rIndx - 1) + pascal(cIndx, rIndx - 1)
  }

  /**
   * Write a recursive function which verifies the balancing of parentheses in a string,
   * which we represent as a List[Char] not a String.
   *
   * For example, the function should return true for the following strings:
   * (if (zero? x) max (/ 1 x))
   * I told him (that it’s not (yet) done). (But he wasn’t listening)
   * The function should return false for the following strings:
   * :-)
   * ())(
   *
   * @param chars
   * @return
   */
  def balance(chars: List[Char]): Boolean = {
    def isBalanced(remainingChars: List[Char], balanceSoFar: Int): Boolean = {
      if (balanceSoFar < 0)
        false
      else if (remainingChars.isEmpty)
        balanceSoFar == 0
      else {
        if (remainingChars.head == '(')
          isBalanced(remainingChars.tail, balanceSoFar + 1)
        else if (remainingChars.head == ')')
          isBalanced(remainingChars.tail, balanceSoFar - 1)
        else
          isBalanced(remainingChars.tail, balanceSoFar)
      }
    }

    isBalanced(chars, 0)
  }


  /**
   * Write a recursive function that counts how many different ways you can make change for an money,
   * given a list of coin denominations.
   *
   * For example, there are 3 ways to give change for 4 if you have coins with denomination 1 and 2: 1+1+1+1, 1+1+2, 2+2.
   * Do this exercise by implementing the countChange function in Main.scala.
   * This function takes an money to change, and a list of unique denominations for the coins.
   * @param money
   * @param coins
   * @return
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty)
      0
    else
      countWithCoin(coins.head, money, coins.tail) + countChange(money, coins.tail)
  }


  def countWithCoin(coin: Int, amount: Int, remainingCoins: List[Int]): Int = {
    if (amount < coin)
      0
    else if (amount == coin)
      1
    else
      countWithCoin(coin, amount - coin, remainingCoins) + countChange(amount - coin, remainingCoins)
  }
}
