
def coinAddsToAmount(coin: Int, amount: Int): Boolean = {
  if (coin <= 0)
    throw new IllegalArgumentException(s"Coin $coin cannot be less than 1")
  if (coin > amount)
    false
  else if ((amount - coin) == 0)
    true
  else
    coinAddsToAmount(coin, amount - coin)
}

def countCombos(amount: Int, coins: List[Int]): Int = {
  if (amount <= 0)
    0
  else if (coins.size == 1)
    if (coinAddsToAmount(coins.head, amount))
      1
    else 0
  else
    countAllCombosWithCoin(amount, coins.head, coins.tail) + countCombos(amount, coins.tail) //Without

  def countAllCombosWithCoin(amount: Int, withCoin: Int, remainingCoins: List[Int]): Int = {
    if (amount < withCoin)
      0
    else if (amount == withCoin)
      1
    else
      countCombos(amount, remainingCoins)
    + countAllCombosWithCoin(amount - withCoin, withCoin, remainingCoins)
  }
}


