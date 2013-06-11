def countChange(initialMoney: Int, initialCoins: List[Int]): Int = {

  def loop(money : Int, coins: List[Int]) : Int = {
    println(money, coins)
    if (money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty) 0
    else if (coins.head == money) 1
    else countChange(money - coins.head, coins) + countChange(money - coins.head, coins.tail) + countChange(money, coins.tail)
  }
  loop(initialMoney, initialCoins)
}
countChange(4, List(1, 2))





























countChange(4, List(1, 2))






























