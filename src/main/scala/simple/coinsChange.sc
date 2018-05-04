def countChange(money: Int, coins: List[Int]): Int = {
  if (money < 0 || coins.isEmpty) 0
  else if (money == 0) 1
  else countChange(money, coins.tail) + countChange(money - coins.head, coins)
}

countChange(0, List())
countChange(0, List(1, 2, 5, 10, 25, 50))
countChange(3, List(1, 2, 5, 10, 25, 50))
countChange(27, List(1, 2, 5, 10, 25, 50))
countChange(100, List(1, 2, 5, 10, 25, 50))
countChange(1000, List(1, 3, 7))