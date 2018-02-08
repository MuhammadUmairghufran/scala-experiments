def pascal(c: Int, r: Int): Int = {
  if (c < 0 || c > r)
    throw new Exception("Incorrect column or row value")

  @annotation.tailrec
  def pascalIteration(countdown: Int, row: List[Int]): List[Int] = {
    val newRow = (0 +: row, row :+ 0).zipped.map(_+_)
    if (countdown > 1) pascalIteration(countdown - 1, newRow) else newRow
  }

  if (c == 0 || c == r) 1
  else if (c == 1 || c == r - 1) r
  else pascalIteration(r, List(1))(c)
}

//7: [1, 7, 21, 35, 35, 21, 7, 1]
//8: [1, 8, 28, 56, 70, 56, 28, 8, 1]
//10: [1, 10, 45, 120, 210, 252, 210, 120, 45, 10, 1]

pascal(0, 0)
pascal(0, 1)
pascal(0, 10)
pascal(10, 10)
pascal(1, 10)
pascal(9, 10)
pascal(8, 10)
pascal(5, 10)
pascal(3, 7)
pascal(15, 30)
pascal(69, 70)