def pascal(c: Int, r: Int): Int = {
  if (c < 0 || c > r)
    return 0
  if (c == 0 || c == r)
    return 1
  if (c == 1 || c == r - 1)
    return r

  @annotation.tailrec
  def pascalIteration(countdown: Int, row: List[Int]): List[Int] = {
    val newRow = (0 +: row, row :+ 0).zipped.map(_+_)
    if (countdown > 1) pascalIteration(countdown - 1, newRow) else newRow
  }

  pascalIteration(r, List(1))(c)
}

//10: [1, 10, 45, 120, 210, 252, 210, 120, 45, 10, 1]

pascal(-1, -1)
pascal(-1, 1)
pascal(0, 0)
pascal(0, 10)
pascal(1, 10)
pascal(9, 10)
pascal(10, 10)
pascal(8, 10)
pascal(5, 10)
pascal(3, 7)
pascal(15, 30)
pascal(69, 70)