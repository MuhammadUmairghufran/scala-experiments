val x = List(1)
val y = 0 +: x
val z = x :+ 2

//// Just some other test code before

val newRow = List(1, 7, 21, 35, 35, 21, 7, 1)
(0 +: newRow, newRow :+ 0).zipped.map(_+_)


//[1, 7, 21, 35, 35, 21, 7, 1]
//[1, 8, 28, 56, 70, 56, 28, 8, 1]


def pascalIter(countdown: Int, row: List[Int]): List[Int] = {
  val newRow = (0 +: row, row :+ 0).zipped.map(_+_)
  if (countdown > 1) pascalIter(countdown - 1, newRow) else newRow
}

pascalIter(8, List(1))

