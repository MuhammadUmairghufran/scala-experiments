package streams

object StreamExamples {
  def main(args: Array[String]): Unit = {
    val stream = Stream(1, 2, 3, 4, 5, 6, 7)
    println(stream.toList)
    val top5 = stream.take(5)
    println("top 5", top5.toList)
    val drop4 = stream.drop(4)
    println("drop 4", drop4.toList, "\n")


    val bigStream = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
    println(bigStream.take(12).drop(5).toList)
    println("has 1", bigStream.take(12).drop(5).exists(_ == 1))
    println("has 7", bigStream.take(12).drop(5).exists(_ == 7))
  }
}
