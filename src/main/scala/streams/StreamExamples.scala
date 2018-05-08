package streams

object StreamExamples {
  def main(args: Array[String]): Unit = {
    val stream = Stream(1, 2, 3, 4, 5, 6, 7)
    //println(stream.toList)
    val top5 = stream.take(5)
    println(top5.toList)
    //
    println("lalala")
    val drop4 = stream.drop(5)
    println(drop4.toList)
  }
}
