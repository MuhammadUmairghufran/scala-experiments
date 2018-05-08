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


    val fr = bigStream.foldRight(0)((elem, acc) => elem + acc)
    println("fr cum sum", fr)


    println("all more than 15", bigStream.forAll(_ > 15))
    println("all less 4", bigStream.forAll(_ < 4))
    println("all less 20", bigStream.forAll(_ < 20))
    println("all eq 1", bigStream.forAll(_ == 1))

    println("from take", Stream.from(10).take(20).toList)

    //    val fibs = {
    //      def next(f0: Int, f1: Int): Stream[Int] = Stream.cons(f0, next(f1, f0 + f1))
    //
    //      next(0, 1)
    //    }
    // uncomment for real streams
    // println("fibs:", fibs.take(10).toList)

    println("map", bigStream.map(_ == 2).toList)
    println("map", bigStream.map(_ * 2).toList)

    println("filter", bigStream.filter(x => x >= 5 && x <= 10).toList)
    println("filter", bigStream.filter(x => x > 3 && x <= 20).toList)

    def isPrimeNumber(n: Int): Boolean = {
      !Stream.from(2, n-1).exists(n % _ == 0)
    }

    println(Stream.from(2, 11000).toList)

    println("prime  9n: ", isPrimeNumber(9))
    println("prime 10n: ", isPrimeNumber(10))
    println("prime 11n: ", isPrimeNumber(11))
    println("prime 12n: ", isPrimeNumber(12))
    println("prime 10,006: ", isPrimeNumber(10006))
    println("prime 10,007: ", isPrimeNumber(10007))
    println("prime 10,008: ", isPrimeNumber(10008))


  }
}
