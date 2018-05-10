package closest_points_pair

import scala.util.Random

case class Point(x: Int, y: Int)

case class PointsPair(point1: Point, point2: Point) {
  def distance: Double = ClosestPoints.distance(point1, point2)
}

object ClosestPoints {
  def main(args: Array[String]): Unit = {
    val points = generate()
    println("Length: ", points.length)
    val bruteForcePair: PointsPair = bruteForceClosest(points)
    println("Brute Force closest: ", bruteForcePair)
  }

  def generate(length: Int = 10000, max: Int = 100000, seed: Int = 42): Vector[Point] = {
    val rnd = new Random(seed = seed)
    (0 until length).map(_ => Point(rnd.nextInt(max), rnd.nextInt(max))).toVector.distinct
  }

  def bruteForceClosest(points: Vector[Point]): PointsPair = {
    var minPair = PointsPair(points(0), points(1))

    val length = points.length
    for (i <- 0 until length - 1) {
      for (j <- i + 1 until length) {
        if (distance(points(i), points(j)) < minPair.distance) {
          minPair = PointsPair(points(i), points(j))
        }
      }
    }
    minPair
  }

//  def divideAndConquerClosest(points: Vector[Point]): PointsPair = {
//    val sorted = sortByY(points)
//    val length = sorted.length
//    //di
//  }

  def divideAndConquer(points: Vector[Point]) = {

  }

  def sortByY(points: Vector[Point]): Vector[Point] = points.sortBy(point => point.y)

  def distance(point1: Point, point2: Point): Double = {
    math.sqrt(scala.math.pow(point1.x - point2.x, 2) + scala.math.pow(point1.y - point2.y, 2))
  }
}
