package closest_points_pair

import org.scalameter.{Key, Warmer, config}

import scala.util.Random

case class Point(x: Int, y: Int)

case class PointsPair(point1: Point, point2: Point) {
  def distance: Double = ClosestPoints.distance(point1, point2)
}

object ClosestPoints {
  private var threshold: Int = 0

  def main(args: Array[String]): Unit = {
    val points = generate(length = 20000)
    val cores = Runtime.getRuntime.availableProcessors()

    println("Brute Force closest:    ", bruteForceClosest(points).distance)
    println("D&C sequential closest: ", divideAndConquerClosest(points))
    println("D&C parallel closest:   ", divideAndConquerClosest(points, threads = cores))

    val bncTime = getMeasureConfig(1, 1).measure(bruteForceClosest(points))
    val dncTime = getMeasureConfig().measure(divideAndConquerClosest(points))
    val dnpTime = getMeasureConfig().measure(divideAndConquerClosest(points, threads = cores))

    println(s"brute force seq time:  $bncTime")
    println(s"divide&conq seq time:  $dncTime")
    println(s"divide&conq par time:  $dnpTime")
    println(s"just dnc speedup:      ${bncTime.value / dncTime.value}")
    println(s"dnc parallel speedup:  ${dncTime.value / dnpTime.value}")
  }

  private def setCoresThreshold(pointsLength: Int, cores: Int): Unit = threshold = pointsLength / cores

  private def getMeasureConfig(minWarmupRuns: Int = 30, benchRuns: Int = 100) = config(
    Key.exec.minWarmupRuns -> minWarmupRuns,
    Key.exec.maxWarmupRuns -> minWarmupRuns * 2,
    Key.exec.benchRuns -> benchRuns,
    Key.verbose -> true)
    .withWarmer(new Warmer.Default)

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

  def divideAndConquerClosest(points: Vector[Point], threads: Int = 1): Double = {
    setCoresThreshold(points.length, threads)
    val (_, distance) = closestPair(points)
    distance
  }

  def closestPair(points: Vector[Point]): (Vector[Point], Double) = {
    if (points.length <= 3)
      (points, bruteForceClosest(points).distance)
    else {
      val (left, right, splitPoint) = splitByX(points)
      val ((leftPoints, leftDistance), (rightPoints, rightDistance)) = parallelProcess(left, right)
      val pointsMerged = sortByY(leftPoints ++ rightPoints)
      val dist = boundaryMerge(pointsMerged, leftDistance, rightDistance, splitPoint)
      (pointsMerged, dist)
    }
  }

  private def parallelProcess(leftPoints: Vector[Point], rightPoints: Vector[Point]) = {
    if (leftPoints.length < threshold)
      (closestPair(leftPoints), closestPair(rightPoints))
    else
      parallel.parallel(closestPair(leftPoints), closestPair(rightPoints))
  }

  def splitByX(points: Vector[Point]): (Vector[Point], Vector[Point], Point) = {
    val sorted = sortByX(points)
    val lengthMedian = sorted.length / 2
    val splitPoint = sorted(lengthMedian)
    val (left, right) = sorted.splitAt(lengthMedian)
    (left, right, splitPoint)
  }

  def boundaryMerge(points: Vector[Point], leftDistance: Double, rightDistance: Double, middlePoint: Point): Double = {
    val lrMinDistance = Math.min(leftDistance, rightDistance)
    val xl = middlePoint.x - lrMinDistance
    val xr = middlePoint.x + lrMinDistance

    val minPoints = points.filter(p => p.x >= xl && p.x <= xr)
    val minPDistance = getMinPointsDistance(minPoints)
    scala.math.min(lrMinDistance, minPDistance)
  }

  def getMinPointsDistance(points: Vector[Point]): Double = {
    if (points.length < 2)
      Double.MaxValue
    else {
      var minDistance = Double.MaxValue
      for (i <- 0 until points.length - 1) {
        var j = i + 1
        while (j < i + 8 && j < points.length) {
          val stepDistance = distance(points(i), points(j))
          minDistance = if (stepDistance < minDistance) stepDistance else minDistance
          j += 1
        }
      }
      minDistance
    }
  }

  def sortByY(points: Vector[Point]): Vector[Point] = points.sortBy(point => point.y)

  def sortByX(points: Vector[Point]): Vector[Point] = points.sortBy(point => point.x)

  def distance(point1: Point, point2: Point): Double = {
    math.sqrt(scala.math.pow(point1.x - point2.x, 2) + scala.math.pow(point1.y - point2.y, 2))
  }
}
