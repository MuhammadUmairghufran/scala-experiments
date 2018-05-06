package montecarlo

import org.scalameter._
import parallel._
import scala.util.Random

object MonteCarloIntegration {

  private def square(f: Double => Double, xMin: Double, xMax: Double, yMin: Double, yMax: Double): Double =
    (yMax - yMin) * (xMax - xMin)

  private def countPointsUnderCurve(f: Double => Double, xMin: Double, xMax: Double, yMin: Double, yMax: Double,
                                    totalNumberOfPoints: Int): Int = {
    val rndX = new Random()
    val rndY = new Random()

    @annotation.tailrec
    def simulate(hits: Int, pointsGenerated: Int): Int = {
      if (pointsGenerated >= totalNumberOfPoints)
        hits
      else {
        val x = xMin + rndX.nextDouble() * (xMax - xMin)
        val y = yMin + rndY.nextDouble() * (yMax - yMin)

        simulate(hits + (if (y < f(x)) 1 else 0), pointsGenerated + 1)
      }
    }

    simulate(0, 0)
  }

  private def getBounds(f: Double => Double, xMin: Double, xMax: Double) = {
    if (xMin > xMax)
      throw new Exception(s"xMin $xMin is bigger than xMax $xMax")

    val yMin = 0 //Math.min(f(xMin), f(xMax))
    val yMax = Math.max(f(xMin), f(xMax))
    val area = square(f, xMin = xMin, xMax = xMax, yMin = yMin, yMax = yMax)
    (yMin, yMax, area)
  }

  private def getIntegral(area: Double, pointsInside: Int, pointsTotal: Int) = area * pointsInside / pointsTotal

  def integralSeq(f: Double => Double, xMin: Double, xMax: Double, totalNumberOfPoints: Int): Double = {
    val (yMin, yMax, area) = getBounds(f, xMin = xMin, xMax = xMax)
    val points = countPointsUnderCurve(f, xMin = xMin, xMax = xMax, yMin = yMin, yMax = yMax, totalNumberOfPoints)
    getIntegral(area, points, totalNumberOfPoints)
  }

  def integralParallelSimple(f: Double => Double, xMin: Double, xMax: Double, totalNumberOfPoints: Int): Double = {
    val (yMin, yMax, area) = getBounds(f, xMin = xMin, xMax = xMax)
    val (p1, p2) = parallel(
      countPointsUnderCurve(f, xMin, xMax, yMin, yMax, totalNumberOfPoints / 2),
      countPointsUnderCurve(f, xMin, xMax, yMin, yMax, totalNumberOfPoints / 2)
    )
    getIntegral(area, p1 + p2, totalNumberOfPoints)
  }

  def integralParallelRecursion(f: Double => Double, xMin: Double, xMax: Double, totalNumberOfPoints: Int): Double = {
    val (yMin, yMax, area) = getBounds(f, xMin = xMin, xMax = xMax)
    val recursionThreshold = 1000000 / 64

    def runParallelRecursion(totalNumberOfPoints: Int): Int = {
      if (totalNumberOfPoints <= recursionThreshold)
        countPointsUnderCurve(f, xMin, xMax, yMin, yMax, totalNumberOfPoints)
      else {
        val (pi1, pi2) = parallel(runParallelRecursion(totalNumberOfPoints / 2), runParallelRecursion(totalNumberOfPoints / 2))
        pi1 + pi2
      }
    }

    val points = runParallelRecursion(totalNumberOfPoints)
    getIntegral(area, points, totalNumberOfPoints)
  }

  def integralParallelList(f: Double => Double, xMin: Double, xMax: Double, totalNumberOfPoints: Int): Double = {
    val count = Runtime.getRuntime.availableProcessors()
    val (yMin, yMax, area) = getBounds(f, xMin = xMin, xMax = xMax)

    def parallelTask = () => countPointsUnderCurve(f, xMin, xMax, yMin, yMax, totalNumberOfPoints / count)

    val results = List.fill(count)(parallelTask).par.map(x => x())
    getIntegral(area, results.sum, totalNumberOfPoints)
  }

  def integralParallelTaskList(f: Double => Double, xMin: Double, xMax: Double, totalNumberOfPoints: Int): Double = {
    val count = Runtime.getRuntime.availableProcessors()
    val (yMin, yMax, area) = getBounds(f, xMin = xMin, xMax = xMax)

    def parallelTask = countPointsUnderCurve(f, xMin, xMax, yMin, yMax, totalNumberOfPoints / count)

    val taskList = List.fill(count - 1)(task(parallelTask))
    val mainTask = parallelTask
    val points = taskList.map(t => t.join()).sum + mainTask
    getIntegral(area, points, totalNumberOfPoints)
  }

  def main(args: Array[String]): Unit = {
    val totalNumberOfPoints = 10000000
    val testPoints = 10000000
    val xMin = -1
    val xMax = 2

    def f: Double => Double = x => 2 * x * x + 4 * x * x * x

    def f1: Double => Double = x => 3 * x * x

    def f2: Double => Double = x => x * x

    def f3: Double => Double = x => x * Math.sin(x * x)

    println(s"Integrate[1, 2]  (3x^2) sequential:       ${integralSeq(f1, xMin = 1, xMax = 2, testPoints)}")
    println(s"Integrate[1, 2]  (x^2) sequential:        ${integralSeq(f2, xMin = 1, xMax = 2, testPoints)}")
    println(s"Integrate[1, 2]  (x*sin(x^2)) sequential: ${integralSeq(f3, xMin = -1, xMax = 2, testPoints)}")
    println("***")
    println(s"Integrate[-1, 2] (2x^2+4x^3) sequential:   ${integralSeq(f, xMin = -1, xMax = 2, testPoints)}")
    println(s"Integrate[-1, 2] (2x^2+4x^3) parallel Sim: ${integralParallelSimple(f, xMin = -1, xMax = 2, testPoints)}")
    println(s"Integrate[-1, 2] (2x^2+4x^3) parallel Rec: ${integralParallelSimple(f, xMin = -1, xMax = 2, testPoints)}")
    println(s"Integrate[-1, 2] (2x^2+4x^3) parallel lst: ${integralParallelList(f, xMin = -1, xMax = 2, testPoints)}")
    println(s"Integrate[-1, 2] (2x^2+4x^3) parallel tl:  ${integralParallelTaskList(f, xMin = -1, xMax = 2, testPoints)}")
    println("*** Speed Measures ***")

    val standardConfig = config(
      Key.exec.minWarmupRuns -> 5,
      Key.exec.maxWarmupRuns -> 30,
      Key.exec.benchRuns -> 30,
      Key.verbose -> true
    ).withWarmer(new Warmer.Default)

    val seqTime = standardConfig.measure(
      integralSeq(f, xMin = xMin, xMax = xMax, totalNumberOfPoints)
    )

    val parSimpleTime = standardConfig.measure(
      integralParallelSimple(f, xMin = xMin, xMax = xMax, totalNumberOfPoints)
    )

    val parRecursionTime = standardConfig.measure(
      integralParallelRecursion(f, xMin = xMin, xMax = xMax, totalNumberOfPoints)
    )

    val parListTime = standardConfig.measure(
      integralParallelList(f, xMin = xMin, xMax = xMax, totalNumberOfPoints)
    )

    val parTaskListTime = standardConfig.measure(
      integralParallelTaskList(f, xMin = xMin, xMax = xMax, totalNumberOfPoints)
    )

    println(s"sequential time:         $seqTime")
    println(s"parallel simple time:    $parSimpleTime")
    println(s"parallel recursion time: $parRecursionTime")
    println(s"parallel list time:      $parListTime")
    println(s"parallel task list time: $parTaskListTime")
    println(s"speedup simple:     ${seqTime.value / parSimpleTime.value}")
    println(s"speedup recursion:  ${seqTime.value / parRecursionTime.value}")
    println(s"speedup par list:   ${seqTime.value / parListTime.value}")
    println(s"speedup task list:  ${seqTime.value / parTaskListTime.value}")
  }
}