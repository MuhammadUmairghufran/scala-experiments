package montecarlo

import org.scalameter._
import parallel._
import scala.util.Random


object MonteCarloPiEstimation {

  def pi(totalNumberOfPoints: Int): Double = 4.0 * countPointsInsideCircle(totalNumberOfPoints) / totalNumberOfPoints

  def countPointsInsideCircle(totalNumberOfPoints: Int): Int = {
    val rndX = new Random()
    val rndY = new Random()

    def simulate(hits: Int, pointsGenerated: Int): Int = {
      if (pointsGenerated >= totalNumberOfPoints)
        hits
      else {
        val x = rndX.nextDouble()
        val y = rndY.nextDouble()

        simulate(hits + (if (x * x + y * y <= 1) 1 else 0), pointsGenerated + 1)
      }
    }

    simulate(0, 0)
  }

  def piParallel(totalNumberOfPoints: Int): Double = {
    val (pi1, pi2) = parallel(countPointsInsideCircle(totalNumberOfPoints / 2), countPointsInsideCircle(totalNumberOfPoints / 2))
    4.0 * (pi1 + pi2) / totalNumberOfPoints
  }

  def piParallelX(totalNumberOfPoints: Int): Double = {
    val count = Runtime.getRuntime.availableProcessors()

    def parallelTask = () => countPointsInsideCircle(totalNumberOfPoints / count)

    val results = List.fill(count)(parallelTask).par.map(x => x())
    4.0 * results.sum / totalNumberOfPoints
  }

  def main(args: Array[String]): Unit = {
    val totalNumberOfPoints = 1000000

    val standardConfig = config(
      Key.exec.minWarmupRuns -> 50,
      Key.exec.maxWarmupRuns -> 300,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true
    ).withWarmer(new Warmer.Default)

    val seqtime = standardConfig.measure(
      pi(totalNumberOfPoints)
    )

    val partime = standardConfig.measure(
      piParallel(totalNumberOfPoints)
    )

    val parXtime = standardConfig.measure(
      piParallelX(totalNumberOfPoints)
    )

    println(s"pi sequential:   ${pi(totalNumberOfPoints)}")
    println(s"pi parallel:     ${piParallel(totalNumberOfPoints)}")
    println(s"pi parallel X:   ${piParallelX(totalNumberOfPoints)}")
    println(s"sequential time: $seqtime")
    println(s"parallel time:   $partime")
    println(s"parallel time:   $parXtime")
    println(s"speedup:   ${seqtime.value / partime.value}")
    println(s"speedup X: ${seqtime.value / parXtime.value}")
  }
}