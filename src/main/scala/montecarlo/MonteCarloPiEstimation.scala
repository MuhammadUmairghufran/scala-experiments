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
    val count = Runtime.getRuntime.availableProcessors() - 1

    def parallelTask = () => countPointsInsideCircle(totalNumberOfPoints / count)

    val results = List.fill(count)(parallelTask).par.map(x => x())
    4.0 * results.sum / totalNumberOfPoints
  }

  def piParallel8(totalNumberOfPoints: Int): Double = {
    val count = Runtime.getRuntime.availableProcessors()

    def parallelTask = countPointsInsideCircle(totalNumberOfPoints / count)

    val taskList = List.fill(count - 1)(task(parallelTask))
    val mainTask = parallelTask
    4.0 * (taskList.map(t => t.join()).sum + mainTask) / totalNumberOfPoints
  }

  def main(args: Array[String]): Unit = {
    val totalNumberOfPoints = 1000000

    val standardConfig = config(
      Key.exec.minWarmupRuns -> 50,
      Key.exec.maxWarmupRuns -> 200,
      Key.exec.benchRuns -> 1000,
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

    val par8time = standardConfig.measure(
      piParallel8(totalNumberOfPoints)
    )

    println(s"pi sequential:   ${pi(totalNumberOfPoints)}")
    println(s"pi parallel:     ${piParallel(totalNumberOfPoints)}")
    println(s"pi parallel X:   ${piParallelX(totalNumberOfPoints)}")
    println(s"pi parallel 8:   ${piParallel8(totalNumberOfPoints)}")
    println(s"sequential time: $seqtime")
    println(s"parallel time:   $partime")
    println(s"parallel X time: $parXtime")
    println(s"parallel 8 time: $par8time")
    println(s"speedup:   ${seqtime.value / partime.value}")
    println(s"speedup X: ${seqtime.value / parXtime.value}")
    println(s"speedup 8: ${seqtime.value / par8time.value}")
  }
}