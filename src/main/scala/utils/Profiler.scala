package utils

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2022
 */
object Profiler {
  def time(block: => Unit): Duration = {
    val t0 = System.nanoTime()
    block
    val t1 = System.nanoTime()
    Duration(t1 - t0, TimeUnit.NANOSECONDS)
  }

  def printTime[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1000000 + "ms")
    result
  }
}
