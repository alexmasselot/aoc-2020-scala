package day10

import utils.{AOCExecutor, InputLoader}
import utils.Profiler.printTime


/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object AdapterArray extends AOCExecutor {
  val jolts = InputLoader.read("10", false).map {
    _.toInt
  }

  def part1() = {
    val sortedJolts = List(0) ::: jolts.sorted
    val diff = sortedJolts.zip(sortedJolts.drop(1) :+ (sortedJolts.last + 3))
      .map { case (j, next) =>
        next - j
      }
      .foldLeft(Map[Int, Int]()) { case (acc, delta) => acc + (delta -> (acc.getOrElse(delta, 0) + 1)) }
    println(diff(3) * diff(1))
  }

  def part2() = {
    val sortedJolts = List(0) ::: jolts.sorted

    def handler(remain: List[Int], count: Map[Int, Long]): Long = {
      remain match {
        case Nil => count(sortedJolts.last)
        case x :: xs =>
          handler(xs,
            xs.takeWhile {
              _ <= x + 3
            }.foldLeft(count) { case (acc, y) => acc + (y -> (acc(x) + acc.getOrElse(y, 0L))) }
          )
      }
    }

    val n = handler(sortedJolts, Map(0 -> 1L))
    println(n)
  }


  def main(args: Array[String]) = {
    execute
  }
}
