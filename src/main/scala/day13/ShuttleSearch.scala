package day13

import utils.{AOCExecutor, InputLoader}
import utils.Profiler.printTime

import scala.annotation.tailrec

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object ShuttleSearch extends AOCExecutor {
  val input = InputLoader.read("13", false)
  val earliest = input.head.toInt
  val buses = input(1).split(",").zipWithIndex.filter(_._1 != "x").map { p => (p._1.toInt, p._2) }


  def part1() = {
    val n = buses.map {
      _._1
    }.map { b =>
      (b, b - earliest % b)
    }.minBy {
      _._2
    }
    println(n)
    println(n._1 * n._2)
  }

  case class ChineseTarget(val a: Int, val n: Int)

  def chineseSolver(targets: List[ChineseTarget]): Long = {
    @tailrec
    def handler(inc: Long, acc: Long, current: ChineseTarget, remain: List[ChineseTarget]): Long = {
      if (acc % current.n == current.a) {
        remain match {
          case Nil => acc
          case ct :: ctx => handler(inc * current.n, acc, ct, ctx)
        }
      } else {
        handler(inc, acc + inc, current, remain)
      }
    }

    handler(targets.head.n, targets.head.a, targets(1), targets.drop(2))
  }

  def part2() = {
    val chinesTargets = buses.toList.map { b => ChineseTarget(b._1 - b._2 % b._1, b._1) }
    val n = chineseSolver(chinesTargets)
    println(n)
  }

  def main(args: Array[String]) = {
    execute
  }
}
