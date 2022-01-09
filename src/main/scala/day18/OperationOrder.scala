package day18

import utils.{AOCExecutor, InputLoader}


/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object OperationOrder extends AOCExecutor {

  val input = InputLoader.read("18", false)

  val reSimpleAdd = raw"(.*)\s*\b(\d+)\s+\+\s+(\d+)(.*)".r
  val reSimpleMult = raw"\s*(\d+)\s+\*\s+(\d+)(.*)".r
  val reNumber = raw"\s*(\d+)\s*".r
  val reParenthesis = raw"(.*)\(([^\)\(]+)\)(.*)".r

  def computeNoParenthesis(line: String): Long = {
    println(s"computeNoParenthesis: $line")
    line match {
      case reSimpleAdd(pref, a, b, rem) => computeNoParenthesis(s"$pref ${a.toLong + b.toLong}$rem")
      case reSimpleMult(a, b, rem) => computeNoParenthesis(s"${a.toLong * b.toLong}$rem")
      case reNumber(n) => n.toLong
    }
  }

  def compute(line: String): Long = {
    println(line)
    line match {
      case reParenthesis(pref, target, suff) => compute(s"$pref ${computeNoParenthesis(target)} $suff")
      case x => computeNoParenthesis(x)
    }
  }

  def part1(): Unit = {
      val n = input.map{compute(_)}.sum
    println(n)
  }

  def part2(): Unit = {

  }

  def main(args: Array[String]) = {
    execute
  }
}
