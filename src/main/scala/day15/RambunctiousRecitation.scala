package day15

import utils.{AOCExecutor, InputLoader}

case class Spoken(
                   turn: Int,
                   numbersWhen: Map[Int, Int],
                   last: Int
                 ) {
  def next(): Spoken = numbersWhen.get(last) match {
    case None => Spoken(turn + 1, numbersWhen + (last -> (turn - 1)), 0)
    case Some(i) => Spoken(turn + 1, numbersWhen + (last -> (turn - 1)), turn - i - 1)
  }

  def next(targetTurn: Int): Spoken = {
    (turn to targetTurn).foldLeft(this) { case (sp, _) => sp.next() }
  }
}

object Spoken {
  def parse(str: String): Spoken = {
    val numbers = str.split(",").map {
      _.toInt
    }
    Spoken(
      numbers.size + 1,
      numbers.dropRight(1).zipWithIndex.map { case (n, i) => (n, i + 1) }.toMap,
      numbers.last
    )
  }
}

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object RambunctiousRecitation extends AOCExecutor {

  val input = InputLoader.read("15", false).head

  val starting = Spoken.parse(input)

  def part1(): Unit = {
    val sp = Spoken.parse(input).next(2020)
    println(sp.last)
  }

  def part2(): Unit = {
    val sp = Spoken.parse(input).next(30000000)
    println(sp.last)
  }

  def main(args: Array[String]) = {
    execute
  }
}
