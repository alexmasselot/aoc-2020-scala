package day03

import utils.{InputLoader, Matrix}

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object TobogganTrajectory {
  def parse(lines: List[String]): Matrix[Boolean] = {
    Matrix.build[Boolean](lines.map { line => line.split("").filter(c => c != "").map(c => c == "#") })

  }

  def part1(forest: Matrix[Boolean]) = {
    val n = (0 until forest.nRows).foldLeft(0) { (acc, i) => if (forest(i, (3 * i) % forest.nCols)) acc + 1 else acc }
    println(n)
  }

  def part2(forest: Matrix[Boolean]) = {
    val steps = List(
      (1, 1),
      (3, 1),
      (5, 1),
      (7, 1),
      (1, 2),
    )
    val n = {
      steps.map { (step) =>
        (0 until forest.nRows / step._2).foldLeft(0) { (acc, i) => if (forest(i * step._2, (step._1 * i) % forest.nCols)) acc + 1 else acc }
      }
    }.foldLeft(1L) { (acc, x) => x * acc }
    println(n)
  }

  def main(args: Array[String]) = {
    val input = InputLoader.read("03", false)
    part1(parse(input))
    part2(parse(input))
  }
}
