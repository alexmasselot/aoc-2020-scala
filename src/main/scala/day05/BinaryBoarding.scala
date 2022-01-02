package day05

import utils.InputLoader

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */


object BinaryBoarding {
  def decode(code: String): Int = {
    val bCode = code.replaceAll("B", "1")
      .replaceAll("F", "0")
      .replaceAll("R", "1")
      .replaceAll("L", "0")

    val row = Integer.parseInt(bCode.take(7), 2)
    val col = Integer.parseInt(bCode.drop(7), 2)
    row * 8 + col
  }

  def part1(lines: List[String]) = {
    val n = lines
      .map { l => decode(l) }
      .max
    println(n)
  }

  def part2(lines: List[String]) = {
    val ids = lines
      .map { l => decode(l) }
      .sorted
    val n = ids.zip(ids.drop(1))
      .filter(a => a._2 - a._1 > 1)
    println(n)
  }


  def main(args: Array[String]) = {
    val input = InputLoader.read("05", false)

    part1(input)
    part2(input)
  }
}
