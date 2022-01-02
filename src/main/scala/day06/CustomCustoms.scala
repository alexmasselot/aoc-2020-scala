package day06

import utils.InputLoader

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object CustomCustoms {
  def part1(input: List[String]) = {
    val n = input.map(block => {
      block.replaceAll("\n", "").split("").filter(!_.isEmpty).toSet.size
    }).sum
    println(n)
  }

  def part2(input: List[String]) = {
    val n = input.map(block => {
      val nPassengers = block.split("\n").size
      block.replaceAll("\n", "").split("").filter(!_.isEmpty)
        .foldLeft(Map[String, Int]()) { (acc, c) =>
          acc + (c -> (acc.getOrElse(c, 0) + 1))
        }
        .filter(v => v._2 == nPassengers)
        .size
    }).sum
    println(n)
  }

  def main(args: Array[String]) = {
    val input = InputLoader.read("06", false, "\n\n")
    part1(input)
    part2(input)
  }
}
