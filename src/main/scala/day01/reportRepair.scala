package day01

import utils.{InputLoader, ListUtils}


/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object reportRepair {
  def part1(numbers: List[Int]) = {
    val pair = numbers
      .flatMap(a => numbers.filter(b => b != a).map(b => (a, b)))
      .filter(p => p._1 + p._2 == 2020)
      .map(p => p._1 * p._2)
    println(pair)
  }

  def part2(numbers: List[Int]) = {
    val p = ListUtils.combinationWithoutRepetitions(numbers, 3)
      .filter(xs => xs.sum == 2020)
      .map(xs => xs.fold(1) { (acc, x) => acc * x })
    println(p)
  }


  def main(args: Array[String]) = {
    val input = InputLoader.read("01", false).map(x => x.toInt)
    part2(input)
  }
}
