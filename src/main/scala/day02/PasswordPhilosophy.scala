package day02

import utils.InputLoader


object PasswordPhilosophy {
  val reParse = raw"(\d+)\-(\d+) (\w): (\w+)".r

  def part1(pcs: List[PasswordCheck]) = {
    val n = pcs.count(pc => pc.isValid1())
    println(n)
  }

  def part2(pcs: List[PasswordCheck]) = {
    val n = pcs.count(pc => pc.isValid2())
    println(n)
  }

  def main(args: Array[String]) = {
    val input = InputLoader.read("02", false).map(line => line match {
      case reParse(from, to, char, password) => PasswordCheck(from.toInt to to.toInt, char.charAt(0), password)
      case _ => ???
    })
    part1(input)
    part2(input)
  }
}
