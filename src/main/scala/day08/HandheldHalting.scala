package day08

import utils.InputLoader


object Instruction {
  val reNop = raw"nop (.*)".r
  val reAcc = raw"acc (.*)".r
  val reJmp = raw"jmp (.*)".r

  def walkAndCrash(instructions: IndexedSeq[String]): Int = {
    def handler(i: Int, acc: Int, visited: Set[Int]): Int = {
      if (visited.contains(i)) {
        return acc
      }

      instructions(i) match {
        case reNop(_) => handler(i + 1, acc, visited + i)
        case reAcc(v) => handler(i + 1, acc + v.toInt, visited + i)
        case reJmp(v) => handler(i + v.toInt, acc, visited + i)
      }
    }

    handler(0, 0, Set())
  }

  def walkAndRepair(instructions: IndexedSeq[String]): Int = {
    def handler(i: Int, acc: Int, visited: Set[Int], hasAlteredOnce: Boolean): Option[Int] = {
      if (i == instructions.size) {
        return Some(acc)
      }
      if (visited.contains(i)) {
        return None
      }

      instructions(i) match {
        case reAcc(v) => handler(i + 1, acc + v.toInt, visited + i, hasAlteredOnce)
        case reNop(v) => handler(i + 1, acc, visited + i, hasAlteredOnce).orElse(if (hasAlteredOnce) None else handler(i + v.toInt, acc, visited + i, true))
        case reJmp(v) => handler(i + v.toInt, acc, visited + i, hasAlteredOnce).orElse(if (hasAlteredOnce) None else handler(i + 1, acc, visited + i, true))
      }
    }

    handler(0, 0, Set(), false).getOrElse(throw new UnsupportedOperationException("could not solve"))
  }
}

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object HandheldHalting {
  def part1(instructions: IndexedSeq[String]) = {
    val n = Instruction.walkAndCrash(instructions)
    println(n)
  }
  def part2(instructions: IndexedSeq[String]) = {
    val n = Instruction.walkAndRepair(instructions)
    println(n)
  }

  def main(args: Array[String]) = {
    val input = InputLoader.read("08", false).toIndexedSeq

    part1(input)
    part2(input)
  }
}
