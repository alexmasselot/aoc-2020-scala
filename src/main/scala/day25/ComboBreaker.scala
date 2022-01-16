package day25

import utils.{AOCExecutor, InputLoader}

import scala.annotation.tailrec


/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object ComboBreaker extends AOCExecutor {

  val input = InputLoader.read("25", false)
  val doorPublicKey = input.head.toLong
  val cardPublicKey = input(1).toLong


  def loopSize(start: Long, publicKey: Long): Int = {
    @tailrec
    def handler(value: Long, acc: Int): Int = {
      if (value == publicKey) {
        return acc
      }
      return handler((value * 7) % 20201227, acc + 1)
    }

    handler(1, 0)
  }

  def part1(): Unit = {
    val doorLoopSize = loopSize(1L, doorPublicKey)
    val cardLoopSize = loopSize(1L, cardPublicKey)
    println(cardLoopSize, doorLoopSize)
    val n = (1 to doorLoopSize).foldLeft(1L) { case (value, _) =>
      (value * cardPublicKey) % 20201227L
    }
    val m = (1 to cardLoopSize).foldLeft(1L) { case (value, _) =>
      (value * doorPublicKey) % 20201227L
    }
    println(n, m)
  }

  def part2(): Unit = {

  }

  def main(args: Array[String]) = {
    execute
  }
}
