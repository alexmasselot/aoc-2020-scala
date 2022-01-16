package day23

import utils.{AOCExecutor, InputLoader}


/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object CrabCups extends AOCExecutor {

  val numbers = InputLoader.read("23", false).head.toList.map {
    _.toString.toInt
  }

  def part1(): Unit = {
    val ring = Ring.build(numbers)
    val nPlay = 10
    (1 to nPlay).foldLeft(numbers.head) { case (at, _) =>
      val next = ring.play(at)
      next
    }

    println(ring.values(1).drop(1).mkString(""))

  }

  def part2(): Unit = {
    val nPlay = 10000000//10000000
    val max = 1000000

    val ring = Ring.build(numbers ++ ((numbers.max + 1) to max).toList)
    (1 to nPlay).foldLeft(numbers.head) { case (at, _) =>
       ring.play(at)
      //      val a = nextRing.nextValue(1)
      //      val b = nextRing.nextValue(a)
      //      println(a, b)

    }
    val a = ring.next(1)
    val b = ring.next(a)
    println(a.toLong *  b)
  }

  def main(args: Array[String]) = {
    execute
  }
}
