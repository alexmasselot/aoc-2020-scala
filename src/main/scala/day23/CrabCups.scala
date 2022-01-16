package day23

import utils.{AOCExecutor, InputLoader}


/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object CrabCups extends AOCExecutor {

  val startRing = Ring(InputLoader.read("23", true).head.toList.map {
    _.toString.toInt
  })

  def part1(): Unit = {
    val nPlay = 100
    val (last, _) = (1 to nPlay).foldLeft((startRing, startRing.head)) { case ((r, at), _) =>
      val nextRing = r.play1(3, at)
      (nextRing, nextRing.nextValue(at))
    }
    val i = last.indexOf(1)
    println((last.values.drop(i + 1) ++ last.values.take(i)).mkString(""))
  }

  def part2(): Unit = {
    val nPlay = 50

    val startRing1M = Ring(startRing.values ++ ((startRing.values.max + 1) to 50).toList)
    val (last, _) = (1 to nPlay).foldLeft((startRing1M, startRing1M.head)) { case ((r, at), _) =>
      val nextRing = r.play1(3, at)
      println(s"${nextRing.nextValue(at)}/ $nextRing")
//      val a = nextRing.nextValue(1)
//      val b = nextRing.nextValue(a)
//      println(a, b)

      (nextRing, nextRing.nextValue(at))
    }
    val a = last.nextValue(1)
    val b = last.nextValue(a)
    println(a, b)
  }

  def main(args: Array[String]) = {
    execute
  }
}
