package day23

import scala.annotation.tailrec

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2022
 */
case class Ring(next: Array[Int]) {

  def size = next.size

  def values(start: Int) = {
    @tailrec
    def handler(i: Int, acc: List[Int] = List()): List[Int] = {
      val j = next(i)
      if (j == start) {
        return acc :+ i
      }
      handler(j, acc :+ i)
    }

    handler(start)
  }

  def getNext3(after: Int) =
    (next(after), next(next(after)), next(next(next(after))))

  def nextInsertValue(after: Int, skip: (Int, Int, Int)): Int =
    (after - 1 to after - 4 by -1).map { x => if (x <= 0) x + size - 1 else x }.find { x => x != skip._1 && x != skip._2 && x != skip._3 }.get

  def insert(xs: (Int, Int, Int), origin: Int, after: Int) = {
    next(origin) = next(xs._3)
    next(xs._3) = next(after)
    next(after) = xs._1
  }

  def nextValue(after: Int): Int = next(after)

  def play(at: Int): Int = {
    val chunk = getNext3(at)
    val nextAt = nextInsertValue(at, chunk)
    insert(chunk, at, nextAt)
    next(at)
  }

  override def toString: String = values(1).mkString(" ")
}

object Ring {
  def build(values: List[Int]) = {
    val next = values.zip(values.drop(1)) :+ (values.last -> values.head)
    Ring(
      next.sortBy { x => x._1 }.map {
        _._2
      }.toArray.prepended(0)
    )
  }
}
