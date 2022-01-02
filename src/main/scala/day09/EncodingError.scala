package day09

import utils.{InputLoader, ListUtils}

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object EncodingError {
  def findSum(win: List[Long], n: Long): Option[(Long, Long)] = {
    ListUtils.combinationWithoutRepetitions(win, 2)
      .find(_.sum == n)
      .map(l => (l(0), l(1)))
  }


  def missSum(numbers: List[Long], window: Int) = {
    numbers.sliding(window).zip(numbers.drop(window)).filter { case (win, n) =>
      findSum(win, n).isEmpty
    }
      .map(_._2)
      .take(1)
      .toList
      .head
  }

  def part1(numbers: List[Long], window: Int) = {
    val xs = missSum(numbers, window)
    println(xs)
  }

  def part2(numbers: List[Long], window: Int) = {
    val target = missSum(numbers, window)

    def findSummingRange(remain: List[Long], acc: List[Long], accTot: Long): List[Long] = {
      if (accTot == target) {
        return acc
      }
      if (accTot > target) {
        return findSummingRange(remain, acc.drop(1), accTot - acc.head)
      }
      return findSummingRange(remain.drop(1), acc :+ remain.head, accTot + remain.head)
    }

    val sumList = findSummingRange(numbers, List(), 0)
    println(sumList.min + sumList.max)
  }

  def main(args: Array[String]) = {
    val input = InputLoader.read("09", false).map {
      _.toLong
    }

    part1(input, 25)
    part2(input, 25)
  }
}
