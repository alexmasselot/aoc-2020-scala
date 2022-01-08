package day09

import utils.{AOCExecutor, InputLoader, ListUtils}

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object EncodingError extends AOCExecutor {
  val numbers = InputLoader.read("09", false).map {
    _.toLong
  }
  val window = 25

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

  def part1() = {
    val xs = missSum(numbers, window)
    println(xs)
  }

  def part2() = {
    val target = missSum(numbers, window)

    def findSummingRange(remain: List[Long], acc: List[Long], accTot: Long): List[Long] = {
      if (accTot == target) {
        return acc
      }
      if (accTot > target) {
        return findSummingRange(remain, acc.drop(1), accTot - acc.head)
      }
       findSummingRange(remain.drop(1), acc :+ remain.head, accTot + remain.head)
    }

    val sumList = findSummingRange(numbers, List(), 0)
    println(sumList.min + sumList.max)
  }

  def main(args: Array[String]) = {
    execute
  }
}
