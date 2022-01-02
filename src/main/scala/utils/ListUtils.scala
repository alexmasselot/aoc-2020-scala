package utils

import scala.annotation.tailrec

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object ListUtils {
  def combinationWithoutRepetitions[A](orig: List[A], nDraw: Int): List[List[A]] = {
    def combinator[A](n: Int, list: List[A], acc: List[A]): List[List[A]] = {
      if (n == 0)
        List(acc.reverse)
      else if (list == Nil)
        List()
      else
        combinator(n - 1, list.tail, list.head :: acc) ::: combinator(n, list.tail, acc)
    }
    combinator(nDraw, orig, List())
  }
}
