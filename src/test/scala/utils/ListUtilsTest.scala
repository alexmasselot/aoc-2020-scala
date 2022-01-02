package utils

import org.specs2.mutable.Specification

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
class ListUtilsTest extends Specification {
  "combinationWithoutRepetitions 2" >> {

    val given = 1 :: 2 :: 3 :: Nil

    val got = ListUtils.combinationWithoutRepetitions(given, 2)
    val expected = List(List(1, 2), List(1, 3), List(2, 3))
    got must equalTo(expected)
  }

  "combinationWithoutRepetitions 3 out of 5" >> {

    val given = 1 :: 2 :: 3 :: 4 :: 5 :: Nil

    val got = ListUtils.combinationWithoutRepetitions(given, 3)
    val expected = List(
      List(1, 2, 3),
      List(1, 2, 4),
      List(1, 2, 5),
      List(1, 3, 4),
      List(1, 3, 5),
      List(1, 4, 5),
      List(2, 3, 4),
      List(2, 3, 5),
      List(2, 4, 5),
      List(3, 4, 5),
    )
    got must equalTo(expected)
  }
}
