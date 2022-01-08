package day13

import day13.ShuttleSearch.{ChineseTarget, chineseSolver}
import org.specs2.mutable.Specification

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2022
 */
class ShuttleSearchTest extends Specification {
  "chinese solver" should {
    "two numbers" in {
      val given = List(
        ChineseTarget(4, 5),
        ChineseTarget(3, 4)
      )
      val got = chineseSolver(given)

      got must equalTo(19L)

    }
    "three numbers" in {
      val given = List(
        ChineseTarget(4, 5),
        ChineseTarget(3, 4),
        ChineseTarget(0, 3)
      )
      val got = chineseSolver(given)

      got must equalTo(39L)

    }
    "bus sample 1" in {
      val given = List(
        ChineseTarget(0, 17),
        ChineseTarget(13 - 2, 13),
        ChineseTarget(19 - 3, 19),
      )
      val got = chineseSolver(given)

      got must equalTo(3417)

    }
    "bus sample 4" in {
      val given = List(
        ChineseTarget(0, 7),
        ChineseTarget(13 - 1, 13),
        ChineseTarget(59 - 4, 59),
        ChineseTarget(31 - 6, 31),
        ChineseTarget(19 - 7, 19),
      )
      val got = chineseSolver(given)

      got must equalTo(1068781L)

    }
  }

}
