package day15

import org.specs2.mutable.Specification

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2022
 */
class SpokenTest extends Specification {

  "Companion" should {
    "parse" in {
      val given = "0,3,6"
      val got = Spoken.parse(given)
      got must equalTo(Spoken(
        4,
        Map(0 -> 1, 3 -> 2),
        6
      ))
    }
  }
  "Next" should {
    val spoken = Spoken.parse("0,3,6")
    "0,3,6 #4" in {
      val got = spoken.next()
      got must equalTo(Spoken(
        5,
        Map(0 -> 1, 3 -> 2, 6 -> 3),
        0
      ))
    }
    "0,3,6 #5" in {
      val got = spoken.next().next()

      got must equalTo(Spoken(
        6,
        Map(0 -> 4, 3 -> 2, 6 -> 3),
        3
      ))
    }
    "0,3,6 #6" in {
      val got = spoken.next().next().next()

      got must equalTo(Spoken(
        7,
        Map(0 -> 4, 3 -> 5, 6 -> 3),
        3
      ))
    }
    "0,3,6 #7" in {
      val got = spoken.next().next().next().next()

      got must equalTo(Spoken(
        8,
        Map(0 -> 4, 3 -> 6, 6 -> 3),
        1
      ))
    }
    "0,3,6 next(7)" in {
      val got = spoken.next(7)

      got must equalTo(Spoken(
        8,
        Map(0 -> 4, 3 -> 6, 6 -> 3),
        1
      ))
    }
    "0,3,6 next(2020)" in {
      val got = spoken.next(2020)

      got.last must equalTo(436)
    }

    "1,3,2 next(2020)" in {
      val got = Spoken.parse("1,3,2").next(2020)
      got.last must equalTo(1)
    }
    "2,1,3 next(2020)" in {
      val got = Spoken.parse("2,1,3").next(2020)
      got.last must equalTo(10)
    }
    "1,2,3 next(2020)" in {
      val got = Spoken.parse("1,2,3").next(2020)
      got.last must equalTo(27)
    }
    "2,3,1 next(2020)" in {
      val got = Spoken.parse("2,3,1").next(2020)
      got.last must equalTo(78)
    }
    "3,2,1 next(2020)" in {
      val got = Spoken.parse("3,2,1").next(2020)
      got.last must equalTo(438)
    }
    "3,1,2 next(2020)" in {
      val got = Spoken.parse("3,1,2").next(2020)
      got.last must equalTo(1836)
    }
  }
}
