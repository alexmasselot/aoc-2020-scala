package day23

import org.specs2.mutable.Specification

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2022
 */
class RingTest extends Specification {

  "RingTest" should {

    "remove" should {
      val ring = Ring(List(3, 8, 9, 1, 2, 5, 4, 6, 7))
      "after 3" should {
        val got = ring.remove(3, 3)

        got._1 must equalTo(Ring(List(3, 2, 5, 4, 6, 7)))
        got._2 must equalTo(List(8, 9, 1))
      }

      "after 2" should {
        val got = ring.remove(3, 2)

        got._1 must equalTo(Ring(List(3, 8, 9, 1, 2, 7)))
        got._2 must equalTo(List(5, 4, 6))
      }

      "3 last" should {
        val got = ring.remove(3, 5)

        got._1 must equalTo(Ring(List(3, 8, 9, 1, 2, 5)))
        got._2 must equalTo(List(4, 6, 7))
      }

      "after last" should {
        val got = ring.remove(3, 7)

        got._1 must equalTo(Ring(List(1, 2, 5, 4, 6, 7)))
        got._2 must equalTo(List(3, 8, 9))
      }

      "after ante last" should {
        val got = ring.remove(3, 6)

        got._1 must equalTo(Ring(List(9, 1, 2, 5, 4, 6)))
        got._2 must equalTo(List(7, 3, 8))
      }

      "after ante ante last" should {
        val got = ring.remove(3, 4)

        got._1 must equalTo(Ring(List(8, 9, 1, 2, 5, 4)))
        got._2 must equalTo(List(6, 7, 3))
      }
    }

    "nextInsertValue" should {
      val ring = Ring(List(3, 2, 5, 4, 6, 7))
      "3 should 2" in {
        val got = ring.nextInsertValue(3)
        got must equalTo(2)
      }

      "2 should 7" in {
        val got = ring.nextInsertValue(2)
        got must equalTo(7)
      }

    }

    "insert" should {
      val ring = Ring(List(3, 2, 5, 4, 6, 7))
      "after last" in {
        val got = ring.insert(List(8, 9, 1), 7)
        got must equalTo(Ring(List(3, 2, 5, 4, 6, 7, 8, 9, 1)))
      }
      "after first" in {
        val got = ring.insert(List(8, 9, 1), 3)
        got must equalTo(Ring(List(3, 8, 9, 1, 2, 5, 4, 6, 7)))
      }
      "middle" in {
        val got = ring.insert(List(8, 9, 1), 5)
        got must equalTo(Ring(List(3, 2, 5, 8, 9, 1, 4, 6, 7)))
      }
    }

    "nextValue" should {
      val ring = Ring(List(3, 2, 5, 4, 6, 7))
      "after last" in {
        val got = ring.nextValue(7)
        got must equalTo(3)
      }
      "after first" in {
        val got = ring.nextValue(3)
        got must equalTo(2)
      }
      "middle" in {
        val got = ring.nextValue(5)
        got must equalTo(4)
      }
    }
  }

  "play1" should {
    val ring = Ring(List(3, 8, 9, 1, 2, 5, 4, 6, 7))
    "3" in {
      val got = ring.play1(3, 3)

      got must equalTo(Ring(List(3, 2, 8, 9, 1, 5, 4, 6, 7)))
    }
    "4" in {
      val got = ring.play1(3, 4)

      got must equalTo(Ring(List(8, 9, 1, 2, 6, 7, 3, 5, 4)))
    }
    "7" in {
      val got = ring.play1(3, 7)

      got must equalTo(Ring(List(1, 2, 5, 4, 6, 3, 8, 9, 7)))
    }
    "1" in {
      val got = ring.play1(3, 1)

      got must equalTo(Ring(List(3, 8, 9, 2, 5, 4, 1, 6, 7)))
    }

  }
}
