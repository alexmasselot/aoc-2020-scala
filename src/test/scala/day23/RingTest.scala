package day23

import org.specs2.mutable.Specification

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2022
 */
class RingTest extends Specification {

  "RingTest" should {


    "nextInsertValue" should {
      val ring = Ring.build(List(3, 8, 9, 1, 2, 5, 4, 6, 7))
      "3 should 2" in {
        val got = ring.nextInsertValue(3, (8, 9, 1))
        got must equalTo(2)
      }

      "2 should 7" in {
        val got = ring.nextInsertValue(2, (8, 9, 1))
        got must equalTo(7)
      }

    }

    "insert" should {
      "after last" in {
        val ring = Ring.build(List(3, 8, 9, 1, 2, 5, 4, 6, 7))
        ring.insert((8, 9, 1), 3, 7)
        ring.values(3) must equalTo(List(3, 2, 5, 4, 6, 7, 8, 9, 1))
      }
      "after first" in {
        val ring = Ring.build(List(3, 8, 9, 1, 2, 5, 4, 6, 7))
        val got = ring.insert((8, 9, 1), 3,3)
        ring.values(3) must equalTo(List(3, 8, 9, 1, 2, 5, 4, 6, 7))
      }
      "middle" in {
        val ring = Ring.build(List(3, 8, 9, 1, 2, 5, 4, 6, 7))
        val got = ring.insert((8, 9, 1), 3, 5)
        ring.values(3) must equalTo(List(3, 2, 5, 8, 9, 1, 4, 6, 7))
      }
    }

    "nextValue" should {
      val ring = Ring.build(List(3, 8, 9, 1, 2, 5, 4, 6, 7))
      "after last" in {
        val got = ring.nextValue(7)
        got must equalTo(3)
      }
      "after first" in {
        val ring = Ring.build(List(3, 8, 9, 1, 2, 5, 4, 6, 7))
        val got = ring.nextValue(3)
        got must equalTo()
      }
      "middle" in {
        val ring = Ring.build(List(3, 8, 9, 1, 2, 5, 4, 6, 7))
        val got = ring.nextValue(5)
        got must equalTo(4)
      }
    }
  }

  "play1" should {
    "3" in {
      val ring = Ring.build(List(3, 8, 9, 1, 2, 5, 4, 6, 7))
      val got = ring.play(3)

      ring.values(3) must equalTo(List(3, 2, 8, 9, 1, 5, 4, 6, 7))
      got must equalTo(2)
    }
    "4" in {
      val ring = Ring.build(List(3, 8, 9, 1, 2, 5, 4, 6, 7))
      val got = ring.play(4)

      ring.values(8) must equalTo(List(8, 9, 1, 2, 6, 7, 3, 5, 4))
      got must equalTo(8)

    }
    "7" in {
      val ring = Ring.build(List(3, 8, 9, 1, 2, 5, 4, 6, 7))
      val got = ring.play(7)

      ring.values(1) must equalTo(List(1, 2, 5, 4, 6, 3, 8, 9, 7))
      got must equalTo(1)

    }
    "1" in {
      val ring = Ring.build(List(3, 8, 9, 1, 2, 5, 4, 6, 7))
      val got = ring.play(1)

      ring.values(3) must equalTo(List(3, 8, 9, 2, 5, 4, 1, 6, 7))
      got must equalTo(6)
    }

  }
}
