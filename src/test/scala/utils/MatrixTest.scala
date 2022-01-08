package utils

import org.specs2.mutable.Specification
import utils.Direction.{LEFT, RIGHT, UP, DOWN}

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
class MatrixTest extends Specification {
  val matrix = Matrix.build(List(List(1, 2, 3), List(10, 20, 30), List(100, 200, 300)))
  "Build" should {
    "nRows" in {
      matrix.nRows must equalTo(3)
    }
    "nCols" in {
      matrix.nCols must equalTo(3)
    }
    "get element" in {
      val el = matrix(1, 2)

      el must equalTo(30)
    }

    "append" should {
      "right" in {
        val other = Matrix.build(List(List(4, 5), List(40, 50), List(400, 500)))

        val got = matrix.append(RIGHT, other)

        val expected = Matrix.build(List(List(1, 2, 3, 4, 5), List(10, 20, 30, 40, 50), List(100, 200, 300, 400, 500)))
        got must equalTo(expected)
      }
    }

    "fillRows" in {
      val got = Matrix.fillRows(2, List(1, 2, 3))

      val expected = Matrix.build(List(List(1, 2, 3), List(1, 2, 3)))

      got must equalTo(expected)
    }

    "fillColumns" in {
      val got = Matrix.fillColumns(2, List(1, 2, 3))

      val expected = Matrix.build(List(List(1, 1), List(2, 2), List(3, 3)))

      got must equalTo(expected)
    }
  }

  "transformations" should {
    "map" in {
      val got = matrix.map {
        _ * 10
      }
      val expected = Matrix.build(List(List(10, 20, 30), List(100, 200, 300), List(1000, 2000, 3000)))

      got must equalTo(expected)
    }


  }
  "scan" should {
    "left" in {
      val got = matrix.scan(LEFT, 0) { case (acc, i) => acc + i }
      val expected = Matrix.build(List(List(1, 3, 6), List(10, 30, 60), List(100, 300, 600)))

      got must equalTo(expected)
    }
    "right" in {
      val got = matrix.scan(RIGHT, 0) { case (acc, i) => acc + i }
      val expected = Matrix.build(List(List(6, 5, 3), List(60, 50, 30), List(600, 500, 300)))

      got must equalTo(expected)
    }
    "up" in {
      val got = matrix.scan(UP, 0) { case (acc, i) => acc + i }
      val expected = Matrix.build(List(List(1, 2, 3), List(11, 22, 33), List(111, 222, 333)))

      got must equalTo(expected)
    }
    "down" in {
      val got = matrix.scan(DOWN, 0) { case (acc, i) => acc + i }
      val expected = Matrix.build(List(List(111, 222, 333), List(110, 220, 330), List(100, 200, 300)))

      got must equalTo(expected)
    }
  }

  "scanDiag" should {
    val matrix = Matrix.build(List(List(1, 2, 3), List(10, 20, 30), List(100, 200, 300), List(1000, 2000, 3000)))

    /**
     * 1     2     3
     * 10    20    30
     * 100   200   300
     * 1000  2000  3000
     */

    " UP, LEFT" in {
      val got = matrix.scanDiag(UP, LEFT, 0) { case (acc, i) => acc + i }
      val expected = Matrix.build(List(List(1, 2, 3), List(10, 21, 32), List(100, 210, 321), List(1000, 2100, 3210)))

      got must equalTo(expected)
    }
    " UP, RIGHT" in {
      val got = matrix.scanDiag(UP, RIGHT, 0) { case (acc, i) => acc + i }
      val expected = Matrix.build(List(List(1, 2, 3), List(12, 23, 30), List(123, 230, 300), List(1230, 2300, 3000)))

      got must equalTo(expected)
    }
    " DOWN, LEFT" in {
      val got = matrix.scanDiag(DOWN, LEFT, 0) { case (acc, i) => acc + i }
      val expected = Matrix.build(List(List(1, 12, 123), List(10, 120, 1230), List(100, 1200, 2300), List(1000, 2000, 3000)))

      got must equalTo(expected)
    }
    " DOWN, RIGHT" in {
      val got = matrix.scanDiag(DOWN, RIGHT, 0) { case (acc, i) => acc + i }
      val expected = Matrix.build(List(List(321, 32, 3), List(3210, 320, 30), List(2100, 3200, 300), List(1000, 2000, 3000)))

      got must equalTo(expected)
    }
  }
  "fold" should {
    "foldRows" in {
      val got = matrix.foldRows(0) { case (acc, i) => acc + i }
      val expected = IndexedSeq(6, 60, 600)
      got must equalTo(expected)
    }
    "foldColumns" in {
      val got = matrix.foldColumns(0) { case (acc, i) => acc + i }
      val expected = IndexedSeq(111, 222, 333)
      got must equalTo(expected)
    }
  }

  "zip" should {
    "with one" in {
      val other = matrix.map {
        _ + 3
      }
      val got = matrix.zip(other)

      val expected = Matrix.build(
        List(
          List((1, 4), (2, 5), (3, 6)),
          List((10, 13), (20, 23), (30, 33)),
          List((100, 103), (200, 203), (300, 303)),
        )
      )

      got must equalTo(expected)
    }
    "with N" in {
      val other1 = matrix.map {
        _ + 3
      }
      val other2 = matrix.map {
        _ + 6
      }
      val got = matrix.zipN(other1, other2)

      val expected = Matrix.build(
        List(
          List(List(1, 4, 7), List(2, 5, 8), List(3, 6, 9)),
          List(List(10, 13, 16), List(20, 23, 26), List(30, 33, 36)),
          List(List(100, 103, 106), List(200, 203, 206), List(300, 303, 306)),
        )
      )

      got must equalTo(expected)
    }
  }
  "shift (non circular)" should {
    "left" in {
      val got = matrix.shift(LEFT, 0)
      val expected = Matrix.build(List(List(0, 1, 2), List(0, 10, 20), List(0, 100, 200)))

      got must equalTo(expected)

    }
    "right" in {
      val got = matrix.shift(RIGHT, 0)
      val expected = Matrix.build(List(List(2, 3, 0), List(20, 30, 0), List(200, 300, 0)))

      got must equalTo(expected)

    }
    "up" in {
      val got = matrix.shift(UP, 0)
      val expected = Matrix.build(List(List(0, 0, 0), List(1, 2, 3), List(10, 20, 30)))

      got must equalTo(expected)

    }
    "down" in {
      val got = matrix.shift(DOWN, 0)
      val expected = Matrix.build(List(List(10, 20, 30), List(100, 200, 300), List(0, 0, 0)))

      got must equalTo(expected)

    }
  }
  "neighbors8" in {
    val got = matrix.neighbors8().map {
      _.sorted
    }
    val expected = Matrix.build(List(
      List(List(2, 20, 10), List(3, 30, 20, 10, 1), List(20, 30, 2)),
      List(List(1, 2, 20, 100, 200), List(2, 3, 30, 300, 200, 100, 10, 1), List(2, 3, 300, 200, 20)),
      List(List(10, 20, 200), List(10, 20, 30, 300, 100), List(30, 200, 20))
    )).map {
      _.sorted
    }
    got must equalTo(expected)
  }
}
