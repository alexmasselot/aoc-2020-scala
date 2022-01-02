package utils

import org.specs2.mutable.Specification

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
class MatrixTest extends Specification {
  val matrix = Matrix.build(List(List(1, 2, 3), List(10, 20, 30), List(100, 200, 300)))
  "MatrixTest" should {
    "get element" in {
      val el = matrix(1, 2)

      el must equalTo(30)
    }
    "appendRight" in {
      val other = Matrix.build(List(List(4, 5), List(40, 50), List(400, 500)))

      val got = matrix.appendRight(other)

      val expected = Matrix.build(List(List(1, 2, 3, 4, 5), List(10, 20, 30, 40, 50), List(100, 200, 300, 400, 500)))
      got must equalTo(expected)
    }
  }
}
