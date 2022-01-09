package day17

import org.specs2.mutable.Specification

import scala.collection.immutable.BitSet

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2022
 */
class CubeTest extends Specification {
  "parse universe" should {
    "sample" in {
      val given =
        """
          |.#.
          |..#
          |###""".stripMargin.split("\n")

      val cu = CubeUniverse.parse2D(given, 3)

      cu.size must equalTo(5)
    }
  }

  "propagate" in {
    val cube = Cube(List(2, 5, 8))

    val got = cube.propagate

    val expected = Set(
      Cube(List(2 - 1, 5, 8)),
      Cube(List(2 + 1, 5, 8)),
      Cube(List(2, 5 - 1, 8)),
      Cube(List(2, 5 + 1, 8)),
      Cube(List(2, 5, 8 - 1)),
      Cube(List(2, 5, 8 + 1)),
      Cube(List(2 - 1, 5 - 1, 8)),
      Cube(List(2 - 1, 5 + 1, 8)),
      Cube(List(2 - 1, 5, 8 + 1)),
      Cube(List(2 - 1, 5, 8 - 1)),
      Cube(List(2 + 1, 5 - 1, 8)),
      Cube(List(2 + 1, 5 + 1, 8)),
      Cube(List(2 + 1, 5, 8 + 1)),
      Cube(List(2 + 1, 5, 8 - 1)),
      Cube(List(2, 5 - 1, 8 - 1)),
      Cube(List(2, 5 - 1, 8 + 1)),
      Cube(List(2, 5 + 1, 8 - 1)),
      Cube(List(2, 5 + 1, 8 + 1)),
      Cube(List(2 - 1, 5 - 1, 8 - 1)),
      Cube(List(2 - 1, 5 - 1, 8 + 1)),
      Cube(List(2 - 1, 5 + 1, 8 - 1)),
      Cube(List(2 - 1, 5 + 1, 8 + 1)),
      Cube(List(2 + 1, 5 - 1, 8 - 1)),
      Cube(List(2 + 1, 5 - 1, 8 + 1)),
      Cube(List(2 + 1, 5 + 1, 8 - 1)),
      Cube(List(2 + 1, 5 + 1, 8 + 1)),
    )

    got.toSet must equalTo(expected)

  }

}
