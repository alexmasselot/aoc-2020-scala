package day17

import org.specs2.mutable.Specification

import scala.collection.immutable.BitSet

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2022
 */
class CubeTest extends Specification {
  "build" should {
    /*
      ..#.
      .#.#
      .#..

      ####
      ..#.
      #..#


      => 2,5,7,9,12,13,14,15,18,20,23
     */
    "a 4x3x2" in {
      val got = Cube.build(
        4,
        3,
        2,
        List((2, 0, 0), (1, 1, 0), (3, 1, 0), (1, 2, 0), (0, 0, 1), (1, 0, 1), (2, 0, 1), (3, 0, 1), (2, 1, 1), (0, 2, 1), (3, 2, 1))
      )

      val expected = Cube(4, 3, 2, BitSet(2, 5, 7, 9, 12, 13, 14, 15, 18, 20, 23))
      got must equalTo(expected)
    }
    "to string" in {
      val got = Cube.build(
        4,
        3,
        2,
        List((2, 0, 0), (1, 1, 0), (3, 1, 0), (1, 2, 0), (0, 0, 1), (1, 0, 1), (2, 0, 1), (3, 0, 1), (2, 1, 1), (0, 2, 1), (3, 2, 1))
      ).toString

      val expected =
        """
          |..#.
          |.#.#
          |.#..
          |
          |####
          |..#.
          |#..#
          |""".stripMargin.trim

      got must equalTo(expected)
    }

  }

  "shift" should {
    val cube = Cube.build(
      4,
      3,
      2,
      List((2, 0, 0), (1, 1, 0), (3, 1, 0), (1, 2, 0), (0, 0, 1), (1, 0, 1), (2, 0, 1), (3, 0, 1), (2, 1, 1), (0, 2, 1), (3, 2, 1))
    )

    "xRight" in {
      val got = cube.xRight

      val expected =
        """
          |.#..
          |#.#.
          |#...
          |
          |###.
          |.#..
          |..#.""".stripMargin.trim

      got.toString must equalTo(expected)

    }
    "xLeft" in {
      val got = cube.xLeft

      val expected =
        """
          |...#
          |..#.
          |..#.
          |
          |.###
          |...#
          |.#..""".stripMargin.trim

      got.toString must equalTo(expected)

    }

    "yRight" in {
      val got = cube.yRight

      val expected =
        """
          |.#.#
          |.#..
          |....
          |
          |..#.
          |#..#
          |....""".stripMargin.trim

      got.toString must equalTo(expected)

    }

    "yLeft" in {
      val got = cube.yLeft

      val expected =
        """
          |....
          |..#.
          |.#.#
          |
          |....
          |####
          |..#.
          |""".stripMargin.trim

      got.toString must equalTo(expected)

    }

    "zRight" in {
      val got = cube.zRight

      val expected =
        """
          |####
          |..#.
          |#..#
          |
          |....
          |....
          |....
          |""".stripMargin.trim

      got.toString must equalTo(expected)

    }
    "zLeft" in {
      val got = cube.zLeft

      val expected =
        """
          |....
          |....
          |....
          |
          |..#.
          |.#.#
          |.#..
          |""".stripMargin.trim

      got.toString must equalTo(expected)

    }
  }

  "grow" should {
    val cube = Cube.build(
      4,
      3,
      2,
      List((2, 0, 0), (1, 1, 0), (3, 1, 0), (1, 2, 0), (0, 0, 1), (1, 0, 1), (2, 0, 1), (3, 0, 1), (2, 1, 1), (0, 2, 1), (3, 2, 1))
    )
    "x" in {
      val got = cube.growX

      val expected =
        """
          |...#..
          |..#.#.
          |..#...
          |
          |.####.
          |...#..
          |.#..#.
          |""".stripMargin.trim
      got.toString must equalTo(expected)
    }

    "y" in {
      val got = cube.growY

      val expected =
        """
          |....
          |..#.
          |.#.#
          |.#..
          |....
          |
          |....
          |####
          |..#.
          |#..#
          |....
          |""".stripMargin.trim
      got.toString must equalTo(expected)
    }

    "z" in {
      val got = cube.growZ

      val expected =
        """
          |....
          |....
          |....
          |
          |..#.
          |.#.#
          |.#..
          |
          |####
          |..#.
          |#..#
          |
          |....
          |....
          |....
          |""".stripMargin.trim
      got.toString must equalTo(expected)
    }
  }

  "Conway" should {
    val cube = Cube.parse2D(
      """
        |.#.
        |..#
        |###""".stripMargin.trim.split("\n"))


    "is parsed" in {
      val expected =
        """
          |.#.
          |..#
          |###""".stripMargin.trim

      cube.toString must equalTo(expected)
    }

    "step 1" in {
      val got = cube.grow.conway
      val expected =
        """
          |.....
          |.....
          |.#...
          |...#.
          |..#..
          |
          |.....
          |.....
          |.#.#.
          |..##.
          |..#..
          |
          |.....
          |.....
          |.#...
          |...#.
          |..#..""".stripMargin.trim

      got.toString must equalTo(expected)
    }
  }

}
