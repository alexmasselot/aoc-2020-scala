package day20

import org.specs2.mutable.Specification

import scala.collection.immutable.BitSet

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2022
 */
class TileTest extends Specification {

  "TileTest" should {
    val input2311 =
      """
        |Tile 2311:
        |..##.#..#.
        |##..#.....
        |#...##..#.
        |####.#...#
        |##.##.###.
        |##...#.###
        |.#.#.#..##
        |..#....#..
        |###...#.#.
        |..###..###""".stripMargin.trim

    val input1427 =
      """
        |Tile 1427:
        |###.##.#..
        |.#..#.##..
        |.#.##.#..#
        |#.#.#.##.#
        |....#...##
        |...##..##.
        |...#.#####
        |.#.####.#.
        |..#..###.#
        |..##.#..#.""".stripMargin.trim

    val input1489 =
      """
        |Tile 1489:
        |##.#.#....
        |..##...#..
        |.##..##...
        |..#...#...
        |#####...#.
        |#..#.#.#.#
        |...#.#.#..
        |##.#...##.
        |..##.##.##
        |###.##.#..""".stripMargin.trim

    val input2473 =
      """
        |Tile 2473:
        |#....####.
        |#..#.##...
        |#.##..#...
        |######.#.#
        |.#...#.#.#
        |.#########
        |.###.#..#.
        |########.#
        |##...##.#.
        |..###.#.#.""".stripMargin.trim

    val input1171 =
      """
        |Tile 1171:
        |####...##.
        |#..##.#..#
        |##.#..#.#.
        |.###.####.
        |..###.####
        |.##....##.
        |.#...####.
        |#.##.####.
        |####..#...
        |.....##...""".stripMargin.trim

    val input3079 =
      """
        |Tile 3079:
        |#.#.#####.
        |.#..######
        |..#.......
        |######....
        |####.#..#.
        |.#...#.##.
        |#.#####.##
        |..#.###...
        |..#.......
        |..#.###...""".stripMargin.trim


    "build" in {
      val got = Tile.parse(input2311)
      got.dots.values.head must equalTo(IndexedSeq(false, false, true, true, false, true, false, false, true, false))
    }

    "TileBorder" in {
      val tileBorder = TileBorder.build(Tile.parse(input2311))

      tileBorder.borders must equalTo(
        List(
          BitSet(2, 3, 5, 8),
          BitSet(1, 2, 3, 4, 5, 8),
          BitSet(2, 3, 4, 7, 8, 9),
          BitSet(3, 5, 6, 9),
        )
      )
    }

    "align 1427/1489" in {
      /*
      *
      * Tile 1427:
      * ###.##.#..
      * .#..#.##..
      * .#.##.#..#
      * #.#.#.##.#
      * ....#...##
      * ...##..##.
      * ...#.#####
      * .#.####.#.
      * ..#..###.#
      * ..##.#..#.
      *
      * Tile 1489:
      * ##.#.#....
      * ..##...#..
      * .##..##...
      * ..#...#...
      * #####...#.
      * #..#.#.#.#
      * ...#.#.#..
      * ##.#...##.
      * ..##.##.##
      * ###.##.#..
      */
      val anchor = Tile.parse(input1427)
      val other = Tile.parse(input1489)

      val n = other.transformed.count { o => !anchor.border.align(o.border).isEmpty }
      n must equalTo(1)
    }
    "align 1711/1489" in {
      /*
       *
       * Tile 1171:
       * ####...##.
       * #..##.#..#
       * ##.#..#.#.
       * .###.####.
       * ..###.####
       * .##....##.
       * .#...####.
       * #.##.####.
       * ####..#...
       * .....##...
       *
       * Tile 1489:
       * ##.#.#....
       * ..##...#..
       * .##..##...
       * ..#...#...
       * #####...#.
       * #..#.#.#.#
       * ...#.#.#..
       * ##.#...##.
       * ..##.##.##
       * ###.##.#..
       */
      val anchor = Tile.parse(input1171)
      val other = Tile.parse(input1489)

      val n = other.transformed.count { o => !anchor.border.align(o.border).isEmpty }

      //      println(anchor.borders)
      //      println("----------------")
      //
      //      other.transformed.foreach{o => println(o.borders)}

      n must equalTo(1)
    }

    "align 3079/2311" in {
      /*
       *
       * Tile 3079:
       * #.#.#####.
       * .#..######
       * ..#.......
       * ######....
       * ####.#..#.
       * .#...#.##.
       * #.#####.##
       * ..#.###...
       * ..#.......
       * ..#.###...
       *
       * Tile 2311:
       * ..##.#..#.
       * ##..#.....
       * #...##..#.
       * ####.#...#
       * ##.##.###.
       * ##...#.###
       * .#.#.#..##
       * ..#....#..
       * ###...#.#.
       * ..###..###
       */
      val anchor = Tile.parse(input3079)
      val other = Tile.parse(input2311)

      val n = other.transformed.count { o => !anchor.border.align(o.border).isEmpty }

      n must equalTo(1)
    }
  }
}
