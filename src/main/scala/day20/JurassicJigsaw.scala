package day20

import breeze.numerics.sqrt
import utils.Direction.{DOWN, Direction, LEFT, RIGHT, UP}
import utils.TransformationUnit.{VERTICAL_SYMMETRY, ROTATION}
import utils.{AOCExecutor, InputLoader, Matrix}

import scala.collection.immutable.BitSet

case class Tile(
                 val id: Int,
                 val dots: Matrix[Boolean]
               ) {
  val border: TileBorder = {
    TileBorder(
      List(
        BitSet() ++ dots.row(0).zipWithIndex.filter {
          _._1
        }.map {
          _._2
        },
        BitSet() ++ dots.col(0).zipWithIndex.filter {
          _._1
        }.map {
          _._2
        },
        BitSet() ++ dots.row(-1).zipWithIndex.filter {
          _._1
        }.map {
          _._2
        },
        BitSet() ++ dots.col(-1).zipWithIndex.filter {
          _._1
        }.map {
          _._2
        },
      ),
      dots.nCols
    )
  }

  def transformed = List(
    List(),
    List(ROTATION),
    List(ROTATION, ROTATION),
    List(ROTATION, ROTATION, ROTATION),
    List(VERTICAL_SYMMETRY),
    List(VERTICAL_SYMMETRY, ROTATION),
    List(VERTICAL_SYMMETRY, ROTATION, ROTATION),
    List(VERTICAL_SYMMETRY, ROTATION, ROTATION, ROTATION),
  ).map { ts => Tile(id, dots.transform(ts)) }

  override def toString: String =
    s"""Tile ${id}
       |${dots.map { b => if (b) '#' else '.' }.toString("")}""".stripMargin
}

object Tile {
  val reTitle = raw"Tile (\d+):".r

  def parse(block: String) = {
    val lines = block.split("\n")
    val reTitle(id) = lines.head

    val matrix = Matrix.build(
      lines.drop(1)
        .map {
          _.map {
            _ == '#'
          }.toList
        }.toList
    )
    Tile(id.toInt, matrix)
  }
}

case class TileBorder(
                       val borders: List[BitSet], //Up, Left, Down, Right
                       length: Int
                     ) {

  def align(other: TileBorder): Option[Direction] = {
    if (other.borders(1) == borders(3)) return Some(RIGHT)
    if (other.borders(3) == borders(1)) return Some(LEFT)
    if (other.borders(2) == borders(0)) return Some(UP)
    if (other.borders(0) == borders(2)) return Some(DOWN)
    None
  }

}

case class TileCollection(
                           tiles: List[Tile]
                         ) {
  private val dico = tiles.map { tb => tb.id -> tb }.toMap

  def size = tiles.size

  def apply(i: Int) = dico(i)

  def -(tile: Tile) = TileCollection(tiles.filter {
    _.id != tile.id
  })

  def findCompatible(target: Tile, direction: Direction) = {
    tiles
      .filter {
        _.id != target.id
      }
      .map { tb =>
        tb.transformed.find { tbt => target.border.align(tbt.border) == Some(direction) }
      }
      .filter {
        _.isDefined
      }
      .map {
        _.get
      }
      .headOption
  }
}

object TileCollection {
  def parse(input: Seq[String]) = TileCollection(input.mkString("\n")
    .split("\n\n")
    .map { block =>
      Tile.parse(block)
    }.toList
  )
}

object TileBorder {
  def build(tile: Tile): TileBorder = {
    TileBorder(
      List(
        BitSet() ++ tile.dots.row(0).zipWithIndex.filter {
          _._1
        }.map {
          _._2
        },
        BitSet() ++ tile.dots.col(0).zipWithIndex.filter {
          _._1
        }.map {
          _._2
        },
        BitSet() ++ tile.dots.row(-1).zipWithIndex.filter {
          _._1
        }.map {
          _._2
        },
        BitSet() ++ tile.dots.col(-1).zipWithIndex.filter {
          _._1
        }.map {
          _._2
        },
      ),
      tile.dots.nRows
    )
  }
}


/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object JurassicJigsaw extends AOCExecutor {

  val input = InputLoader.read("20", false)
  val tiles = TileCollection.parse(input)


  def findCornersIds = tiles.tiles.map { tile =>
    tile.id -> List(UP, LEFT, DOWN, RIGHT)
      .map { dir =>
        tiles.findCompatible(tile, dir).map {
          _.id
        }
      }
      .filter {
        _.isDefined
      }
      .size
  }
    .filter {
      _._2 == 2
    }
    .map {
      _._1
    }

  def turnUpperLeft(tile: Tile) = {
    tile.transformed.find { t => tiles.findCompatible(t, LEFT).isEmpty && tiles.findCompatible(t, UP).isEmpty }.get
  }


  def findConnected(target: Tile, in: TileCollection, direction: Direction) = {
    in.findCompatible(target, direction)
  }

  def part1(): Unit = {
    val n = findCornersIds.foldLeft(1L) {
      _ * _
    }
    println(n)
  }

  def part2(): Unit = {
    val nbTile = sqrt(tiles.tiles.size).toInt

    def handler(remain: TileCollection, acc: List[List[Tile]]): List[List[Tile]] = {
      if (remain.tiles.size == 0) {
        return acc
      }
      acc match {
        case Nil => {
          val corner = tiles(findCornersIds.head)
          val turnedCorner = turnUpperLeft(corner)
          handler(remain - corner, List(List(turnedCorner)))
        }
        case row0 :: Nil => if (row0.size < nbTile) {
          val newTile = findConnected(row0.last, remain, RIGHT).head
          handler(remain - newTile, List(row0 :+ newTile))
        } else {
          handler(remain, acc :+ List())
        }
        case _ => {
          val rowUp = acc.takeRight(2).head
          val rowCur = acc.last
          if (rowCur.size < nbTile) {
            val upEl = rowUp(rowCur.size)

            val newTile = findConnected(upEl, remain, DOWN).head
            handler(remain - newTile, acc.dropRight(1) :+ (rowCur :+ newTile))
          } else {
            handler(remain, acc :+ List())
          }

        }
      }
    }

    val assembled = handler(tiles, List())
    val mappedRows = assembled
      .map {
        row =>
          row.map {
            _.dots.shrink()
          }
      }
      .map {
        row => {
          row.drop(1).foldLeft(row.head) {
            case (acc, m) => acc.append(RIGHT, m)
          }
        }
      }

    val mat = mappedRows.drop(1).foldLeft(mappedRows.head) {
      case (acc, m) => acc.append(DOWN, m)
    }
    val tileAssemble = Tile(0, mat)

    val seaMonsterDef =
      """
        |..................#.
        |#....##....##....###
        |.#..#..#..#..#..#...""".stripMargin.trim
    val seaMonster =
    seaMonsterDef.replaceAll(" ", ".").replaceAll("\n", ".".repeat(tileAssemble.dots.nCols - 20)).r

    val seaMonsterCount = tileAssemble.transformed.map { t =>
      val tLine = t.toString.replace("""Tile 0""", "").replaceAll("\n", "")

      def handler(remain: String, shift: Int): Int = {
        seaMonster.findFirstMatchIn(remain) match {
          case None => 0
          case Some(m) => {
            val posLine = (shift + m.start) % tileAssemble.dots.nCols
            (if (posLine < tileAssemble.dots.nCols - 20) 1 else 0) + handler(remain.drop(m.start + 1), shift + m.start + 1)
          }
        }
      }

      handler(tLine, 0)
    }.max

    val n = tileAssemble.dots.count { b => b } - 15 * seaMonsterCount
    println(s"${seaMonsterCount} monsters / ${tileAssemble.dots.count(identity)} => $n")
  }

  def main(args: Array[String]) = {
    execute
  }
}
