package day24

import utils.Direction.{LEFT, RIGHT, UP, DOWN}
import utils.{AOCExecutor, InputLoader, Matrix}

case class HexaDirection(val name: String, val x: Int, val y: Int) {
  override def toString: String = name
}

case class Position(x: Int, y: Int) {
  def +(dir: HexaDirection) = Position(x + dir.x, y + dir.y)
}

object Position {
  def computePosition(moves: List[HexaDirection]) =
    moves.foldLeft(Position(0, 0)) { case (acc, d) => acc + d }
}


/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object LobbyLayout extends AOCExecutor {
  def parseList(line: String): List[HexaDirection] = {
    val reE = "e(.*)".r
    val reSE = "se(.*)".r
    val reSW = "sw(.*)".r
    val reW = "w(.*)".r
    val reNW = "nw(.*)".r
    val reNE = "ne(.*)".r

    val hexaE = HexaDirection("e", 1, 0)
    val hexaSE = HexaDirection("se", 1, 1)
    val hexaSW = HexaDirection("sw", 0, 1)
    val hexaW = HexaDirection("w", -1, 0)
    val hexaNW = HexaDirection("nw", -1, -1)
    val hexaNE = HexaDirection("ne", 0, -1)

    def handler(remain: String, acc: List[HexaDirection] = List()): List[HexaDirection] = remain match {
      case "" => acc
      case reE(rem) => handler(rem, acc :+ hexaE)
      case reSE(rem) => handler(rem, acc :+ hexaSE)
      case reSW(rem) => handler(rem, acc :+ hexaSW)
      case reW(rem) => handler(rem, acc :+ hexaW)
      case reNW(rem) => handler(rem, acc :+ hexaNW)
      case reNE(rem) => handler(rem, acc :+ hexaNE)
      case _ => ???
    }

    handler(line)
  }

  val input = InputLoader.read("24", false).map { l => parseList(l) }

  def part1(): Unit = {
    val blacks = input.foldLeft(Set[Position]()) { case (acc, moves) =>
      val pos = Position.computePosition(moves)
      if (acc.contains(pos)) {
        acc - pos
      } else {
        acc + pos
      }
    }
    println(blacks.size)
  }

  def part2(): Unit = {
    val blacks = input.foldLeft(Set[Position]()) { case (acc, moves) =>
      val pos = Position.computePosition(moves)
      if (acc.contains(pos)) {
        acc - pos
      } else {
        acc + pos
      }
    }

    val minX = blacks.map {
      _.x
    }.min
    val maxX = blacks.map {
      _.x
    }.max
    val minY = blacks.map {
      _.y
    }.min
    val maxY = blacks.map {
      _.y
    }.max
    val matPos = blacks.foldLeft(Matrix.fill[Boolean](maxX - minX + 1, maxY - minY + 1)(false)) { case (acc, p) =>
      acc.set(p.x - minX, p.y - minY, true)
    }


    def play(mat: Matrix[Boolean]): Matrix[Boolean] = {
      val larger = mat.grow(false)
      val withNeighbors = larger.zipN(
        larger.shift(RIGHT, false),
        larger.shift(RIGHT, false).shift(DOWN, false),
        larger.shift(DOWN, false),
        larger.shift(LEFT, false),
        larger.shift(LEFT, false).shift(UP, false),
        larger.shift(UP, false),
      )

      withNeighbors.map { xs =>
        val x = xs.head
        val ns = xs.drop(1)
        val countBlack = ns.count { b => b }
        if (x && (countBlack == 0 || countBlack > 2)) {
          false
        } else if (!x && countBlack == 2) {
          true
        } else {
          x
        }
      }
    }

    def countBlack(mat: Matrix[Boolean]) = mat.count { b => b }


    val matFinal = (1 to 100).foldLeft(matPos){case(m, _) => play(m)}
    println(countBlack(matFinal))

  }

  def main(args: Array[String]) = {
    execute
  }
}
