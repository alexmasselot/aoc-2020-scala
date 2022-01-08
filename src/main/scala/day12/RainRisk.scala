package day12

import day12.DirectionCompass.{DirectionCompass, EAST, NORTH, SOUTH, WEST}
import utils.{AOCExecutor, InputLoader}
import utils.Profiler.printTime

object DirectionCompass extends Enumeration {
  type DirectionCompass = Value
  val NORTH, EAST, SOUTH, WEST = Value

  def right(d: DirectionCompass) = d match {
    case NORTH => EAST
    case EAST => SOUTH
    case SOUTH => WEST
    case WEST => NORTH
  }

  def left(d: DirectionCompass) = d match {
    case NORTH => WEST
    case EAST => NORTH
    case SOUTH => EAST
    case WEST => SOUTH
  }
}

case class Position(val x: Int, val y: Int, val heading: DirectionCompass) {
  def move(towards: DirectionCompass, inc: Int) = towards match {
    case NORTH => Position(x, y - inc, heading)
    case SOUTH => Position(x, y + inc, heading)
    case EAST => Position(x + inc, y, heading)
    case WEST => Position(x - inc, y, heading)
  }

  def moveToward(inc: Int) = move(heading, inc)

  def turnRight(inc: Int) = Position(x, y, (0 until inc by 90).foldLeft(heading) { case (d, _) => DirectionCompass.right(d) })

  def turnLeft(inc: Int) = Position(x, y, (0 until inc by 90).foldLeft(heading) { case (d, _) => DirectionCompass.left(d) })

  def manhattan() = {
    x.abs + y.abs
  }
}

case class PositionWaypoint(val xPos: Int, val yPos: Int, val xWaypoint: Int, val yWaypoint: Int) {
  def moveWaypoint(towards: DirectionCompass, inc: Int) = towards match {
    case NORTH => PositionWaypoint(xPos, yPos, xWaypoint, yWaypoint - inc)
    case SOUTH => PositionWaypoint(xPos, yPos, xWaypoint, yWaypoint + inc)
    case EAST => PositionWaypoint(xPos, yPos, xWaypoint + inc, yWaypoint)
    case WEST => PositionWaypoint(xPos, yPos, xWaypoint - inc, yWaypoint)
  }

  def moveToward(inc: Int) = PositionWaypoint(
    xPos + xWaypoint * inc,
    yPos + yWaypoint * inc,
    xWaypoint,
    yWaypoint
  )

  def turnWaypointRight(inc: Int) =
    (0 until inc by 90).foldLeft(this) { case (p, _) =>
      p.copy(xWaypoint = -p.yWaypoint, yWaypoint = p.xWaypoint)
    }

  def turnWaypointLeft(inc: Int) =
    (0 until inc by 90).foldLeft(this) { case (p, _) =>
      p.copy(xWaypoint = p.yWaypoint, yWaypoint = -p.xWaypoint)
    }


  def manhattan() = {
    xPos.abs + yPos.abs
  }
}

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object RainRisk extends AOCExecutor {
  val instructions = InputLoader.read("12", false)


  def part1() = {
    def move(p: Position, instruction: String) = {
      val order = instruction.head
      val inc = instruction.drop(1).toInt
      order match {
        case 'F' => p.moveToward(inc)
        case 'L' => p.turnLeft(inc)
        case 'R' => p.turnRight(inc)
        case 'N' => p.move(NORTH, inc)
        case 'S' => p.move(SOUTH, inc)
        case 'E' => p.move(EAST, inc)
        case 'W' => p.move(WEST, inc)
      }
    }

    val p = instructions.foldLeft(Position(0, 0, EAST)) { case (p, instr) =>
      move(p, instr)
    }
    println(p.manhattan())
  }


  def part2() = {
    def move(p: PositionWaypoint, instruction: String) = {
      val order = instruction.head
      val inc = instruction.drop(1).toInt
      order match {
        case 'F' => p.moveToward(inc)
        case 'L' => p.turnWaypointLeft(inc)
        case 'R' => p.turnWaypointRight(inc)
        case 'N' => p.moveWaypoint(NORTH, inc)
        case 'S' => p.moveWaypoint(SOUTH, inc)
        case 'E' => p.moveWaypoint(EAST, inc)
        case 'W' => p.moveWaypoint(WEST, inc)
      }
    }

    val p = instructions.foldLeft(PositionWaypoint(0, 0, 10, -1)) { case (p, instr) =>
      move(p, instr)
    }
    println(p.manhattan())
  }


  def main(args: Array[String]) = {
    execute
  }
}
