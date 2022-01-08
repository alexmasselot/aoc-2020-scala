package day11

import utils.Direction.{DOWN, LEFT, RIGHT, UP}
import utils.Profiler.printTime
import utils.{AOCExecutor, Direction, InputLoader, Matrix}

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object SeatingSystem extends AOCExecutor {

  val input = InputLoader.read("11", false)
  val plan = Matrix.buildChar(input)

  def part1() = {
    def iterate(p: Matrix[Char]): Matrix[Char] =
      p.zip(p.neighbors8()).map { case (x, neighb) =>
        x match {
          case '.' => '.'
          case '#' => if (neighb.count(_ == '#') >= 4) 'L' else '#'
          case 'L' => if (neighb.count(_ == '#') == 0) '#' else 'L'
        }
      }

    def handler(p: Matrix[Char]): Int = {
      val p2 = iterate(p)
      if (p2 == p) {
        return p.count(_ == '#')
      }
      handler(p2)
    }

    val n = handler(plan)

    println(n)

  }

  def part2() = {
    def countVisible(plan: Matrix[Char]): Matrix[Int] = {


      def op(a: Char, b: Char) = if (b == '.') a else b

      def visibleStraights = List(LEFT, RIGHT, UP, DOWN).map { dir =>
        plan.scan(dir, 'L')(op)
          .shift(dir, 'L')
      }

      def visibleDiags = List(UP, DOWN).flatMap { dirVert =>
        List(LEFT, RIGHT).map { dirHoriz =>
          plan.scanDiag(dirVert, dirHoriz, 'L')(op)
            .shift(dirVert, 'L')
            .shift(dirHoriz, 'L')
        }
      }

      Matrix.fill(plan.nRows, plan.nCols)('.').zipN((visibleStraights :++ visibleDiags): _*)
        .map { xs =>
          xs.count {
            _ == '#'
          }
        }
    }

    def iterate(p: Matrix[Char]): Matrix[Char] = {
      p.zip(countVisible(p)).map { case (x, neighb) =>
        x match {
          case '.' => '.'
          case '#' => if (neighb >= 5) 'L' else '#'
          case 'L' => if (neighb == 0) '#' else 'L'
        }
      }
    }


    def handler(p: Matrix[Char]): Int = {
      val p2 = iterate(p)
      if (p2 == p) {
        return p.count(_ == '#')
      }
      handler(p2)
    }

    val n = handler(plan)

    println(n)
  }

  def main(args: Array[String]) = {
    execute
  }
}
