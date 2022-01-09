package day17

import utils.{AOCExecutor, InputLoader}

import scala.collection.immutable.BitSet


/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object ConwayCubes extends AOCExecutor {

  val input = InputLoader.read("17", false)

  def part1(): Unit = {
    val universe = CubeUniverse.parse2D(input, 3)

    val target = (1 to 6).foldLeft(universe) { case (acc, _) => acc.conway }
    println(target.size)
  }

  def part2(): Unit = {
    val universe = CubeUniverse.parse2D(input, 4)

    val target = (1 to 6).foldLeft(universe) { case (acc, _) => acc.conway }
    println(target.size)
  }

  def main(args: Array[String]) = {
    execute
  }
}


case class Cube(coordinates: List[Int]) {
  val dim = coordinates.size

  def propagate = {
    def handler(d: Int, acc: List[Cube]): List[Cube] = {
      if (d == dim) {
        return acc
      }
      handler(
        d + 1,
        (-1 to 1).flatMap { i =>
          acc.map { c =>
            c.copy(coordinates = (c.coordinates.take(d) :+ (c.coordinates(d) + i)) ++ coordinates.drop(d + 1))
          }
        }.toList
      )
    }

    handler(0, List(this)).filter {
      _ != this
    }
  }
}

case class CubeUniverse(
                         val cubes: List[Cube]
                       ) {
  def size = cubes.size

  def conway = {
    val countCubes = cubes.flatMap {
      _.propagate
    }
      .groupBy(identity)
      .map { case (c, cs) => c -> cs.size }
      .toMap

    CubeUniverse(
      (cubes.filter { c =>
        val n = countCubes.getOrElse(c, 0)
        n >= 2 & n <= 3
      } ::: countCubes.filter {
        _._2 == 3
      }.map {
        _._1
      }.toList).distinct
    )
  }
}


object CubeUniverse {
  def parse2D(input: Seq[String], dim: Int) = {
    CubeUniverse(
      input.zipWithIndex
        .flatMap { case (line, y) =>
          line.zipWithIndex.filter { case (c, _) => c == '#' }
            .map { case (_, x) => Cube(List(x, y) ++ List.fill(dim - 2)(0)) }
        }.toList
    )
  }


}
