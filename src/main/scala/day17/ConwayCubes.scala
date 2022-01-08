package day17

import utils.{AOCExecutor, InputLoader}

import scala.collection.immutable.BitSet


/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object ConwayCubes extends AOCExecutor {

  val input = InputLoader.read("17", true)
  val cube = Cube.parse2D(input)

  def part1(): Unit = {
    val endCube = (1 until 7).foldLeft(cube) { case (acc, _) =>
      val c = acc.grow.conway
      println(c)
      c
    }
    println(endCube.bitSet.count{_ => true})
  }

  def part2(): Unit = {

  }

  def main(args: Array[String]) = {
    execute
  }
}

case class Cube(val xSize: Int, val ySize: Int, val zSize: Int, val bitSet: BitSet) {
  def +(x: Int, y: Int, z: Int) = Cube(xSize, ySize, zSize, bitSet + (x + y * xSize + z * xSize * ySize))

  /*  ..#.
      .#.#
      .#..

      ####
      ..#.
      #..#

      => 2,5,7,9,12,13,14,15,18,20,23
   */

  def conway = {
    val neighb = neighbors.flatMap { c => c.bitSet.toList }.groupBy(identity).view.mapValues(_.size).toList
    copy(
      bitSet = bitSet ++
        neighb.filter { case (_, count) => count == 3 }.map {
          _._1
        } --
        neighb.filter { case (_, count) => count > 3 | count < 2 }.map {
          _._1
        }
    )

  }

  def xRight = copy(
    bitSet = bitSet.filter {
      _ % xSize != 0
    }.map {
      _ - 1
    }
  )

  def xLeft = copy(
    bitSet = bitSet.filter { v =>
      v % xSize != (xSize - 1)
    }.map {
      _ + 1
    }
  )

  def yRight = copy(
    bitSet = bitSet.filter { v =>
      (v / xSize) % ySize != 0
    }.map {
      _ - xSize
    }
  )

  def yLeft = copy(
    bitSet = bitSet.filter { v =>
      (v / xSize) % ySize != (ySize - 1)
    }.map {
      _ + xSize
    }
  )

  def zRight = copy(
    bitSet = bitSet.filter { v =>
      (v / (xSize * ySize)) % zSize != 0
    }.map {
      _ - (xSize * ySize)
    }
  )

  def zLeft = copy(
    bitSet = bitSet.filter { v =>
      (v / (xSize * ySize)) % zSize != (zSize - 1)
    }.map {
      _ + (xSize * ySize)
    }
  )

  def position(v: Int) = (
    v % xSize,
    (v / xSize) % ySize,
    v / (xSize * ySize)
  )

  def bit(x: Int, y: Int, z: Int) =
    x + y * xSize + z * xSize * ySize

  def grow = growX.growY.growZ

  def growX = copy(
    xSize = xSize + 2,
    bitSet = bitSet.map { v =>
      val (x, y, z) = position(v)
      x + 1 + y * (xSize + 2) + z * (xSize + 2) * ySize
    }
  )

  def growY = copy(
    ySize = ySize + 2,
    bitSet = bitSet.map { v =>
      val (x, y, z) = position(v)
      x + (y + 1) * xSize + z * xSize * (ySize + 2)
    }
  )

  def growZ = copy(
    zSize = zSize + 2,
    bitSet = bitSet.map {
      _ + xSize * ySize
    }
  )

  def neighbors: List[Cube] =
    List(
      xLeft,
      xRight,
      yLeft,
      yRight,
      zLeft,
      zRight,
      xLeft.yLeft,
      xLeft.yRight,
      xRight.yLeft,
      xRight.yRight,
      xLeft.zLeft,
      xLeft.zRight,
      xRight.zLeft,
      xRight.zRight,
      yLeft.zLeft,
      yLeft.zRight,
      yRight.zLeft,
      yRight.zRight,
      xLeft.yLeft.zLeft,
      xLeft.yLeft.zRight,
      xLeft.yRight.zLeft,
      xLeft.yRight.zRight,
      xRight.yLeft.zLeft,
      xRight.yLeft.zRight,
      xRight.yRight.zLeft,
      xRight.yRight.zRight,
    )

  override def toString: String = {
    val bm = bitSet.toBitMask
    val bmStr = bm.map { l =>
      val s = l.toBinaryString
      "0".repeat(64 - s.size) + s
    }.reverse.mkString("").takeRight(xSize * ySize * zSize)
      .replaceAll("0", ".")
      .replaceAll("1", "#")
      .reverse
      .sliding(xSize * ySize, xSize * ySize)
      .map { b => b.sliding(xSize, xSize).mkString("\n") }
      .mkString("\n\n")

    bmStr
  }

}


object Cube {
  def parse2D(input: Seq[String]) = {
    val ySize = input.size
    val xSize = input.head.size
    Cube(
      xSize,
      ySize,
      1,
      BitSet() ++
        input.mkString("").zipWithIndex.filter {
          _._1 == '#'
        }.map {
          _._2
        }
    )
  }


  def build(xSize: Int, ySize: Int, zSize: Int, set: List[(Int, Int, Int)]): Cube = {
    set.foldLeft(Cube(xSize, ySize, zSize, BitSet())) { case (acc, t) =>
      acc.copy(bitSet = acc.bitSet + (t._1 + t._2 * xSize + t._3 * xSize * ySize))
    }
  }
}
