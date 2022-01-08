package day14

import utils.{AOCExecutor, InputLoader}
import java.math.BigInteger

import scala.annotation.tailrec

case class Mask(where: Long, value: Long) {
  def apply(x: Long): Long = {
    (~where) & x | value
  }
}

object Mask {
  def parseBinary(string: String): Long = {
    new BigInteger(string, 2).longValue
  }

  def parse(string: String): Mask = {
    Mask(
      parseBinary(string.replaceAll("0", "1").replaceAll("X", "0")),
      parseBinary(string.replaceAll("X", "0"))
    )
  }
}

case class FloatingMask(where: Long, value: Long) {
  val floatingAdd = {
    def handler(inc: Long, zeroed: Long, acc: List[Long]): List[Long] = {
      if (zeroed == 0) {
        acc
      } else if ((zeroed & inc) == 0) {
        handler(inc * 2, zeroed, acc)
      }
      else {
        handler(inc * 2, zeroed - inc, acc ::: acc.map {
          _ + inc
        })
      }
    }

    if (where == 0) {
      List()
    } else {
      handler(1, ~where, List(0))
    }
  }

  def apply(x: Long): List[Long] = {
    floatingAdd.map {
      _ + (where & (x | value))
    }
  }
}

object FloatingMask {
  def parseBinary(string: String): Long = {
    new BigInteger(string, 2).longValue
  }

  def parse(string: String): FloatingMask = {
    FloatingMask(
      parseBinary(("0".repeat(64 - string.size) + string)
        .replaceAll("0", "1")
        .replaceAll("X", "0")),
      parseBinary(string.replaceAll("X", "0"))
    )
  }
}


case class Registers(val regs: Map[Long, Long] = Map()) {
  def +(p: (Long, Long)): Registers = {
    Registers(regs + p)
  }

  def sum = regs.values.sum
}


/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object DockingData extends AOCExecutor {

  val input = InputLoader.read("14", false)

  val reMask = raw"mask = (.*)".r
  val reMem = raw"mem\[(.*)\] = (.*)".r

  def process(instructions: List[String]): Registers = {
    @tailrec
    def handler(currentMask: Mask, acc: Registers, remainInstructions: List[String]): Registers = {
      remainInstructions match {
        case Nil => acc
        case inst :: insts => inst match {
          case reMask(str) => handler(Mask.parse(str), acc, insts)
          case reMem(i, x) => handler(currentMask, acc + (i.toLong, currentMask(x.toLong)), insts)
        }
      }
    }

    handler(Mask(0, 0), Registers(), instructions)
  }

  def processFloating(instructions: List[String]): Registers = {
    @tailrec
    def handler(currentFloatingMask: FloatingMask, currentMask: Mask, acc: Registers, remainInstructions: List[String]): Registers = {
      remainInstructions match {
        case Nil => acc
        case inst :: insts => inst match {
          case reMask(str) => handler(FloatingMask.parse(str), Mask.parse(str), acc, insts)
          case reMem(i, x) => {
            val value = currentMask(x.toLong)
            handler(
              currentFloatingMask,
              currentMask,
              currentFloatingMask(i.toLong).foldLeft(acc) { case (a, j) => a + (j, x.toLong) },
              insts
            )
          }
        }
      }
    }

    handler(FloatingMask(0, 0), Mask(0, 0), Registers(), instructions)
  }


  def part1(): Unit = {
    val regs = process(input)
    println(regs.sum)
  }

  def part2(): Unit = {
    val regs = processFloating(input)
    println(regs.sum)
  }

  def main(args: Array[String]) = {
    execute
  }
}
