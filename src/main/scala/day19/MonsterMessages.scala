package day19

import scalaz.Memo
import utils.{AOCExecutor, InputLoader}


abstract class CheckNode {

  val isValid: String => Boolean = Memo.immutableHashMapMemo {
    str => toRegexp.r.matches(str)
  }

  def toRegexp: String
}


case class CheckNodeValue(
                           val value: Char
                         ) extends CheckNode {

  def toRegexp: String = s"$value"
}


case class CheckNodePair(
                          val checkerA: CheckNode,
                          val checkerB: CheckNode
                        ) extends CheckNode {


  def toRegexp: String = s"${checkerA.toRegexp}${checkerB.toRegexp}"
}

object CheckNodePair {
  def build(checkers: List[CheckNode]): CheckNodePair = {
    checkers match {
      case c1 :: c2 :: Nil => CheckNodePair(c1, c2)
      case c1 :: cs => CheckNodePair(c1, build(cs))
      case _ => throw new UnsupportedOperationException(s"Cannot parse [$checkers]")
    }
  }
}

case class CheckNodeOr(
                        val checkerA: CheckNode,
                        val checkerB: CheckNode,
                      ) extends CheckNode {

  def toRegexp: String = s"(?:${checkerA.toRegexp}|${checkerB.toRegexp})"
}

case class CheckGraph(val root: CheckNode) {
  def isValid(string: String): Boolean = {
    root.isValid(string)
  }
}

object CheckGraph {
  val reValue = raw"""(\d+): "([ab])"""".r
  val reList = raw"""(\d+): ([\d ]+)""".r
  val reOr = raw"""(\d+): ([\d ]+) \| ([\d ]+)""".r
  val reJustColon = raw"""(\d+): (.*)""".r
  val reNumber = raw"""(\d+)""".r

  def parse(input: Seq[String]): Map[Int, CheckNode] = {
    def removeLine(remain: Seq[String], line: String) = remain.filter {
      _ != line
    }

    def nodeList(xs: String, indexed: Map[Int, CheckNode]): CheckNode = {
      xs.split(" ").map { x =>
        indexed(x.toInt)
      }.toList match {
        case c :: Nil => c
        case cs => CheckNodePair.build(cs)
      }

    }

    def handler(remain: Seq[String], indexed: Map[Int, CheckNode]): Map[Int, CheckNode] = {
      if (remain.isEmpty) return indexed
      remain.find { line =>
        line match {
          case reValue(_, _) => true
          case reJustColon(index, right) => reNumber.findAllIn(right).map {
            _.toInt
          }.forall { i =>
            indexed.contains(i)
          }
          case _ => throw new UnsupportedOperationException(s"cannot parse [$line]")
        }
      } match {
        case Some(line) => line match {
          case reValue(i, v) => handler(
            removeLine(remain, line),
            indexed + (i.toInt -> CheckNodeValue(v.head))
          )
          case reList(i, xs) => handler(
            removeLine(remain, line),
            indexed + (i.toInt -> nodeList(xs, indexed))
          )
          case reOr(i, xs, ys) => handler(
            removeLine(remain, line),
            indexed + (i.toInt -> CheckNodeOr(nodeList(xs, indexed), nodeList(ys, indexed)))
          )
        }
        case None =>
          throw new UnsupportedOperationException(s"Doomed with $remain /${indexed.keySet.toList.sorted}")
      }
      //
    }

    handler(input, Map())
  }
}

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object MonsterMessages extends AOCExecutor {

  val input = InputLoader.read("19", false)
  val graph = CheckGraph.parse(input.takeWhile(_ != ""))
  val messages = input.dropWhile(_ != "").drop(1)


  def part1(): Unit = {
    val re = graph(0).toRegexp.r
    val n = messages.count { message =>
      val ok = re.matches(message)
      if (ok) println(message)
      ok
    }
    println(s"$n valid messages")
  }

  def part2(): Unit = {
    val res = (0 until 20).map { i => s"${graph(42).toRegexp}{${2 + i},}${graph(31).toRegexp}{1,${1 + i}}".r }


    val n = messages.count { message =>
      val ok = res.exists {
        _.matches(message)
      }
      if (ok) println(message)
      ok
    }
    println(s"$n valid messages")
  }

  def main(args: Array[String]) = {
    execute
  }
}
