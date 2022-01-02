package day07

import scalax.collection.edge.WDiEdge
import scalax.collection.immutable.Graph
import utils.InputLoader

import scala.language.postfixOps

case class Bag(
                val tone: String,
                val color: String
              ) {
  override def toString() = s"$tone $color"
}

object BagGraphParser {
  val reLine = raw"(\w+) (\w+) bags contain (.+)\.".r
  val reLineNone = raw"(\w+) (\w+) bags contain no other bags\.".r
  val reContained = raw"(\d+) (\w+) (\w+) bag".r

  def parse(input: List[String]): Graph[Bag, WDiEdge] = {
    val edges = input.flatMap(_ match {
      case reLine(tone, color, contained) => reContained.findAllIn(contained).map(
        _ match {
          case reContained(i, toneTarget, colorTarget) => WDiEdge(Bag(tone = tone, color = color), Bag(toneTarget, colorTarget))(i.toInt)
          case _ => throw new UnsupportedOperationException()
        }
      )
      case x => throw new UnsupportedOperationException(x)
    })
    Graph.empty[Bag, WDiEdge] ++ edges
  }
}


/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object HandyHaversacks {

  def part1(graph: Graph[Bag, WDiEdge]) = {
    def n(bag: Bag): graph.NodeT = graph get bag

    def handler(bag: Bag): Set[Bag] = {
      Set(bag) ++ (n(bag) diPredecessors).flatMap { succ => handler(succ.value) }
    }

    val count = handler(Bag("shiny", "gold")) - Bag("shiny", "gold")
    println(count.size)


  }


  def part2(graph: Graph[Bag, WDiEdge]) = {
    def n(bag: Bag): graph.NodeT = graph get bag

    def handler(bag: Bag): Int = {
      val countDesc = 1 + n(bag).outgoing.toList.map { e =>
        e.weight.toInt * handler(e.to)
      }.sum

      countDesc
    }


    val count = handler(Bag("shiny", "gold")) - 1
    //val count = handler(Bag("striped", "yellow")) - 1
    //val count = handler(Bag("dark", "indigo")) - 1

    println(count)
  }

  def main(args: Array[String]) = {
    val input = InputLoader.read("07", false)
    val graph = BagGraphParser.parse(input)

    part1(graph)
    part2(graph)
  }
}
