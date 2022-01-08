package day16

import utils.{AOCExecutor, InputLoader}


/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object TicketTranslation extends AOCExecutor {

  val input = InputLoader.read("16", false)
  val reField = raw"(.*): (\d+)-(\d+) or (\d+)-(\d+)".r
  val fields: Fields = Fields(input.takeWhile(_ != "").map {
    _ match {
      case reField(name, i0, i1, j0, j1) => Field(name, (i0.toInt to i1.toInt, j0.toInt to j1.toInt))
    }
  })
  val myTicket = Ticket(input.dropWhile(_ != "your ticket:")(1).split(",").map {
    _.toInt
  }.toList)
  val nearbyTickets: List[Ticket] = input.dropWhile(_ != "nearby tickets:").drop(1).map { l =>
    Ticket(
      l.split(",").map {
        _.toInt
      }.toList
    )
  }

  def part1(): Unit = {
    val invalidTicketValues = nearbyTickets.flatMap { t => fields.invalidTicketValues(t) }
    println(invalidTicketValues.sum)
  }

  def explainFields(iValueToFields: Map[Int, List[Field]]): Map[Int, Field] = {
    def handler(remain: Map[Int, List[Field]], acc: Map[Int, Field]): Map[Int, Field] = {
      if (remain.isEmpty) {
        return acc
      }
      remain.find { case (i, fs) => fs.size == 1 } match {
        case Some((i, fs)) => {
          val f = fs.head
          handler(remain.map { case (i, fs) => (i, fs.filter {
            _ != f
          })
          }.removed(i), acc + (i -> f))
        }
        case _ => {
          println(s"ARGH $remain")
          ???
        }
      }
    }

    handler(iValueToFields, Map())
  }

  def part2(): Unit = {
    val validTickets = nearbyTickets.filter { t => fields.invalidTicketValues(t).isEmpty }
    val possibleFields = (0 until validTickets.head.size).map { iField =>
      val values = validTickets.map {
        _.values(iField)
      }
      iField -> fields.possibleFields(values)
    }.toMap
    val explained = explainFields(possibleFields)
    val iDepartures = explained.filter { case (_, f) => f.name.startsWith("departure") }.map { case (i, _) => i }
    println(iDepartures.map { i => myTicket.values(i) }.foldLeft(1L) { case (acc, v) => acc * v })

  }

  def main(args: Array[String]) = {
    execute
  }
}

case class Ticket(
                   val values: List[Int]
                 ) {
  def size = values.size
}

case class Field(
                  val name: String,
                  val range: (Range.Inclusive, Range.Inclusive)
                ) {
  def isBelonging(x: Int) = range._1.contains(x) || range._2.contains(x)

  def areBelonging(xs: List[Int]) = xs.forall {
    isBelonging(_)
  }
}

case class Fields(
                   fields: List[Field]
                 ) {
  def invalidTicketValues(t: Ticket) =
    t.values.filter { v =>
      fields.forall {
        !_.isBelonging(v)
      }
    }

  def possibleFields(ticketValues: List[Int]): List[Field] =
    fields.filter {
      _.areBelonging(ticketValues)
    }
}

