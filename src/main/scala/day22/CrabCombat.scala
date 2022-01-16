package day22

import utils.{AOCExecutor, InputLoader}

import scala.annotation.tailrec

case class Players(
                    val player1: List[Int],
                    val player2: List[Int]
                  ) {
  def winnerDeck: Option[List[Int]] =
    if (player1.isEmpty)
      Some(player2)
    else if (player2.isEmpty)
      Some(player1)
    else None

  def winner: Option[Int] =
    if (player1.isEmpty)
      Some(2)
    else if (player2.isEmpty)
      Some(1)
    else None

  def play = if (player1.head > player2.head)
    Players(player1.drop(1) :+ player1.head :+ player2.head, player2.drop(1))
  else
    Players(player1.drop(1), player2.drop(1) :+ player2.head :+ player1.head)

  override def toString: String = s"""#1: ${player1.mkString(",")}\n#2: ${player2.mkString(",")}"""
}

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object CrabCombat extends AOCExecutor {

  val decks = InputLoader.read("22", false)
    .mkString("\n").split("\n\n").map {
    _.split("\n")
  }.map {
    _.drop(1)
  }.map {
    _.map {
      _.toInt
    }
  }
  val playersInit = Players(decks(0).toList, decks(1).toList)

  def part1(): Unit = {
    def handler(players: Players): List[Int] = players.winnerDeck match {
      case Some(xs) => xs
      case None => {
        //println(players)
        handler(players.play)
      }
    }

    val winningDeck = handler(playersInit)
    val n = winningDeck.reverse.zipWithIndex.map { case (card, i) => card * (i + 1) }.sum
    println(n)
  }

  // return 1 or 2 for the winner
  def playGame(start: Players): (Int, List[Int]) = {
    @tailrec
    def rounds(players: Players, history: Set[Players]): (Int, List[Int]) = {
      //println(players)
      if (history.contains(players)) {
        return (1, players.player1)
      }
      if (players.winner.isDefined) {
        return (players.winner.get, players.winnerDeck.get)
      }
      val card1 = players.player1.head
      val card2 = players.player2.head
      val nbCard1 = players.player1.size - 1
      val nbCard2 = players.player2.size - 1

      val roundWinner = if ((nbCard1 >= card1) && (nbCard2 >= card2)) {
        //println("------------------------ recursive game")
        val subWinner = playGame(Players(players.player1.drop(1).take(card1), players.player2.drop(1).take(card2)))
        //println(s"---------------------- back with winner ${subWinner._1}")
        subWinner._1
      } else {
        if (card1 > card2)
          1
        else
          2
      }
      //println(s"roundWinner = $roundWinner")
      if (roundWinner == 1) {
        rounds(Players(players.player1.drop(1) :+ card1 :+ card2, players.player2.drop(1)), history + players)
      } else {
        rounds(Players(players.player1.drop(1), players.player2.drop(1) :+ card2 :+ card1), history + players)
      }
    }

    rounds(start, Set())
  }

  def part2(): Unit = {
    val winningDeck = playGame(playersInit)._2
    val n = winningDeck.reverse.zipWithIndex.map { case (card, i) => card * (i + 1) }.sum
    println(n)
  }

  def main(args: Array[String]) = {
    execute
  }
}
