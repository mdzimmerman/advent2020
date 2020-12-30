package org.triclinic.day22

import org.triclinic.Utils

import scala.annotation.tailrec
import scala.collection.immutable.Queue

case class Decks(deck1: Queue[Int], deck2: Queue[Int]) {
  val winner: Int = {
    if (deck2.isEmpty)
      1
    else if (deck1.isEmpty)
      2
    else
      0
  }

  private def scoreDeck(q: Queue[Int]): Int = {
    val ncards = q.length
    q.zipWithIndex.map{ case(n, i) =>
      val out = (ncards-i)*n
      //println(s"${ncards-i} * $n = $out")
      out
    }.sum
  }

  val score: Int = {
    winner match {
      case 1 => scoreDeck(deck1)
      case 2 => scoreDeck(deck2)
      case _ => 0
    }
  }

  def next: Decks = {
    val (t1, rest1) = deck1.dequeue
    val (t2, rest2) = deck2.dequeue
    if (t1 > t2)
      Decks(rest1.enqueue(t1).enqueue(t2), rest2)
    else
      Decks(rest1, rest2.enqueue(t2).enqueue(t1))
  }

  @tailrec
  private def playGame(input: Decks): Decks = {
    if (input.winner > 0) {
      input
    } else {
      playGame(input.next)
    }
  }

  def playGame(): Decks = playGame(this)
}

object Decks {
  def apply(xs: Seq[String]): Decks = {
    val d1 :: d2 :: _ = Utils.split(xs.toList, "")
    Decks(Queue(d1.tail.map(_.toInt): _*), Queue(d2.tail.map(_.toInt): _*))
  }
}

object Day22 extends App {
  val test1 = Decks(Utils.readString(
    """|Player 1:
       |9
       |2
       |6
       |3
       |1
       |
       |Player 2:
       |5
       |8
       |4
       |7
       |10""".stripMargin))

  println(test1)
  val test1out = test1.playGame()
  println(test1out)
  println(test1out.score)
  println(test1)

  val input = Decks(Utils.readResource("/day22/input.txt"))
  println(input)
  val input1out = input.playGame()
  println(input1out)
  println(input1out.score)
}
