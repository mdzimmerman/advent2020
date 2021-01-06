package org.triclinic.day22

import org.triclinic.Utils

import scala.annotation.tailrec
import scala.collection.immutable.Queue

case class Combat(init: Decks) {
  def next(decks: Decks): Decks = {
    val (t1, rest1) = decks.deck1.dequeue
    val (t2, rest2) = decks.deck2.dequeue
    if (t1 > t2)
      Decks(rest1.enqueue(t1).enqueue(t2), rest2)
    else
      Decks(rest1, rest2.enqueue(t2).enqueue(t1))
  }

  @tailrec
  private def play(input: Decks): Decks = {
    if (input.winner > 0) {
      input
    } else {
      play(next(input))
    }
  }

  def play(): Decks = play(init)
}

case class RecursiveCombat(init: Decks) {
  def next(decks: Decks): Decks = ???

  def play(): Decks = ???
}

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

  val input = Decks(Utils.readResource("/day22/input.txt"))

  def part1(decks: Decks, s: String) = {
    println(s"-- part 1 ($s) --")
    val game = Combat(decks)
    println(decks)
    val out = game.play()
    println(out)
    println(out.score)
  }

  part1(test1, "test")
  part1(input, "input")
}
