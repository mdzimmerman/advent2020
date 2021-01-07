package org.triclinic.day22

import org.triclinic.Utils

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable

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
  private def play(input: Decks): Winner = {
    if (input.deck2.isEmpty)
      Winner(1, input)
    else if (input.deck1.isEmpty)
      Winner(2, input)
    else
      play(next(input))
  }

  def play(): Winner = play(init)
}

case class RecursiveCombat(init: Decks) {
  val seen = mutable.HashSet[Decks]()

  def next(input: Decks): Decks = {
    //println(input)
    val (t1, rest1) = input.deck1.dequeue
    val (t2, rest2) = input.deck2.dequeue
    val winner =
      if (t1 <= rest1.length && t2 <= rest2.length) {
        val decksNew = Decks(rest1.take(t1), rest2.take(t2))
        RecursiveCombat(decksNew).play().winner
      } else if (t1 > t2) 1 else 2
    winner match {
      case 1 => Decks(rest1.enqueue(t1).enqueue(t2), rest2)
      case 2 => Decks(rest1, rest2.enqueue(t2).enqueue(t1))
    }
  }

  @tailrec
  private def play(input: Decks): Winner = {
    if (seen.contains(input))
      Winner(1, input)
    else if (input.deck2.isEmpty)
      Winner(1, input)
    else if (input.deck1.isEmpty)
      Winner(2, input)
    else {
      seen += input
      play(next(input))
    }
  }

  def play(): Winner = play(init)
}

case class Winner(winner: Int, decks: Decks) {
  val score: Int = {
    winner match {
      case 1 => decks.scoreDeck(decks.deck1)
      case 2 => decks.scoreDeck(decks.deck2)
      case _ => 0
    }
  }
}

case class Decks(deck1: Queue[Int], deck2: Queue[Int]) {
  def scoreDeck(q: Queue[Int]): Int = {
    val ncards = q.length
    q.zipWithIndex.map{ case(n, i) =>
      val out = (ncards-i)*n
      //println(s"${ncards-i} * $n = $out")
      out
    }.sum
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

  def part2(decks: Decks, s: String) = {
    println(s"-- part 2 ($s) --")
    val game = RecursiveCombat(decks)
    println(decks)
    val out = game.play()
    println(out)
    println(out.score)
  }

  part1(test1, "test")
  part1(input, "input")

  part2(test1, "test")
  part2(input, "input")
}
