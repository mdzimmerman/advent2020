package org.triclinic.day15

import scala.annotation.tailrec

object Numbers {
  @tailrec
  def play(last: Int, seen: Map[Int, Int], turn: Int, lastTurn: Int): Int = {
    //println(s"play($last $seen $turn $lastTurn)")
    val next = if (seen.contains(last)) turn-1-seen(last) else 0
    if (turn == lastTurn)
      next
    else
      play(next, seen + (last -> (turn-1)), turn+1, lastTurn)
  }

  def play(starting: List[Int], lastTurn: Int): Int = {
    play(
      starting.last,
      starting.init.zipWithIndex.toMap.map { case (k, v) => k -> (v + 1) },
      starting.size + 1,
      lastTurn)
  }
}

object Day15 extends App {
  //println(Numbers.play(6, Map(0 -> 1, 3 -> 2), 4, 10))
  println(Numbers.play(List(0, 3, 6), 10))

  println(Numbers.play(List(0, 3, 6), 2020))

  println(Numbers.play(List(1,20,11,6,12,0), 2020))
  println(Numbers.play(List(1,20,11,6,12,0), 30000000))
}
