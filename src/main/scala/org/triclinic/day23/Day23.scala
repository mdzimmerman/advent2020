package org.triclinic.day23

import scala.annotation.tailrec

case class State(cups: Vector[Int]) {
  def minus(n: Int): Int = if (n == 1) 9 else n-1

  def next(): State = {
    val curr = cups(0)
    val pickup = cups.slice(1, 4)
    val rest = cups.slice(4, cups.size)

    //println(s"curr=$curr pickup=$pickup rest=$rest")
    //println()

    var dest = minus(curr)
    while (pickup.contains(dest)) {
      dest = minus(dest)
    }
    val p = rest.indexOf(dest)
    val newcups = rest.slice(0, p+1) ++ pickup ++ rest.slice(p+1, rest.size) :+ curr
    //println(curr, pickup, rest)
    State(newcups)
  }

  def mkString: String = {
    val i = cups.indexOf(1)
    (cups.slice(i+1, cups.size) ++ cups.slice(0, i)).mkString("")
  }
}

object Day23 extends App {
  val test1 = State(Vector(3, 8, 9, 1, 2, 5, 4, 6, 7))

  val input = State(Vector(9, 6, 3, 2, 7, 5, 4, 8, 1))

  @tailrec
  private def applyN(curr: State, n: Int, i: Int): State = {
    //println(curr)
    if (i == n)
      curr
    else
      applyN(curr.next(), n, i+1)
  }

  def applyN(init: State, n: Int): State = applyN(init, n, 0)

  println(test1)
  println(applyN(test1, 10).mkString)
  println(applyN(test1, 100).mkString)

  println(applyN(input, 100).mkString)
}
