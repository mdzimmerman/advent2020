package org.triclinic.day05

import org.triclinic.Utils

import scala.annotation.tailrec


case class Pass(value: String) {
  val rowString: String = value.substring(0, 7)
  val colString: String = value.substring(7)

  val row: Int = binaryParse(0, 127, rowString)
  val col: Int = binaryParse(0, 7, colString)

  val seatId: Int = row * 8 + col

  @tailrec
  private def binaryParse(start: Int, end: Int, s: String): Int = {
    if (s == "") {
      start
    } else {
      val mid = (start + end) / 2
      val (nstart, nend) = s.head match {
        case 'F' | 'L' =>
          (start, mid)
        case 'B' | 'R' =>
          (mid+1, end)
        case _ =>
          throw new RuntimeException("bad char")
      }
      binaryParse(nstart, nend, s.tail)
    }
  }

  override def toString: String =
    s"Pass(value=$value row=$row col=$col seatId=$seatId)"
}

object Day5 extends App {
  val test = List(
    "FBFBBFFRLR",
    "BFFFBBFRRR",
    "FFFBBBFRRR",
    "BBFFBBFRLL"
  ).map(Pass(_))
  for (t <- test)
    println(t)

  val input = Utils.readResource("/day05/input.txt").map(Pass(_)).toList
  //for (i <- input)
  //  println(i)
  val seatIds = input.map(_.seatId).toSet
  val min = seatIds.min
  val max = seatIds.max
  println(s"min=$min max=$max")
  for (i <- min+1 to max-1) {
    if (!seatIds.contains(i) && seatIds.contains(i-1) && seatIds.contains(i+1))
      println(s"mine=$i")
  }
}
