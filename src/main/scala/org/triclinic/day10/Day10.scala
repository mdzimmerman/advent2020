package org.triclinic.day10

import org.triclinic.Utils

object Day10 extends App {
  val test1 = Utils.readString(
    """
      |16
      |10
      |15
      |5
      |1
      |11
      |7
      |19
      |6
      |12
      |4
      |""".stripMargin).filter(_.nonEmpty).map(_.toInt).toList

  val input = Utils.readResource("/day10/input.txt").filter(_.nonEmpty).map(_.toInt).toList

  case class Output(ones: Int, threes: Int, out: Int)

  def calcDiffs(adapters: List[Int]) = {
    val diffs = (0 :: adapters).sorted.toVector.sliding(2).map(x => x(1) - x(0)).toList
    val dist = diffs.groupBy(identity).mapValues(_.size)
    val ones = dist(1)
    val threes = dist(3)+1
    Output(ones, threes, ones*threes)
  }

  println(calcDiffs(test1))
  println(calcDiffs(input))
}

