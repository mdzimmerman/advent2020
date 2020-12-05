package org.triclinic.day01

import org.triclinic.Utils

object Day1 extends App {
  def find(v: Vector[Int], setSize: Int = 2): Int = {
    for (c <- v.combinations(setSize))
      if (c.sum == 2020)
        return c.product
    -1
  }

  val test1 =
    """
      |1721
      |979
      |366
      |299
      |675
      |1456
  """.stripMargin.split("""\n""").map(_.trim).filter(_.nonEmpty).map(_.toInt).toVector

  val input = Utils.readResource("/day01/input.txt").map(_.trim).filter(_.nonEmpty).map(_.toInt).toVector

  println(test1)
  println(find(test1))
  println(find(test1, 3))

  println(input)
  println(find(input))
  println(find(input, 3))
}