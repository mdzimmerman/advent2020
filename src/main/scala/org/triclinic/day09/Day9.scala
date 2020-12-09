package org.triclinic.day09

import org.triclinic.Utils

case class XMAS(numbers: Vector[Long],
                preamble: Int) {
  val length: Int = numbers.length

  def findFirstInvalid(): Long = {
    for (i <- preamble until length ) {
      val valid = numbers.slice(i - preamble, i).combinations(2).map(_.sum).toSet
      //println(s"$i => $valid")
      if (!valid.contains(numbers(i)))
        return numbers(i)
    }
    -1
  }

  case class Range(low: Long, high: Long, sum: Long)

  def findWeaknessRange(invalid: Long): Range = {
    for (n <- 2 to length;
        i <- 0 to length-n) {
      val range = numbers.slice(i, i+n)
      //println(s"$range")
      if (range.sum == invalid) {
        val low = range.min
        val high = range.max
        return Range(low, high, low+high)
      }
    }
    throw new Exception("range not found")
  }
}

object XMAS {
}

object Day9 extends App {
  println("--- test ---")
  val test1 = XMAS(Utils.readString(
    """
      |35
      |20
      |15
      |25
      |47
      |40
      |62
      |55
      |65
      |95
      |102
      |117
      |150
      |182
      |127
      |219
      |299
      |277
      |309
      |576
    """.stripMargin).map(_.trim).filter(_.nonEmpty).map(_.toLong).toVector, 5)
  println(test1)
  println("part 1")
  println(test1.findFirstInvalid())
  println("part 2")
  println(test1.findWeaknessRange(127))

  val input = XMAS(Utils.readResource("/day09/input.txt").map(_.toLong).toVector, 25)
  //println(input)
  println(s"length = ${input.length}")
  println("part 1")
  val invalid = input.findFirstInvalid()
  println(invalid)
  println("part 2")
  println(input.findWeaknessRange(invalid))
}
