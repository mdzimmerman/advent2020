package org.triclinic.day13

import org.triclinic.{AsInt, Utils}

case class Schedule(start: Int, buses: Vector[Option[Int]]) {
  case class Output(bus: Int, waittime: Int, product: Int)

  val remainders: Vector[(Long, Long)] = buses.zipWithIndex.flatMap{
    case (Some(n), a) => Some(n.toLong -> a.toLong)
    case (None, _) => None
  }.sorted.reverse

  def part1(): Output = {
    val (waittime, bus) = buses.flatten.map(n => (n-start%n, n)).min
    Output(bus, waittime, waittime*bus)
  }

  def part2(): Long = {
    val ns = remainders.map(_._1).toList
    val is = remainders.map(_._2).toList
    val as = remainders.map(r => r._1 - r._2).toList
    val as2 = remainders.map{ case(n,i) =>
      val r = (n - i) % n
      if (r < 0)
        r + n
      else
        r

    }.toList
    println(ns)
    println(is)
    println(as)
    println(as2)

    //return 0

    var x = as2(0)
    for (i <- 0 until ns.size-1) {
      val n = (0 to i).map(ns(_)).product
      while ((x % ns(i+1)) != as2(i+1)) {
        println(s"x=$x n=$n ni=$i rem=${x % ns(i+1)}")
        x += n
      }
    }
    x
  }
}

object Schedule {
  def apply(inp: IndexedSeq[String]): Schedule = {
    val start = inp(0).toInt
    val buses = inp(1).split(",").map {
      case AsInt(n) => Some(n)
      case _ => None
    }.toVector
    Schedule(start, buses)
  }
}

object Day13 extends App {
  val test1 = Schedule(
    Utils.readString(
      """
        |939
        |7,13,x,x,59,x,31,19
      """.stripMargin).filter(_.nonEmpty).toVector)
  println(test1)
  println(test1.part1)
  println(test1.part2)

  val input = Schedule(Utils.readResource("/day13/input.txt").toVector)
  println(input)
  println(input.part1)
  println(input.part2)
}
