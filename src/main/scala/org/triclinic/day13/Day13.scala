package org.triclinic.day13

import org.triclinic.{AsInt, Utils}

case class Schedule(start: Int, buses: Vector[Option[Int]]) {
  case class Output(bus: Int, waittime: Int, product: Int)

  def part1(): Output = {
    val (waittime, bus) = buses.flatten.map(n => (n-start%n, n)).min
    Output(bus, waittime, waittime*bus)
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

  val input = Schedule(Utils.readResource("/day13/input.txt").toVector)
  println(input)
  println(input.part1)
}
