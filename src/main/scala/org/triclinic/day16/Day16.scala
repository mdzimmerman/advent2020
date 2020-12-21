package org.triclinic.day16

import org.triclinic.{AsInt, Utils}

case class Interval(low: Int, high: Int) {
  def contains(x: Int): Boolean = x >= low && x <= high
}

case class FieldRange(field: String, ranges: Vector[Interval]) {
  def contains(x: Int): Boolean = ranges.exists(_.contains(x))
}

object FieldRange {
  val pattern = """(.+): (\d+)-(\d+) or (\d+)-(\d+)""".r

  def apply(s: String): Option[FieldRange] =
    s match {
      case pattern(field, AsInt(a1), AsInt(a2), AsInt(b1), AsInt(b2)) =>
        Some(FieldRange(field, Vector(Interval(a1, a2), Interval(b1, b2))))
      case _ =>
        None
    }
}

case class Ticket(fields: Vector[Int])

object Ticket {
  val pattern = """([\d,]+)""".r

  def apply(s: String): Option[Ticket] =
    s match {
      case pattern(columns) =>
        Some(Ticket(columns.split(",").map(_.toInt).toVector))
      case _ =>
        None
    }
}

case class Notes(fieldRanges: List[FieldRange],
                 mine: Ticket,
                 nearby: List[Ticket]) {
  val allRanges = fieldRanges.flatMap(_.ranges)
  val nearbyValid = nearby.filter(ticketIsValid(_))

  def errorRate(): Int = {
    val values = nearby.map(_.fields).flatten
    values.filter{x => !allRanges.exists(_.contains(x))}.sum
  }

  def ticketIsValid(t: Ticket): Boolean = {
    t.fields.forall(x => allRanges.exists(_.contains(x)))
  }

  def identifyPotentialColumns(): Map[String, Set[Int]] = {
    val pot = fieldRanges.map{ f =>
      f.field -> mine.fields.indices.filter{ i =>
        nearbyValid.map(_.fields(i)).forall(f.contains)
      }.toSet
    }.toMap
    pot
  }

  def solve(pot: Map[String, Set[Int]],
            out: Map[String, Int]): Map[String, Int] = {
    val size1 = pot.filter { case (_, s) => s.size == 1 }
    if (size1.isEmpty) {
      out
    } else {
      val curr = pot.filter { case (_, s) => s.size == 1 }.head
      val currField = curr._1
      val currCol = curr._2.head
      val potNew = (pot - currField).map {
        case (f, s) => f -> (s - currCol)
      }
      val outNew = out + (currField -> currCol)
      solve(potNew, outNew)
    }
  }

  def solve(): Map[String, Int] = {
    val pot = identifyPotentialColumns()
    solve(pot, Map())
  }
}

object Notes {
  def apply(xs: Seq[String]): Notes = {
    val fieldRanges = xs.flatMap(FieldRange(_)).toList
    val tickets = xs.flatMap(Ticket(_)).toList
    Notes(fieldRanges, tickets.head, tickets.tail)
  }
}

object Day16 extends App {
  val test1 = Notes(Utils.readResource("/day16/test1.txt"))
  println(test1)
  println(test1.errorRate())
  println(test1.nearbyValid)

  val input = Notes(Utils.readResource("/day16/input.txt"))
  println(input.errorRate())
  val cols = input.solve()
  println(cols.filter{ case(k, _) =>
    k.startsWith("departure")
  }.values.map(input.mine.fields(_)).map(_.toLong).product)
}
