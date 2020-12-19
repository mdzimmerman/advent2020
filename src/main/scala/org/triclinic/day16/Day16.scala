package org.triclinic.day16

import org.triclinic.{AsInt, Utils}

case class FieldRange(field: String, ranges: List[Range])

object FieldRange {
  val pattern = """(.+): (\d+)-(\d+) or (\d+)-(\d+)""".r

  def apply(s: String): Option[FieldRange] =
    s match {
      case pattern(field, AsInt(a1), AsInt(a2), AsInt(b1), AsInt(b2)) =>
        Some(FieldRange(field, List(Range(a1, a2), Range(b1, b2))))
      case _ =>
        None
    }
}

object Day16 extends App {
  val test1 = Utils.readResource("/day16/test1.txt").toList
  println(test1.flatMap(FieldRange(_)))
}
