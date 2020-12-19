package org.triclinic.day18

import scala.util.parsing.combinator.RegexParsers

class ExprParser extends RegexParsers {
  def number: Parser[Int] = """(\d+)""".r ^^ { _.toInt }
  def operator: Parser[Any] = "+" | "*"
  def expr: Parser[Any] = number ~ opt(operator ~ expr)
}


object Day18 extends App {
  val parser = new ExprParser
  println(parser.parseAll(parser.expr, "1 + 2 * 3 + 4 * 5 + 6"))
}
