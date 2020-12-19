package org.triclinic.day18

import org.triclinic.Utils

import scala.util.parsing.combinator.RegexParsers

sealed trait Value {
  def eval(): Long
}
case class Num(n: Long) extends Value {
  def eval(): Long = n
}
case class Bin(v1: Value, op: String, v2: Value) extends Value {
  def eval(): Long = op match {
    case "*" => v1.eval * v2.eval
    case "+" => v1.eval + v2.eval
  }
}

trait BaseParser extends RegexParsers {
  def number: Parser[Value]    = """(\d+)""".r ^^ { x => Num(x.toLong) }
  def operator: Parser[String] = add | mult
  def add: Parser[String]      = "+"
  def mult: Parser[String]     = "*"

  def parseExpr(base: Parser[Value], s: String): Option[Value] = {
    parse(base, s) match {
      case Success(matched,_) =>
        Some(matched)
      case Failure(msg,_) =>
        println(s"FAILURE: $msg")
        None
      case Error(msg,_) =>
        println(s"ERROR: $msg")
        None
    }
  }

  def parseExpr(s: String): Option[Value]
}

class CrazyParser extends BaseParser {
  def expr: Parser[Value] = {
    factor ~ rep(operator ~ factor) ^^ { case head ~ tail =>
      tail.foldLeft(head){ case (acc, op~v) => Bin(acc, op, v) }
    }
  }

  def factor: Parser[Value] = "(" ~> expr <~ ")" | number

  def parseExpr(s: String): Option[Value] = parseExpr(expr, s)
}

class ReallyCrazyParser extends BaseParser {
  def expr: Parser[Value] =
    term ~ rep(mult ~ term) ^^ { case head ~ tail =>
      tail.foldLeft(head) { case (acc, op ~ v) => Bin(acc, op, v) }
    }

  def term: Parser[Value] =
    factor ~ rep(add ~ factor) ^^ { case head ~ tail =>
      tail.foldLeft(head) { case (acc, op ~ v) => Bin(acc, op, v) }
    }

  def factor: Parser[Value] = "(" ~> expr <~ ")" | number

  def parseExpr(s: String): Option[Value] = parseExpr(expr, s)
}

object Day18 extends App {
  val parser1 = new CrazyParser
  val parser2 = new ReallyCrazyParser

  def testParser(parser: BaseParser, s: String): Unit = {
    println(s)
    parser.parseExpr(s) match {
      case Some(expr) =>
        println(expr)
        println(expr.eval())
      case None =>
    }
  }

  println("--- test 1 ---")
  val tests = List(
    "1 + 2 * 3 + 4 * 5 + 6",
    "1 + (2 * 3) + (4 * (5 + 6))",
    "2 * 3 + (4 * 5)",
    "5 + (8 * 3 + 9 + 3 * 4 * 3)",
    "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))",
    "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
  tests.foreach(testParser(parser1, _))

  println("--- test 2 ---")
  tests.foreach(testParser(parser2, _))

  println("--- part 1 ---")
  val input = Utils.readResource("/day18/input.txt").toList
  val input1 = input.flatMap(parser1.parseExpr)
  println(input1.map(_.eval()).sum)

  println("--- part 2 ---")
  val input2 = input.flatMap(parser2.parseExpr)
  println(input2.map(_.eval()).sum)
}
