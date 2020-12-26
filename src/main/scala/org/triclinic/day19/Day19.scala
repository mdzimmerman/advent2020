package org.triclinic.day19

import org.triclinic.{AsInt, Utils}

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

sealed trait Rule {
  val n: Int
}

case class RuleLiteral(n: Int,
                       c: String) extends Rule

case class RuleCombo(n: Int,
                     elem: List[List[Int]]) extends Rule

object Rule {
  private val pattLiteral = """(\d+): \"(.)\"""".r
  private val pattCombo   = """(\d+): ([|\d ]+)""".r

  def apply(s: String): Option[Rule] = {
    s match {
      case pattLiteral(AsInt(n), s) =>
        Some(RuleLiteral(n, s))
      case pattCombo(AsInt(n), xs) =>
        val elem = xs.split(""" \| """).toList.map(_.split(" ").map(_.toInt).toList)
        Some(RuleCombo(n, elem))
      case _ =>
        None
    }
  }
}

case class RuleSet(rules: Map[Int, Rule],
                   input: List[String]) {
  //val regex0: Regex = regex(0)

  def regexstr(n: Int): String = {
    rules(n) match {
      case RuleLiteral(_, c) =>
        c
      case RuleCombo(_, elem) =>
        "(?:" + elem.map(
          _.map(regexstr).mkString("")
        ).mkString("|") + ")"
    }
  }

  def regex(n: Int): String = ("^"+regexstr(n)+"$")

  def regexstr2(n: Int): String = {
    rules(n) match {
      case RuleLiteral(_, c) =>
        c
      case RuleCombo(8, _) =>
        "(?:" + regexstr2(42) + ")+"
      case RuleCombo(11, _) =>
        val r42 = regexstr2(42)
        val r31 = regexstr2(31)
        "(?:" + (1 to 11).map(i => s"$r42{$i}$r31{$i}").mkString("|") + ")"
      case RuleCombo(_, elem) =>
        "(?:" + elem.map(
          _.map(regexstr2).mkString("")
        ).mkString("|") + ")"
    }
  }

  def regex2(n: Int): String = "^"+regexstr2(n)+"$"

  def matches(): List[String] = {
    input.filter(_.matches(regex(0)))
  }

  def matches2(): List[String] = {
    input.filter(_.matches(regex2(0)))
  }
}

object RuleSet {
  def apply(xs: List[String]): RuleSet = {
    val (rules, input) = xs.splitAt(xs.indexOf(""))
    RuleSet(
      rules.flatMap(Rule(_)).map(r => r.n -> r).toMap,
      input.filter(_.nonEmpty))
  }
}

object Day19 extends App {
  val test1 = RuleSet(Utils.readString(
    """|0: 1 2
       |1: "a"
       |2: 1 3 | 3 1
       |3: "b"
       |
       |""".stripMargin).toList)
  for (r <- test1.rules)
    println(r)
  println(test1.regex(0))

  val test2 = RuleSet(Utils.readString(
    """|0: 4 1 5
       |1: 2 3 | 3 2
       |2: 4 4 | 5 5
       |3: 4 5 | 5 4
       |4: "a"
       |5: "b"
       |
       |ababbb
       |bababa
       |abbbab
       |aaabbb
       |aaaabbb
       |""".stripMargin).toList)
  println(test2.regex(0))
  println(test2.matches)

  val test3 = RuleSet(Utils.readString(
    """|42: 9 14 | 10 1
       |9: 14 27 | 1 26
       |10: 23 14 | 28 1
       |1: "a"
       |11: 42 31
       |5: 1 14 | 15 1
       |19: 14 1 | 14 14
       |12: 24 14 | 19 1
       |16: 15 1 | 14 14
       |31: 14 17 | 1 13
       |6: 14 14 | 1 14
       |2: 1 24 | 14 4
       |0: 8 11
       |13: 14 3 | 1 12
       |15: 1 | 14
       |17: 14 2 | 1 7
       |23: 25 1 | 22 14
       |28: 16 1
       |4: 1 1
       |20: 14 14 | 1 15
       |3: 5 14 | 16 1
       |27: 1 6 | 14 18
       |14: "b"
       |21: 14 1 | 1 14
       |25: 1 1 | 1 14
       |22: 14 14
       |8: 42
       |26: 14 22 | 1 20
       |18: 15 15
       |7: 14 5 | 1 21
       |24: 14 1
       |
       |abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
       |bbabbbbaabaabba
       |babbbbaabbbbbabbbbbbaabaaabaaa
       |aaabbbbbbaaaabaababaabababbabaaabbababababaaa
       |bbbbbbbaaaabbbbaaabbabaaa
       |bbbababbbbaaaaaaaabbababaaababaabab
       |ababaaaaaabaaab
       |ababaaaaabbbaba
       |baabbaaaabbaaaababbaababb
       |abbbbabbbbaaaababbbbbbaaaababb
       |aaaaabbaabaaaaababaa
       |aaaabbaaaabbaaa
       |aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
       |babaaabbbaaabaababbaabababaaab
       |aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba
       |""".stripMargin).toList)
  //println(test3)
  //println(test3.regex(0))
  //println(test3.regex2(0))
  println(test3.matches.size)
  println(test3.matches2.size)

  val input = RuleSet(Utils.readResource("/day19/input.txt").toList)
  println(input.matches.size)
  println(input.matches2.size)
}
