package org.triclinic.day07

import org.triclinic.Utils

import scala.annotation.tailrec
import scala.collection.immutable.{HashMap, Queue}


case class Rule(bag: String,
                contains: Map[String, Int])

object Rule {
  val rulePatt     = """(.*) bags contain (.*).""".r
  val containsPatt = """(\d+) (.*) bag(?:s)?""".r

  def parse(s: String): Option[Rule] = {
    s match {
      case rulePatt(bag, c) =>
        val contains = c.split(", ").flatMap{ _ match {
          case containsPatt(n, name) =>
            Some(name -> n.toInt)
          case _ =>
            None
        }}.toMap
        Some(Rule(bag, contains))
      case _ =>
        None
    }
  }
}

case class RuleSet(rules: Map[String, Rule]) {
  val bags = rules.keys.toList

  def bagTypesInBag(bag: String): Set[String] = {
    @tailrec
    def traverse(q: Queue[String],
                 contains: Set[String]): Set[String] = {
      if (q.isEmpty) {
        contains
      } else {
        val (bag, tail) = q.dequeue
        traverse(tail ++ rules(bag).contains.keys, contains + bag)
      }
    }
    traverse(Queue(bag), Set()) - bag
  }

  def bagsInBag(bag: String): Map[String, Int] = {
    @tailrec
    def traverse(q: Queue[(String, Int)],
                 dist: HashMap[String, Int]): Map[String, Int] = {
      if (q.isEmpty) {
        dist
      } else {
        val ((bag, count), tail) = q.dequeue
        val subbags = rules(bag).contains.map{case(b, c) => b -> c * count}
        val distnew = if (dist.contains(bag))
          dist + (bag -> (dist(bag) + count))
        else
          dist + (bag -> count)
        traverse(tail ++ subbags, distnew)
      }
    }
    traverse(Queue(bag -> 1), HashMap()) - bag
  }

  def bagsContainingBagType(bag: String): List[String] = {
    rules.keys.map(x => x -> bagTypesInBag(x)).filter(_._2.contains(bag)).map(_._1).toList
  }
}

object RuleSet {
  def parseAll(seq: Seq[String]): RuleSet = {
    RuleSet(seq.flatMap(Rule.parse).map(x => x.bag -> x).toMap)
  }

  def merge(left: HashMap[String, Int],
            right: HashMap[String, Int]): HashMap[String, Int] = {
    left.merged(right){ case ((k0, v0), (k1, v1)) => k0 -> (v0 + v1) }
  }
}

object Day7 extends App {
  val test1 = RuleSet.parseAll(Utils.readResource("/day07/test1.txt"))
  for (b <- test1.bags) {
    println(s"$b: ${test1.bagTypesInBag(b)}")
    //println(s" ${test1.bagsInBag(t)}")
  }
  println(s"bag types with shiny gold = ${test1.bagsContainingBagType("shiny gold")}")
  println()
  for (b <- test1.bags) {
    val dist = test1.bagsInBag(b)
    println(s"$b: $dist, ${dist.values.sum}")
  }

  val input = RuleSet.parseAll(Utils.readResource("/day07/input.txt"))
  println(input.bagsContainingBagType("shiny gold").size)
}
