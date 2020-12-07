package org.triclinic.day06

import org.triclinic.Utils

import scala.annotation.tailrec
import scala.collection.mutable

case class Group(responses: List[String]) {
  val hist: Map[Char, Int] = {
    val dist = mutable.Map[Char, Int]()
    for (r <- responses; c <- r) {
      if (dist.contains(c))
        dist(c) += 1
      else
        dist += (c -> 1)
    }
    dist.toMap
  }

  val histAll: Map[Char, Int] =
    hist.filter{case(c, v) => v == responses.size}
}

object Group {
  def parse(input: Seq[String]): List[Group] = {
    parse(input, List(), List()).map(Group(_))
  }

  @tailrec
  def parse(input: Seq[String],
            curr: List[String],
            out: List[List[String]]): List[List[String]] = {
    if (input.isEmpty) {
      (curr :: out).reverse
    } else {
      val (currNew, outNew) = input.head match {
        case "" =>
          (List(), curr.reverse :: out)
        case x =>
          (x :: curr, out)
      }
      parse(input.tail, currNew, outNew)
    }
  }
}

object Day6 extends App {
  println("--- test ---")
  val test1 = Group.parse(Utils.readResource("/day06/test1.txt"))
  for (t <- test1) {
    println(s"$t")
    println(s"  ${t.hist}, ${t.hist.size}")
    println(s"  ${t.histAll}, ${t.histAll.size}")
  }
  println(test1.map(_.hist.size).sum)
  println(test1.map(_.histAll.size).sum)
  
  println()
  println("--- input ---")
  val input = Group.parse(Utils.readResource("/day06/input.txt"))
  println(input.map(_.hist.size).sum)
  println(input.map(_.histAll.size).sum)
}
