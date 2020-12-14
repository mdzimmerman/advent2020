package org.triclinic.day10

import org.triclinic.Utils

import scala.::
import scala.annotation.tailrec

object Day10 extends App {
  val test1 = Utils.readString(
    """
      |16
      |10
      |15
      |5
      |1
      |11
      |7
      |19
      |6
      |12
      |4
      |""".stripMargin).filter(_.nonEmpty).map(_.toInt).toList

  val test2 = Utils.readString(
    """
      |28
      |33
      |18
      |42
      |31
      |14
      |46
      |20
      |48
      |47
      |24
      |23
      |49
      |45
      |19
      |38
      |39
      |11
      |1
      |32
      |25
      |35
      |8
      |17
      |7
      |9
      |4
      |2
      |34
      |10
      |3
      |""".stripMargin).filter(_.nonEmpty).map(_.toInt).toList

  val input = Utils.readResource("/day10/input.txt").filter(_.nonEmpty).map(_.toInt).toList

  case class Output(ones: Int, threes: Int, out: Int)

  def calcDiffs(adapters: List[Int]) = {
    val elems = (0 :: adapters).sorted.toVector
    val diffs = elems.sliding(2).map(x => x(1) - x(0)).toVector :+ 3
    //for ((e, d) <- elems.zip(diffs))
    //  println(f"$e%3d $d")
    diffs
  }

  def diffDists(adapters: List[Int]) = {
    val diffs = calcDiffs(adapters)
    val dist = diffs.groupBy(identity).mapValues(_.size)
    val ones = dist(1)
    val threes = dist(3)+1
    Output(ones, threes, ones*threes)
  }

  def countPerms(adapters: List[Int]): Long = {
    val diffs = calcDiffs(adapters)
    split13(diffs).map(_.length).filter(_ > 0).map(combo123).map(_.toLong).product
  }

  def split13(xs: Seq[Int]): List[List[Int]] = split13(xs, Nil, Nil)

  @tailrec
  def split13(xs: Seq[Int],
              curr: List[Int],
              groups: List[List[Int]]): List[List[Int]] = {
    if (xs.isEmpty) {
      (curr :: groups).reverse
    } else {
      val (currNew, groupsNew) = xs.head match {
        case 1 => (1 :: curr, groups)
        case 3 => (Nil, curr :: groups)
        case _ => throw new Exception("bad gap")
      }
      split13(xs.tail, currNew, groupsNew)
    }
  }

  def combo123(n: Int): Int = {
    (1 to n-1).flatMap{ i =>
      List.fill(i)(1 to 3).flatten.combinations(i).toList
    }.filter(_.sum == n).flatMap(_.permutations).size+1
  }

  println("--- test 2 ---")
  println(diffDists(test2))
  println(countPerms(test2))

  println("--- input ---")
  println(diffDists(input))
  println(countPerms(input))
}

