package org.triclinic.day21

import org.triclinic.Utils

import scala.annotation.tailrec
import scala.collection.mutable

case class Food(ingredients: Set[String],
                allergens: Set[String])

object Food {
  val pattern = """(.+) \(contains (.+)\)""".r

  def apply(s: String): Option[Food] = {
    s match {
      case pattern(x, y) =>
        val ingredients = x.split(" ").toSet
        val allergens = y.split(", ").toSet
        Some(Food(ingredients, allergens))
      case _ =>
        None
    }
  }
}

case class FoodCollection(foods: List[Food]) {
  val allAllergens: Set[String] = foods.flatMap(_.allergens).toSet

  val potAllergens: Map[String, Set[String]] = {
    allAllergens.map { a =>
      val pot = foods
        .filter(_.allergens.contains(a))
        .map(_.ingredients)
        .reduceLeft(_.intersect(_))
      a -> pot
    }.toMap
  }

  def countNonAllergens = {
    val allergenIngr = potAllergens.values.reduceLeft(_.union(_))
    foods.map(_.ingredients.diff(allergenIngr).size).sum
  }

  @tailrec
  private def solve(pot: Map[String, Set[String]],
            out: Map[String, String]): Map[String, String] = {
    val size1 = pot.filter{ case(_, is) => is.size == 1 }
    if (size1.isEmpty) {
      out
    } else {
      val curr = size1.head
      val currAlln = curr._1
      val currIngr = curr._2.head
      val potNew = (pot - currAlln).map {
        case(a, is) => a -> (is - currIngr)
      }
      val outNew = out + (currAlln -> currIngr)
      solve(potNew, outNew)
    }
  }

  def solve(): Map[String, String] = solve(potAllergens, Map())
}

object FoodCollection {
  def apply(xs: Seq[String]): FoodCollection =
    FoodCollection(xs.flatMap(Food(_)).toList)

}

object Day21 extends App {
  def test1(data: FoodCollection): Unit = {
    println("=== test 1 ===")

    println("-- recipes: ")
    for (r <- data.foods)
      println(r)

    println(s"-- non-allergens seen:")
    println(data.countNonAllergens)
  }

  def part1(data: FoodCollection) = {
    println("=== part 1 ===")
    println(data.countNonAllergens)

  }

  def part2(data: FoodCollection) = {
    println("=== part 2 ===")
    for ((a, is) <- data.potAllergens) {
      println(s"$a -> $is")
    }
    println()
    for ((a, i) <- data.solve) {
      println(s"$a -> $i")
    }
    println()
    val out = data.solve.toList.sorted.map(_._2).mkString(",")
    println(out)
  }

  val data = FoodCollection(Utils.readString(
    """
      |mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
      |trh fvjkl sbzzf mxmxvkd (contains dairy)
      |sqjhc fvjkl (contains soy)
      |sqjhc mxmxvkd sbzzf (contains fish)
      |""".stripMargin))

  val input = FoodCollection(Utils.readResource("/day21/input.txt"))

  test1(data)
  part1(input)
  part2(input)
}
