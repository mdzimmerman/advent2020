package org.triclinic.day21

import org.triclinic.Utils

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

case class FoodCollection(recipes: List[Food]) {
  val allAllergens: Set[String] = recipes.flatMap(_.allergens).toSet

  val eliminated: Map[String, Set[String]] = {
    val e: mutable.Map[String, Set[String]] = mutable.Map()
    for (r <- recipes) {
      val elim = allAllergens.diff(r.allergens)
      for (i <- r.ingredients)
        if (e.contains(i))
          e(i) = e(i).union(elim)
        else
          e(i) = elim
    }
    e.toMap
  }

  val nonAllergens: Set[String] = {
    eliminated.filter{case(_, a) => a == allAllergens}.keys.toSet
  }

  def countNonAllergensSeen() = {
    recipes.map{r => nonAllergens.intersect(r.ingredients)}
  }
}

object FoodCollection {
  def apply(xs: Seq[String]): FoodCollection =
    FoodCollection(xs.flatMap(Food(_)).toList)

}

object Day21 extends App {
  def test1(): Unit = {
    val data = FoodCollection(Utils.readString(
      """
        |mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
        |trh fvjkl sbzzf mxmxvkd (contains dairy)
        |sqjhc fvjkl (contains soy)
        |sqjhc mxmxvkd sbzzf (contains fish)
        |""".stripMargin))
    println("-- recipes: ")
    for (r <- data.recipes)
      println(r)
    println(s"-- allAllergens: ${data.allAllergens}")
    println(s"-- eliminated:")
    for ((k, v) <- data.eliminated) {
      println(s"$k => $v")
    }
    println(s"-- non-allergen ingredients:")
    println(data.nonAllergens)

    println(s"-- non-allergens seen:")
    println(data.countNonAllergensSeen())
  }

  def part1(data: FoodCollection) = {
    println("### part 1 ###")

  }

  val input =

  test1()
}
