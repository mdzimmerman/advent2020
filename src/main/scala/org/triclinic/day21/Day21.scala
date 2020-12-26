package org.triclinic.day21

import org.triclinic.Utils

import scala.collection.mutable

case class IngredientList(ingredients: Set[String],
                          allergens: Set[String])

object IngredientList {
  val pattern = """(.+) \(contains (.+)\)""".r

  def apply(s: String): Option[IngredientList] = {
    s match {
      case pattern(x, y) =>
        val ingredients = x.split(" ").toSet
        val allergens = y.split(", ").toSet
        Some(IngredientList(ingredients, allergens))
      case _ =>
        None
    }
  }
}

object Day21 extends App {
  def test1(): Unit = {
    val data = Utils.readString(
      """
        |mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
        |trh fvjkl sbzzf mxmxvkd (contains dairy)
        |sqjhc fvjkl (contains soy)
        |sqjhc mxmxvkd sbzzf (contains fish)
        |""".stripMargin).flatMap(IngredientList(_)).toList
    for (d <- data)
      println(d)
    val allAllergens = data.flatMap(_.allergens).toSet
    println(allAllergens)

    val eliminated: mutable.Map[String, Set[String]] = mutable.Map()
    for (il <- data) {
      val elim = allAllergens.diff(il.allergens)
      for (i <- il.ingredients)
        if (eliminated.contains(i))
          eliminated(i) = eliminated(i).union(elim)
        else
          eliminated(i) = elim
    }
    for ((k, v) <- eliminated) {
      println(s"$k => $v")
    }
    println(eliminated.filter{case(i, a) => a == allAllergens}.keys)
  }

  test1()
}
