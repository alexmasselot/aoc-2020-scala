package day21

import utils.{AOCExecutor, InputLoader}

case class Food(ingredients: Set[String], allergens: Set[String]) {
  val countIngredients = ingredients.size
  val countAllergens = allergens.size
}

case class FoodList(foods: List[Food]) {

  def allergens = foods.foldLeft(Set[String]()) { case (acc, f) => acc ++ f.allergens }

  def countAllergens = allergens.size

  def ingredients = foods.foldLeft(Set[String]()) { case (acc, f) => acc ++ f.ingredients }

  def countIngredients = ingredients.size

  def containsAllergen(allergen: String) = foods.filter {
    _.allergens.contains(allergen)
  }

  def containsIngredient(ingredient: String) = foods.filter {
    _.ingredients.contains(ingredient)
  }

  def inertIngredients = allergens.foldLeft(ingredients) { case (acc, allergen) =>
    val foodWithAllergen = containsAllergen(allergen)
    val ingNotInAllergen = foodWithAllergen.foldLeft(
      ingredients.map {
        _ -> 0
      }.toMap
    ) { case (acc, food) =>
      food.ingredients.foldLeft(acc) { case (acc2, ing) => acc2 + (ing -> (acc2(ing) + 1)) }
    }
      .filter { case (_, count) => count != foodWithAllergen.size }
      .map {
        _._1
      }
      .toSet
    acc.intersect(ingNotInAllergen)
  }


  def removeIngredients(ingredients: Set[String]) =
    FoodList(foods.map { food => Food(food.ingredients -- ingredients, food.allergens) })


  def removeAllergen(allergen: String) =
    FoodList(foods.map { food => food.copy(allergens = food.allergens - allergen) }.filter {
      !_.allergens.isEmpty
    })


  override def toString: String = {
    1 to foods.map { f => f.allergens.size }.max
  }.flatMap { i =>
    foods
      .filter {
        _.countAllergens == i
      }
      .sortBy {
        _.allergens.toList.sorted.mkString(" ")
      }
      .map { f => s"${f.allergens.toList.sorted.mkString(" ")}: ${f.ingredients.toList.sorted}" }
  }.mkString("\n")
}

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object AllergenAssessment extends AOCExecutor {

  val reInput = raw"""(.*) \(contains (.*)\)""".r
  val foodList = FoodList(
    InputLoader.read("21", false)
      .map {
        _ match {
          case reInput(ings, alls) => Food(ings.split(" ").toSet, alls.split(", ").toSet)
        }
      }
  )

  def part1(): Unit = {
    val n = foodList.inertIngredients.toList.map { ing =>
      foodList.containsIngredient(ing).size
    }.sum
    println(s"n=$n")
  }

  def part2(): Unit = {
    val foodListWithoutInert = foodList.removeIngredients(foodList.inertIngredients)

    def handler(foodlist: FoodList): List[(String, String)] = {
      if(foodlist.foods.isEmpty){
        return List()
      }
      val allergenToIngredient = foodlist.allergens.toList.map { allergen =>
        allergen -> foodlist.foods.filter {
          _.allergens.contains(allergen)
        }
          .foldLeft(foodList.ingredients) { case (acc, f) => acc.intersect(f.ingredients) }
      }.filter {
        _._2.size == 1
      }
        .map { case (a, is) => a -> is.head }

      if (allergenToIngredient.size == 0) {
        throw new UnsupportedOperationException(s"$foodlist")
      }
      val (all, ing) = allergenToIngredient.head
      handler(foodlist.removeIngredients(Set(ing)).removeAllergen(all)) :+ (all -> ing)
    }
    val allergenToIngredient = handler(foodListWithoutInert)

    println(allergenToIngredient.sortBy{_._1}.map{_._2}.mkString(","))
  }

  def main(args: Array[String]) = {
    execute
  }
}
