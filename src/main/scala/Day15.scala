package advent

import math.max

object Day15 {
  type Name = String
  case class Ingredient(name: Name, capacity: Int, durabilitiy: Int, flavor: Int, texture: Int, calories: Int)
  type Ingredients = Map[Name, Ingredient]
  type Recipe = Map[Name, Int]

  val IngredientRegex = """(\w+):\s*capacity\s*(.*),\s*durability(.*),\s*flavor\s(.*),\s*texture\s*(.*),\s*calories\s*(.*)""".r

  def solvePuzzle(ingredients: Ingredients): Int = {
    def next(rs: List[Recipe], n: Int): List[Recipe] = {
      if(n == 100) rs
      else {
        val nextRs = rs.flatMap(r => nextRecipes(r)).map(r => (r, scoreRecipe(r, ingredients)))
        val (_, mx) = nextRs.maxBy(_._2)
        next(nextRs.collect { case (r, s) if s == mx => r }, n+1)
      }
    }

    val rs = next(List(emptyRecipe(ingredients)), 0)
    scoreRecipe(rs.head, ingredients)
  }

  def findFactors(ingredients: Ingredients): List[Recipe] = {
    def next(rs: List[Recipe], n: Int): List[Recipe] = {
      if(n == 100) rs.filter(r => countCalories(r, ingredients) == 500)
      else {
        val nextRs = rs.flatMap(r => nextRecipes(r)).collect { case r if countCalories(r, ingredients) <= 500 => r }
        next(nextRs, n+1)
      }
    }

    next(List(emptyRecipe(ingredients)), 0)
  }

  // def solvePuzzle2(ingredients: Ingredients): Int = {
  //   def next(rs: List[Recipe], n: Int): List[Recipe] = {
  //     if(n == 100) rs.filter(r => countCalories(r, ingredients) == 500)
  //     else {
  //       val nextRs = rs.flatMap(r => nextRecipes(r)).map(r => (r, countCalories(r, ingredients)))
  //       next(nextRs.collect { case (r, c) if c <= 500 => r }, n+1)
  //     }
  //   }

  //   val rs = next(List(emptyRecipe(ingredients)), 0)
  //   rs.map(r => scoreRecipe(r, ingredients)).max
  // }

  // def solvePuzzle2(ingredients: Ingredients, maxCalories: Int): Int = {
  //   def next(accum: List[Recipe], rs: List[Recipe]): List[Recipe] = {
  //     val nextRs = rs.flatMap(r => nextRecipes(r)).map(r => (r, scoreRecipe(r, ingredients), countCalories(r, ingredients)))
  //     val (_, maxScore, _) = nextRs.maxBy(_._2)
  //     val (_, _, minCalories) = nextRs.minBy(_._3)
  //     if(minCalories > maxCalories) accum
  //     else
  //       next(
  //         accum ++ nextRs.collect { case (r, s, c) if c == maxCalories => r },
  //         nextRs.collect { case (r, s, _) if s == maxScore => r }
  //       )
  //   }

  //   val rs = next(List.empty[Recipe], List(emptyRecipe(ingredients)))
  //   rs.map(r => scoreRecipe(r, ingredients)).max
  // }

 def scoreRecipe(recipe: Recipe, ingredients: Ingredients): Int = {
    val (sumC, sumD, sumF, sumT) =
      recipe.foldLeft((0, 0, 0, 0)) { case ((c, d, f, t), (i, n)) =>
        ingredients.get(i)
          .map { case Ingredient(_, ic, id, iff, it, _) => (c + ic*n, d + id*n, f + iff*n, t + it*n) }
        .getOrElse(throw new Exception(s"Unknown ingredient $i"))
    }

    max(sumC, 0) * max(sumD, 0) * max(sumF, 0) * max(sumT, 0)
  }

  def emptyRecipe(ingredients: Ingredients): Recipe =
    ingredients.map { case (k, _) => (k, 0) }

  def countIngredients(r: Recipe): Int =
    r.values.sum

  def countCalories(r: Recipe, ingredients: Ingredients): Int =
    r.keys.map(n => ingredients(n).calories).sum

  def nextRecipes(recipe: Recipe): List[Recipe] = {
    recipe.keys.toList.map { name =>
      recipe.updated(name, recipe(name)+1)
    }
  }

  def parseIngredient(line: String): Ingredient = {
    val IngredientRegex(n, c, d, f, t, cc) = line
    Ingredient(n, c.trim.toInt, d.trim.toInt, f.trim.toInt, t.trim.toInt, cc.trim.toInt)
  }

  def readFile(file: String): Ingredients =
    io.Source.fromFile(file)
      .getLines()
      .map(parseIngredient)
      .map(i => (i.name, i))
      .toMap
}
