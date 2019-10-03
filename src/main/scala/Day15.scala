package advent

import math.max

object Day15 {
  type Name = String
  case class Ingredient(name: Name, capacity: Int, durabilitiy: Int, flavor: Int, texture: Int, calories: Int)
  type Ingredients = Vector[Ingredient]


  def day15(): Unit = {
    val ingredients = readFile(inputFile)
    println(s"Day15.part1 = ${part1(ingredients)}")
    println(s"Day15.part2 = ${part2(ingredients)}")
  }

  def part1(ingredients: Ingredients): Int = {
    sumIter.foldLeft(0) { case (maxScore, v) =>
      val score = scoreRecipe(v, ingredients)
      if(score > maxScore) score
      else                 maxScore
    }
  }

  def part2(ingredients: Ingredients): Int = {
    sumIter
      .filter(totalCalories(_, ingredients) == 500)
      .foldLeft(0) { case (maxScore, v) =>
      val score = scoreRecipe(v, ingredients)
      if(score > maxScore) score
      else                 maxScore
    }
  }

  def sumIter =
    (List.fill(100)('1') ++ List.fill(3)('+'))
      .mkString.toSeq
      .permutations
      .map(_.unwrap)
      .map(_.split('+').map(_.size).toVector.padTo(4, 0))

  def scoreRecipe(recipe: Vector[Int], ingredients: Ingredients): Int = {
    val (sumC, sumD, sumF, sumT) =
      (recipe zip ingredients).foldLeft((0, 0, 0, 0)) {
        case ((ac, ad, af, at), (r, Ingredient(_, c, d, f, t, _))) =>
          (ac + r*c, ad + r*d, af + r*f, at + r*t)
      }

    max(sumC, 0) * max(sumD, 0) * max(sumF, 0) * max(sumT, 0)
  }

  def totalCalories(recipe: Vector[Int], ingredients: Ingredients): Int =
    (recipe zip ingredients).foldLeft(0) { case (accum, (r, Ingredient(_, _, _, _, _, c))) =>
      accum + r*c
    }

  val IngredientRegex = """(\w+):\s*capacity\s*(.*),\s*durability(.*),\s*flavor\s(.*),\s*texture\s*(.*),\s*calories\s*(.*)""".r

  def parseIngredient(line: String): Ingredient = {
    val IngredientRegex(n, c, d, f, t, cc) = line
    Ingredient(n, c.trim.toInt, d.trim.toInt, f.trim.toInt, t.trim.toInt, cc.trim.toInt)
  }

  def readFile(file: String): Ingredients =
    io.Source.fromFile(file)
      .getLines()
      .map(parseIngredient)
      .toVector

  val inputFile = "data/Day15.txt"
}

