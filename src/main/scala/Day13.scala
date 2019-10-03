package advent

object Day13 {

  def day13(): Unit = {
    val rules = readFile(inputFile)
    println(s"Day13.part1 = ${part1(rules)}")
    println(s"Day13.part2 = ${part2(rules)}")
  }

  def part1(rules: Rules): Int = {
    val diners = rules.keySet.map(_._1).toList
    val tables = diners.permutations

    tables.map(valueOfTable(rules, _)).max
  }

  def part2(rules: Rules): Int = {
    val diners = "self" :: rules.keySet.map(_._1).toList
    val tables = diners.permutations

    tables.map(valueOfTable(rules, _)).max
  }

  def valueOfTable(rules: Rules, table: List[String]): Int = {
    table.zip(LazyList.continually(table).flatten.drop(1))
      .map { case (n, a) =>  rules.getOrElse((n, a), 0) + rules.getOrElse((a, n), 0) }
      .sum
  }

  type Rules = Map[(String, String), Int]

  val ruleRegex = """(\w+) would (\w+) (\d+) happiness units by sitting next to (\w+).""".r

  def parseRule(s: String): ((String, String), Int) =
    s match {
      case ruleRegex(n, "gain", v, a) => ((n, a), v.toInt)
      case ruleRegex(n, "lose", v, a) => ((n, a), -v.toInt)
    }

  def readFile(file: String): Rules =
    io.Source.fromFile(file)
      .getLines()
      .map(parseRule)
      .toMap

  val inputFile = "data/Day13.txt"
}
