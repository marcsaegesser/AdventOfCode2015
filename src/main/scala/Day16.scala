package advent

object Day16 {

  def day16(): Unit = {
    val aunts = readFile(inputFile)
    println(s"Day16.part1 = ${part1(aunts, constraints)}")
    println(s"Day16.part2 = ${part2(aunts, constraints)}")
  }

  def part1(aunts: List[Aunt], constraints: AuntData): Int =
    filterAunts(aunts, constraints).head.id

  def part2(aunts: List[Aunt], constraints: AuntData): Int =
    filterAunts2(aunts, constraints).head.id


  def filterAunts(aunts: List[Aunt], constraints: AuntData): List[Aunt] =
    aunts.filter { case Aunt(id, d) => (d & constraints) == d }

  def filterAunts2(aunts: List[Aunt], constraints: AuntData): List[Aunt] =
    aunts.filter { case Aunt(id, d) => compareData(d, constraints) }

  def compareData(d: AuntData, c: AuntData): Boolean = {
    def compareValues(name: String, value: Int): Boolean =
      name match {
        case n@"cats"        => c.exists { case (f, v) => f == n && value > v }
        case n@"trees"       => c.exists { case (f, v) => f == n && value > v }
        case n@"pomeranians" => c.exists { case (f, v) => f == n && value < v }
        case n@"goldfish"    => c.exists { case (f, v) => f == n && value < v }
        case n               => c.exists { case (f, v) => f == n && value == v }
      }

    d.foldLeft(true) { case (accum, (n, v)) => accum && compareValues(n, v) }
  }

  type AuntData = Set[(String, Int)]
  case class Aunt(id: Int, data: AuntData)

  val auntRegex = """Sue (\d+): (.*)""".r
  val fieldRegex = """(\w+): (\d+)""".r

  def parseAunt(s: String): Aunt = {
    val auntRegex(id, data) = s
    val fieldData =
      data.split(",")
        .map { field =>
          val fieldRegex(f, n) = field.trim
          (f, n.toInt)
        }
        .toSet

    Aunt(id.toInt, fieldData)
  }

  def readFile(f: String): List[Aunt] =
    io.Source.fromFile(f)
      .getLines()
      .map(parseAunt)
      .toList

  val constraints = Set(
    "children" -> 3,
    "cats"-> 7,
    "samoyeds" -> 2,
    "pomeranians" -> 3,
    "akitas" -> 0,
    "vizslas" -> 0,
    "goldfish" -> 5,
    "trees" -> 3,
    "cars" -> 2,
    "perfumes" -> 1
  )

  val inputFile = "data/Day16.txt"
}
