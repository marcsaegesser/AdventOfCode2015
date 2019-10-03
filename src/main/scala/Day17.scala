package advent

object Day17 {

  def day17(): Unit = {
    val input = readFile(inputFile)
    println(s"Day17.part1 = ${part1(input, 150)}")
    println(s"Day17.part2 = ${part2(input, 150)}")
  }

  def part1(input: Set[(Int, Int)], capacity: Int): Int =
    findFills(input, capacity).size

  def part2(input: Set[(Int, Int)], capacity: Int): Int = {
    val fs =
      findFills(input, capacity)
        .map(_.size)
        .sorted

    fs.segmentLength(_ == fs.head)
  }

  def findFills(input: Set[(Int, Int)], capacity: Int): List[Set[(Int, Int)]] = {
    def helper(s: Set[(Int, Int)]): Int =
      s.foldLeft(0) { case (a, (c, _)) => a + c }

    input.subsets.filter(helper(_) == capacity).toList
  }

  def readFile(f: String): Set[(Int, Int)] =
    io.Source.fromFile(f)
      .getLines()
      .map(_.toInt)
      .zipWithIndex
      .toSet

  val inputFile = "data/Day17.txt"
}
