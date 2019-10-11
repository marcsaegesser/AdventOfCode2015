package advent

object Day24 {

  def day24(): Unit = {
    val input = readFile(puzzleInput)
    println(s"Day24.part1 = ${part1(input)}")
    println(s"Day24.part2 = ${part2(input)}")
  }

  def part1(input: List[Long]): Long =
    findGroups(input, 3).head._1._2

  def part2(input: List[Long]): Long =
    findGroups(input, 4).head._1._2

  def findGroups(input: List[Long], groups: Int): List[((Int, Long), List[Long])] =
    input.sorted.reverse.tails
      .flatMap(subsetsSumTo(_, input.sum/groups))
      .distinct
      .map(g => (scoreGroup(g), g))
      .toList
      .sortBy(_._1)

 def subsetsSumTo(input: List[Long], sumTo: Long): List[List[Long]] = {
    def helper(found: List[List[Long]], accum: List[Long], rest: List[Long]): List[List[Long]] = {
      val sum = accum.sum
      rest match {
        case Nil                         => found // No matches
        case h :: Nil if h + sum == sumTo => (h +: accum).reverse +: found
        case h :: t if h + sum > sumTo   => helper(found, accum, t)
        case h :: t if h + sum == sumTo   => helper((h +: accum).reverse +: found, accum, t)
        case h :: t                      => helper(found, h +: accum, t) ++ helper(found, accum, t)
      }
    }

    helper(List(), List(), input)
  }

  def scoreGroup(group: List[Long]): (Int, Long) =
    (group.size, group.product)

  def readFile(f: String): List[Long] =
    io.Source.fromFile(f)
      .getLines()
      .map(_.toLong)
      .toList

  val puzzleInput = "data/Day24.txt"
}
