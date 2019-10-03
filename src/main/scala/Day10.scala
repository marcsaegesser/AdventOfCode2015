package advent

object Day10 {

  def day10(): Unit = {
    println(s"Day10.part1 = ${part1(puzzleInput)}")
    println(s"Day10.part2 = ${part2(puzzleInput)}")
  }

  def part1(input: String): Int =
    seeAndSayN(40, input).size

  def part2(input: String): Int =
    seeAndSayN(50, input).size

  def seeAndSayN(n: Int, input: String): String =
    if(n == 0) input
    else seeAndSayN(n-1, seeAndSay(input))

  def seeAndSay(input: String): String = {
    findGroups(input)
      .foldLeft(new StringBuilder) { case (sb, s) =>
        sb.append(s"${s.size}${s.head}")
      }.toString
  }

  def findGroups(input: String): List[String] = {
    def helper(accum: List[String], curr: List[Char], s: List[Char]): List[String] =
      s.toList match {
        case Nil                     => (curr.mkString :: accum).reverse
        case h :: t if h == curr.head => helper(accum, h :: curr, t)
        case h :: t                  => helper(curr.mkString :: accum, List(h), t)
      }

    val l = input.toList
    helper(List.empty[String], l.take(1), l.drop(1))
  }

  val puzzleInput = "1321131112"
}
